
use std::vec;

use swc_core::{atoms::Atom, common::{Span, Spanned, SyntaxContext, DUMMY_SP}, ecma::ast::{op, ArrayLit, AssignExpr, AssignOp, AssignTarget, AssignTargetPat, BinExpr, BinaryOp, BindingIdent, BlockStmt, CallExpr, Callee, ComputedPropName, CondExpr, Decl, Expr, ExprOrSpread, ExprStmt, FnExpr, Function, Ident, IdentName, KeyValueProp, MemberExpr, MemberProp, ObjectLit, ObjectPatProp, ParenExpr, Pat, Prop, PropName, PropOrSpread, SeqExpr, SimpleAssignTarget, SpreadElement, Stmt, UnaryExpr, UpdateOp, VarDecl, VarDeclKind, VarDeclarator}};

use crate::util::{self, find_declared, member_root};

fn immutize_member_op(expr: &MemberExpr, value: Expr) -> Option<Expr> {
  match &*expr.obj {
    Expr::Member(member_expr) => {
      if let MemberProp::PrivateName(_) = &expr.prop {
        None
      } else {
        immutize_member_op(member_expr, Expr::Object(ObjectLit {
          props: vec![
            PropOrSpread::Spread(SpreadElement {
              dot3_token: DUMMY_SP,
              expr: Box::new(Expr::Member(member_expr.clone()))
            }),
            PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
              key: match &expr.prop {
                MemberProp::Ident(ident) => PropName::Ident(ident.clone()),
                MemberProp::Computed(name) => PropName::Computed(name.clone()),
                _ => todo!(),
              },
              value: Box::new(value),
            }))),
          ],
          span: DUMMY_SP,
        }))
      }
    }
    Expr::Ident(root) => {
      match &expr.prop {
        MemberProp::Ident(_) | MemberProp::Computed(_) => {
          Some(Expr::Object(ObjectLit {
            props: vec![
              PropOrSpread::Spread(SpreadElement {
                dot3_token: DUMMY_SP,
                expr: Box::new(Expr::Ident(root.clone()))
              }),
              PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                key: match &expr.prop {
                  MemberProp::Ident(ident) => PropName::Ident(ident.clone()),
                  MemberProp::Computed(name) => PropName::Computed(name.clone()),
                  _ => todo!(),
                },
                value: Box::new(value),
              }))),
            ],
            span: DUMMY_SP,
          }))
        }
        MemberProp::PrivateName(_) => None
      }
    }
    _ => None
  }
}

fn set_ident(ident: &Ident) -> Expr {
  Expr::Call(CallExpr {
      callee: Callee::Expr(
          Box::new(Expr::Ident(
              Ident::new(format!("set{}", util::capitalize_first(&ident.sym)).into(),  DUMMY_SP, SyntaxContext::empty())))),
      args: vec![
          ExprOrSpread {
              spread: None,
              expr: Box::new(Expr::Ident(Ident::new(ident.sym.clone(), DUMMY_SP, ident.ctxt)))
          }],
      type_args: None,
      span: DUMMY_SP,
      ctxt: SyntaxContext::empty(),
  })
}

/*
  a.b.c = x  =>  (a = {...a, b: {...a.b, c: x}}}, setA(a), a)
  a[b].c = x  =>  (a = {...a, [b]: {...a[b], c: x}}}, setA(a), a)

  - {... a.b, c: x}
  - {... a, b: {... a.b, c: x}}
*/
pub fn immutize_member_assignment(expr: &MemberExpr, value: &Expr) -> Option<Expr> {
  if let Some(tranformed) = immutize_member_op(&expr, value.clone()) {
    let ident= member_root(expr).unwrap();
    return Some(Expr::Seq(SeqExpr {
        exprs: vec![
            Box::new(Expr::Assign(AssignExpr {
              left: ident.clone().into(),
              right: Box::new(tranformed),
              span: DUMMY_SP,
              op: AssignOp::Assign,
            })),
            Box::new(set_ident(&ident)),
            Box::new(Expr::Member(expr.clone())),
        ],
        span: DUMMY_SP,
    }))
  }
  None
}

pub fn list_destruct_vars(pat: &AssignTargetPat) -> Vec<String> {
  let mut names = Vec::new();
  match pat {
    AssignTargetPat::Array(array_pat) => {
      for opat in &array_pat.elems {
        if let Some(pat) = opat {
          for name in find_declared(&pat) {
            names.push(name.to_string());
          }
        }
      }
    }
    AssignTargetPat::Object(object_pat) => {
      for prop in &object_pat.props {
        match prop {
            ObjectPatProp::KeyValue(kv_pat) => {
              for name in find_declared(&kv_pat.value) {
                names.push(name.to_string());
              }
            },
            ObjectPatProp::Assign(_) => {},
            ObjectPatProp::Rest(_) => {}
        }
      }
    }
    _ => {}
  }
  names
}

pub fn immutize_destruct() {}

/*
  ++foo  =>  foo, setFoo(foo), foo
  foo++  =>  foo++, setFoo(foo), foo - 1
 */
pub fn immutize_update(expr: &Expr, ident: &Ident, update_op: &UpdateOp, prefix: bool) -> Option<Expr> {
  Some(Expr::Seq(SeqExpr {
      exprs: vec![
          Box::new(expr.clone()),
          Box::new(Expr::Call(CallExpr {
              callee: Callee::Expr(
                  Box::new(Expr::Ident(
                      Ident::new(format!("set{}", util::capitalize_first(&ident.sym)).into(), DUMMY_SP, SyntaxContext::empty())))),
              args: vec![
                  ExprOrSpread {
                      spread: None,
                      expr: Box::new(Expr::Ident(Ident::new(ident.sym.clone(), DUMMY_SP, ident.ctxt)))
                  }],
              type_args: None,
              span: DUMMY_SP,
              ctxt: SyntaxContext::empty(),
          })),
          if prefix {
              Box::new(Expr::Ident(Ident::new(ident.sym.clone(), DUMMY_SP, ident.ctxt)))
          } else {
              match update_op {
                  UpdateOp::PlusPlus => {
                      Box::new(Expr::Bin(BinExpr {
                          span: DUMMY_SP,
                          op: op!(bin, "-"),
                          left: Box::new(Expr::Ident(Ident::new(ident.sym.clone(), DUMMY_SP, ident.ctxt))),
                          right: 1.into()
                      }))
                  }
                  UpdateOp::MinusMinus => {
                      Box::new(Expr::Bin(BinExpr {
                          span: DUMMY_SP,
                          op: op!(bin, "+"),
                          left: Box::new(Expr::Ident(Ident::new(ident.sym.clone(), DUMMY_SP, ident.ctxt))),
                          right: 1.into()
                      }))
                  }
              }
          }
      ],
      span: expr.span(),
  }))
}

pub fn immutize_member_update(expr: &MemberExpr, update_op: &UpdateOp, prefix: bool) -> Option<Expr> {
  let value = match (update_op, prefix) {
    (UpdateOp::PlusPlus, true) => Expr::Bin(BinExpr { span: DUMMY_SP, op: op!(bin, "+"), left: Box::new(Expr::Member(expr.clone())), right: 1.into() }),
    (UpdateOp::PlusPlus, false) => Expr::Bin(BinExpr { span: DUMMY_SP, op: op!(bin, "+"), left: Box::new(Expr::Member(expr.clone())), right: 1.into() }),
    (UpdateOp::MinusMinus, true) => Expr::Bin(BinExpr { span: DUMMY_SP, op: op!(bin, "-"), left: Box::new(Expr::Member(expr.clone())), right: 1.into() }),
    (UpdateOp::MinusMinus, false) => Expr::Bin(BinExpr { span: DUMMY_SP, op: op!(bin, "-"), left: Box::new(Expr::Member(expr.clone())), right: 1.into() }),
  };
  if let Some(tranformed) = immutize_member_op(&expr, value.clone()) {
    let ident= member_root(expr).unwrap();
    return Some(Expr::Seq(SeqExpr {
        exprs: vec![
            Box::new(Expr::Assign(AssignExpr {
              left: ident.clone().into(),
              right: Box::new(tranformed),
              span: DUMMY_SP,
              op: AssignOp::Assign,
            })),
            Box::new(Expr::Call(CallExpr {
                callee: Callee::Expr(
                    Box::new(Expr::Ident(
                        Ident::new(format!("set{}", util::capitalize_first(&ident.sym)).into(),  DUMMY_SP, SyntaxContext::empty())))),
                args: vec![
                    ExprOrSpread {
                        spread: None,
                        expr: Box::new(Expr::Ident(Ident::new(ident.sym.clone(), DUMMY_SP, ident.ctxt)))
                    }],
                type_args: None,
                span: DUMMY_SP,
                ctxt: SyntaxContext::empty(),
            })),
            if prefix {
                Box::new(Expr::Member(expr.clone()))
            } else {
                match update_op {
                    UpdateOp::PlusPlus => {
                        Box::new(Expr::Bin(BinExpr {
                            span: DUMMY_SP,
                            op: op!(bin, "-"),
                            left: Box::new(Expr::Member(expr.clone())),
                            right: 1.into()
                        }))
                    }
                    UpdateOp::MinusMinus => {
                        Box::new(Expr::Bin(BinExpr {
                            span: DUMMY_SP,
                            op: op!(bin, "+"),
                            left: Box::new(Expr::Member(expr.clone())),
                            right: 1.into()
                        }))
                    }
                }
            }
        ],
        span: DUMMY_SP,
    }))
  }
  None
}

fn is_array_guard(cond: &Expr, cons: &Expr, alt: &Expr) -> Expr {
  Expr::Cond(CondExpr {
    span: DUMMY_SP,
    test: Box::new(Expr::Call(CallExpr {
      span: DUMMY_SP,
      ctxt: SyntaxContext::empty(),
      callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
        span: DUMMY_SP,
        obj: Box::new(Expr::Ident(Ident { span: DUMMY_SP, ctxt: SyntaxContext::empty(), sym: "Array".into(), optional: false })),
        prop: MemberProp::Ident(IdentName { span: DUMMY_SP, sym: "isArray".into() })}))),
      args: vec![ExprOrSpread {spread: None, expr: Box::new(cond.clone()) } ],
      type_args: None
    })),
    cons: Box::new(cons.clone()),
    alt: Box::new(alt.clone()),
  })
}

/*
  
  todo: warn if obj.push() not inside expression statement

  obj.push(item1, item2)  =>  Array.isArray(obj) ? (obj = [... obj, item1, item2], setObj(obj), obj[obj.length - 1]) : obj.push(item1, item2)
  obj.prop.push(expr)  =>  Array.isArray(obj.prop) ? obj = {... obj, prop: [... obj.prop, expr]}, setObj(obj), expr : obj.prop.push(expr)
*/
fn immutize_push(expr: &CallExpr) -> Option<Expr> {
  let member_expr = expr.callee.as_expr().unwrap().as_member().unwrap();
  match &*member_expr.obj {
    Expr::Ident(ident) => {
      let mut elems = vec![
        Some(ExprOrSpread {
          spread: Some(DUMMY_SP),
          expr: Box::new(Expr::Ident(ident.clone())),
        }),
      ];
      elems.extend(expr.args.clone().into_iter().map(|x| Some(x.clone())));
      Some(is_array_guard(
        &Expr::Ident(ident.clone()),
        &paren(Expr::Seq(SeqExpr {
          span: expr.span(),
          exprs: vec![
            Box::new(assign(ident.clone().into(), Expr::Array(ArrayLit{ span:DUMMY_SP, elems:elems }))),
            Box::new(set_ident(&ident)),
          ]})),
        &Expr::Call(expr.clone())))
    }
    Expr::Member(member_expr) => {
      None
    }
    _ => None
  }
}

fn assign(left: SimpleAssignTarget, right: Expr) -> Expr {
  Expr::Assign(AssignExpr {
    left: AssignTarget::Simple(left),
    op: AssignOp::Assign,
    right: Box::new(right),
    span: DUMMY_SP,
  })
}

fn bin_op(left: Expr, op: BinaryOp, right: Expr) -> Expr {
  Expr::Bin(BinExpr {
    span: DUMMY_SP,
    op: op,
    left: Box::new(left),
    right: Box::new(right),
  })
}

fn computed_member(obj: Expr, prop: Expr) -> Expr {
  Expr::Member(MemberExpr {
    span: DUMMY_SP,
    obj: Box::new(obj),
    prop: MemberProp::Computed(ComputedPropName{
      span: DUMMY_SP,
      expr: Box::new(prop),
    })
  })
}

fn member(obj: Expr, prop: Atom) -> Expr {
  Expr::Member(MemberExpr {
    span: DUMMY_SP,
    obj: Box::new(obj),
    prop: MemberProp::Ident(IdentName {
      span: DUMMY_SP,
      sym: prop,
    })
  })
}

fn call(callee: Expr, args: Vec<Expr>) -> Expr {
  Expr::Call(CallExpr {
    span: DUMMY_SP,
    ctxt: SyntaxContext::empty(),
    callee: Callee::Expr(Box::new(callee)),
    args: args.into_iter().map(|x|ExprOrSpread {
      spread: None,
      expr: Box::new(x),
    }).collect(),
    type_args: None,
  })
}

fn call_with_args(callee: Expr, args: Vec<ExprOrSpread>) -> Expr {
  Expr::Call(CallExpr {
    span: DUMMY_SP,
    ctxt: SyntaxContext::empty(),
    callee: Callee::Expr(Box::new(callee)),
    args: args,
    type_args: None,
  })
}

fn paren(expr: Expr) -> Expr {
  Expr::Paren(ParenExpr {
    span: DUMMY_SP,
    expr: Box::new(expr),
  })
}

fn iife(block: BlockStmt) -> Expr {
  call(paren(
    Expr::Fn(FnExpr {
      ident: None,
      function: Box::new(Function {
         params: vec![],
         decorators: vec![],
         span: DUMMY_SP,
         ctxt: SyntaxContext::empty(),
         body: Some(block),
         is_generator: false,
         is_async: false,
         type_params: None,
         return_type: None
      })
    })),
    vec![])
}

fn var_decl(ident: Ident, init: Expr) -> Decl {
  Decl::Var(Box::new(VarDecl {
    span: DUMMY_SP,
    ctxt: SyntaxContext::empty(),
    kind: VarDeclKind::Let,
    declare: false,
    decls: vec![VarDeclarator {
      span: DUMMY_SP,
      name: Pat::Ident(BindingIdent { id: ident, type_ann: None }),
      init: Some(Box::new(init)),
      definite: false,
    }]
  }))
}

fn block(stmts: Vec<Stmt>) -> BlockStmt {
  BlockStmt { span: DUMMY_SP, ctxt: SyntaxContext::empty(), stmts: stmts }
}

/*
  obj.pop()  =>  Array.isArray(obj) ? (function() {let tmp = obj[obj.length - 1]; return obj = obj.slice(0, -1), setObj(obj), tmp}() : obj.pop()
*/
fn immutize_pop(expr: &CallExpr) -> Option<Expr> {
  let member_expr = expr.callee.as_expr().unwrap().as_member().unwrap();
  match &*member_expr.obj {
    Expr::Ident(ident) => {
      let poped = Ident {
        span: DUMMY_SP,
        ctxt: ident.ctxt,
        sym: "poped".into(),
        optional: false,
    };
    Some(is_array_guard(
      &Expr::Ident(ident.clone()),
      &iife(block(vec![
        Stmt::Decl(var_decl(
          poped.clone(),
          computed_member(Expr::Ident(ident.clone()), bin_op(member(Expr::Ident(ident.clone()), "length".into()), op!(bin, "-"), 1.into())))),
        Stmt::Expr(ExprStmt { span: DUMMY_SP, expr: 
          Box::new(Expr::Seq(SeqExpr {
            span: expr.span(),
            exprs: vec![
              Box::new(assign(
                ident.clone().into(),
                call(member(Expr::Ident(ident.clone()), "slice".into()), vec![0.into(), Expr::Unary(UnaryExpr { span: DUMMY_SP, op: op!(unary, "-"), arg: 1.into() })]))),
              Box::new(set_ident(&ident)),
              Box::new(Expr::Ident(poped.clone())),
            ]
          }))
        }),
      ])),
      &Expr::Call(expr.clone())))
    }
    Expr::Member(member_expr) => {
      None
    }
    _ => None
  }
}

/*
  obj.unshift(expr)  =>  Array.isArray(obj) ? obj = [expr, ... obj], obj.length : obj.unshift(expr)
*/
fn immutize_unshift(expr: &CallExpr) -> Option<Expr> {
  todo!()
}

/*
  obj.shift()  =>  Array.isArray(obj) ? let tmp_1 = obj[0], obj = obj.slice(1), tmp_1: obj.shift()
*/
fn immutize_shift(expr: &CallExpr) -> Option<Expr> {
  todo!()
}

/*
  obj.splice(start, delCount, item1, ...items)  =>  Array.isArray(obj) ?
    let deleted = obj.slice(start, start + delCount),
    obj = [... obj.slice(0, start), item1, ...items, ...obj.slice(start + delCount)],
    deleted
    : obj.splice(start, delCount, ...items)
*/
fn immutize_splice(expr: &CallExpr) -> Option<Expr> {
  todo!()
}

fn seq(exprs: Vec<Expr>) -> Expr {
  Expr::Seq(SeqExpr {
    span: DUMMY_SP,
    exprs: exprs.into_iter().map(|x|Box::new(x)).collect(),
  })
}

fn array(elems: Vec<ExprOrSpread>) -> Expr {
  Expr::Array(ArrayLit{ span:DUMMY_SP, elems: elems.into_iter().map(|x| Some(x)).collect() })
}

fn spread_elem(expr: Expr) -> ExprOrSpread {
  ExprOrSpread {
    spread: Some(DUMMY_SP),
    expr: Box::new(expr),
  }
}

/*
  obj.reverse() =>  Array.isArray(obj) ? (obj = [...obj].reverse(), setObj(obj), obj) : obj.reverse()
  obj.prop.sort() =>  Array.isArray(obj.prop) ? (obj = {...obj, prop: [...obj.prop].sort()}, setObj(obj), obj.prop) : obj.prop.sort()
  obj.fill(value, start, end) =>  Array.isArray(obj) ? (obj = [...obj].fill(value, start, end), setObj(obj), obj) : obj.fill(value, start, end)
*/

fn immutize_array_op_by_clone(expr: &CallExpr) -> Option<Expr> {
  let member_expr = expr.callee.as_expr().unwrap().as_member().unwrap();
  let method = member_expr.prop.as_ident().unwrap();
  match &*member_expr.obj {
    Expr::Ident(ident) => {
      Some(is_array_guard(
        &member_expr.obj,
        &paren(seq(vec![
          assign(ident.clone().into(), call_with_args(
            member(array(vec![spread_elem(ident.clone().into())]), method.sym.clone().into()),
            expr.args.clone())),
          set_ident(&ident),
          *member_expr.obj.clone(),
        ])),
        &expr.clone().into()))
    }
    Expr::Member(obj) => {
      Some(is_array_guard(
        &member_expr.obj,
        &paren(seq(vec![
          assign(member_root(obj).unwrap().into(), immutize_member_op(&obj.clone(), call_with_args(
            member(array(vec![spread_elem(obj.clone().into())]), method.sym.clone().into()),
            expr.args.clone())).unwrap()),
          set_ident(&member_root(obj).unwrap()),
          *member_expr.obj.clone(),
        ])),
        &expr.clone().into()))
    }
    _ => None
  }
}

pub fn immutize_array_op(expr: &CallExpr) -> Option<Expr> {
  expr.callee.as_expr()
    .and_then(|expr| expr.as_member())
    .and_then(|member| member.prop.as_ident())
    .and_then(|ident| match ident.sym.as_str() {
      "push" => immutize_push(expr),
      "pop" => immutize_pop(expr),
      "reverse" | "sort" | "fill" | "copyWithin" => immutize_array_op_by_clone(expr),
      _ => None
    })
}


#[cfg(test)]
mod tests {
  use std::sync::Arc;
  use swc_core::ecma::ast::*;
  use swc_core::common::{DUMMY_SP, SourceMap, FileName};
  use swc_ecma_parser::{Parser, StringInput, Syntax};
  use super::*;

  fn parse_expr(code: &str) -> Box<Expr> {
    let sm = SourceMap::new(Default::default());
    let fm = sm.new_source_file(Arc::new(FileName::Real("inline".into())), code.to_string());
    let mut parser = Parser::new(
        Syntax::Es(Default::default()),
        StringInput::from(&*fm),
        None,
    );
    parser.parse_expr().expect("Failed to parse expression")
  }

  #[test]
  fn immutize_member() {
    assert_eq!(
      swc_ecma_codegen::to_code(
        &immutize_member_assignment(
          &*parse_expr("a.b.c").as_member().unwrap(),
          &Expr::Lit(Lit::Null(Null {span: DUMMY_SP}))
        ).unwrap()),
      swc_ecma_codegen::to_code(
        &*parse_expr("a = {...a, b: {... a.b, c: null}}, setA(a), a.b.c"),
      ))
  }

  #[test]
  fn immutize_member_computed() {
    assert_eq!(
      swc_ecma_codegen::to_code(
        &immutize_member_assignment(
          &*parse_expr("a[b].c['key']").as_member().unwrap(),
          &Expr::Lit(Lit::Null(Null {span: DUMMY_SP}))
        ).unwrap()),
      swc_ecma_codegen::to_code(
        &*parse_expr("a = {...a, [b]: {... a[b], c: {... a[b].c, ['key']: null}}}, setA(a), a[b].c['key']"),
      ))
  }

  #[test]
  fn immutize_array_push() {
    assert_eq!(
      swc_ecma_codegen::to_code(
        &immutize_push(&*parse_expr("a.push(0)").as_call().unwrap()).unwrap()),
      swc_ecma_codegen::to_code(
        &*parse_expr("Array.isArray(a) ? (a = [...a, 0], setA(a)) : a.push(0)"),
      ))
  }

  #[test]
  fn immutize_array_pop() {
    assert_eq!(
      swc_ecma_codegen::to_code(
        &immutize_pop(&*parse_expr("a.pop()").as_call().unwrap()).unwrap()),
      swc_ecma_codegen::to_code(
        &*parse_expr("Array.isArray(a) ? (function() {let poped = a[a.length - 1]; a = a.slice(0, -1), setA(a), poped})() : a.pop()"),
      ))
  }

  #[test]
  fn immutize_array_reverse() {
    assert_eq!(
      swc_ecma_codegen::to_code(
        &immutize_array_op_by_clone(&*parse_expr("a.reverse()").as_call().unwrap()).unwrap()),
      swc_ecma_codegen::to_code(
        &*parse_expr("Array.isArray(a) ? (a = [...a].reverse(), setA(a), a) : a.reverse()"),
      ))
  }

  #[test]
  fn immutize_array_reverse_member() {
    assert_eq!(
      swc_ecma_codegen::to_code(
        &immutize_array_op_by_clone(&*parse_expr("a.b.reverse()").as_call().unwrap()).unwrap()),
      swc_ecma_codegen::to_code(
        &*parse_expr("Array.isArray(a.b) ? (a = {... a, b: [...a.b].reverse()}, setA(a), a.b) : a.b.reverse()"),
      ))
  }

  #[test]
  fn immutize_array_fill() {
    assert_eq!(
      swc_ecma_codegen::to_code(
        &immutize_array_op_by_clone(&*parse_expr("a.fill(0, 1, 2)").as_call().unwrap()).unwrap()),
      swc_ecma_codegen::to_code(
        &*parse_expr("Array.isArray(a) ? (a = [...a].fill(0, 1, 2), setA(a), a) : a.fill(0, 1, 2)"),
      ))
  }
}
