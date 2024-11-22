use swc_core::{common::{Spanned, SyntaxContext, DUMMY_SP},
  ecma::ast::{op, AssignExpr, AssignOp, AssignTargetPat, BinExpr, CallExpr, Callee, Expr, ExprOrSpread, Ident, KeyValueProp, MemberExpr, MemberProp, ObjectLit, ObjectPatProp, Prop, PropName, PropOrSpread, SeqExpr, SpreadElement, UpdateOp}};

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
}