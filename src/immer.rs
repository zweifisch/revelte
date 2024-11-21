use swc_core::{common::{SyntaxContext, DUMMY_SP},
  ecma::ast::{AssignExpr, AssignOp, CallExpr, Callee, Expr, ExprOrSpread, Ident, KeyValueProp, MemberExpr, MemberProp, ObjectLit, Prop, PropName, PropOrSpread, SeqExpr, SpreadElement}};

use crate::util::{self, member_root};

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
            Box::new(Expr::Ident(Ident::new(ident.sym.clone(), DUMMY_SP, ident.ctxt))),
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
        &*parse_expr("a = {...a, b: {... a.b, c: null}}, setA(a), a"),
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
        &*parse_expr("a = {...a, [b]: {... a[b], c: {... a[b].c, ['key']: null}}}, setA(a), a"),
      ))
  }
}