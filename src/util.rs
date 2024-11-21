use std::collections::HashSet;

use swc_core::{
    atoms::Atom,
    ecma::ast::{self, Expr, Ident, MemberExpr}};

pub fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    if let Some(first_char) = chars.next() {
        format!("{}{}", first_char.to_uppercase(), chars.as_str())
    } else {
        String::new()
    }
}

pub fn find_declared(pat: &ast::Pat) -> HashSet<Atom> {
    let mut deps: HashSet<Atom> = HashSet::new();
    match &pat {
        ast::Pat::Ident(ident) => {
            deps.insert(ident.sym.clone());
        }
        ast::Pat::Array(list) => {
            for el in &list.elems {
                if let Some(element) = el {
                    let declared = find_declared(element);
                    deps = deps.union(&declared).cloned().into_iter().collect();
                }
            }
        }
        ast::Pat::Rest(rest) => {
            if let Some(ident) = &rest.arg.clone().ident() {
                deps.insert(ident.sym.clone());
            }
        }
        ast::Pat::Assign(assign) => {
            deps = deps.union(&find_declared(&assign.left)).cloned().into_iter().collect();
        }
        ast::Pat::Object(obj) => {
            for prop in &obj.props {
                match prop {
                    ast::ObjectPatProp::KeyValue(kv) => {
                        deps = deps.union(&find_declared(&kv.value)).cloned().into_iter().collect();
                    }
                    ast::ObjectPatProp::Rest(rest) => {
                        if let Some(ident) = &rest.arg.clone().ident() {
                            deps.insert(ident.sym.clone());
                        }
                    }
                    ast::ObjectPatProp::Assign(_) => {}
                }
            }
        }
        ast::Pat::Expr(_) => { }
        ast::Pat::Invalid(_) => {}
    }
    deps
}

pub fn member_root(expr: &MemberExpr) -> Option<Ident> {
    match &*expr.obj {
        Expr::Ident(ident) => Some(ident.clone()),
        Expr::Member(member) => member_root(&member),
        _ => None
    }
}

pub fn member_expr_to_string(expr: &ast::MemberExpr) -> String {
    let prefix = match &*expr.obj {
        ast::Expr::Member(obj) => {
            member_expr_to_string(&obj)
        }
        ast::Expr::Ident(ident) => {
            ident.sym.to_string()
        }
        _ => "".into()
    };
    let prop = match &expr.prop {
        ast::MemberProp::Ident(ident) => format!(".{}", ident.sym),
        ast::MemberProp::Computed(prop) => {
            match &*prop.expr {
                ast::Expr::Lit(x) => {
                    match x {
                        ast::Lit::Str(x) => format!("['{}']", x.value),
                        ast::Lit::Num(number) => format!("[{}]", number),
                        _ => "".into(),
                    }
                },
                ast::Expr::Ident(ident) => format!("[{}]", ident.sym),
                _ => "".into(),
            }
        }
        _ => "".into(),
    };
    format!("{}{}", prefix, prop)
}

#[cfg(test)]
mod tests {
    use ast::*;
    use swc_core::common::{SyntaxContext, DUMMY_SP};

    use super::*;

    #[test]
    fn member() {
        let result = member_expr_to_string(&MemberExpr{
            span: DUMMY_SP,
            prop: MemberProp::Ident(IdentName {
                span: DUMMY_SP,
                sym: "c".into()
            }),
            obj: Box::new(Expr::Member(MemberExpr {
                span: DUMMY_SP,
                prop: MemberProp::Ident(IdentName {
                    span: DUMMY_SP,
                    sym: "b".into()
                }),
                obj: Box::new(Expr::Ident(Ident {
                    span: DUMMY_SP,
                    ctxt: SyntaxContext::empty(),
                    sym: "a".into(),
                    optional: false
                })),
            })),
        });
        assert_eq!(result, "a.b.c");
    }

    #[test]
    fn member_lit() {
        let result = member_expr_to_string(&MemberExpr{
            span: DUMMY_SP,
            prop: MemberProp::Computed(ComputedPropName {
                span: DUMMY_SP,
                expr: Box::new(Expr::Lit(Lit::Str("b c".into())))
            }),
            obj: Box::new(Expr::Ident(Ident {
                span: DUMMY_SP,
                ctxt: SyntaxContext::empty(),
                sym: "a".into(),
                optional: false
            })),
        });
        assert_eq!(result, "a['b c']");
    }

    #[test]
    fn member_number_indice() {
        let result = member_expr_to_string(&MemberExpr{
            span: DUMMY_SP,
            prop: MemberProp::Computed(ComputedPropName {
                span: DUMMY_SP,
                expr: Box::new(Expr::Lit( Lit::Num(0.into())))
            }),
            obj: Box::new(Expr::Ident(Ident {
                span: DUMMY_SP,
                ctxt: SyntaxContext::empty(),
                sym: "a".into(),
                optional: false
            })),
        });
        assert_eq!(result, "a[0]");
    }

    #[test]
    fn member_computed() {
        let result = member_expr_to_string(&MemberExpr{
            span: DUMMY_SP,
            prop: MemberProp::Ident("c".into()),
            obj: Box::new(Expr::Member(MemberExpr {
                span: DUMMY_SP,
                prop: MemberProp::Computed(ComputedPropName {
                    span: DUMMY_SP,
                    expr: Box::new(Expr::Ident("b".into()))
                }),
                obj: Box::new(Expr::Ident(Ident {
                    span: DUMMY_SP,
                    ctxt: SyntaxContext::empty(),
                    sym: "a".into(),
                    optional: false
                })),
            })),
        });
        assert_eq!(result, "a[b].c");
    }
}
