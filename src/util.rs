use std::collections::HashSet;

use swc_core::{
    atoms::Atom,
    ecma::ast::{self}};

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
