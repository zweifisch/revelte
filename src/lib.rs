use std::collections::HashSet;

use swc_core::{
    atoms::Atom,
    common::{Spanned, SyntaxContext, DUMMY_SP},
    ecma::{
        ast::{self, Decl, Expr, Ident, Program},
        transforms::testing::test_inline,
    visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
}};
use swc_core::plugin::{
    plugin_transform,
    proxies::TransformPluginProgramMetadata};


fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    if let Some(first_char) = chars.next() {
        format!("{}{}", first_char.to_uppercase(), chars.as_str())
    } else {
        String::new()
    }
}

pub struct TransformVisitor {
    states: HashSet<String>,
}

impl TransformVisitor {
    fn new() -> Self {
        Self {
            states: HashSet::new(),
        }
    }

    fn handle_func(&mut self, node: &mut ast::BlockStmt) {
        for inner_stmt in &mut node.stmts {
            if let ast::Stmt::Decl(Decl::Var(var_decl)) = &mut *inner_stmt {
                for decl in &mut var_decl.decls {
                    if let (ast::Pat::Ident(name_ident), Some(expr)) = (&mut decl.name, &mut decl.init) {
                        let ident = name_ident.clone();

                        if let Expr::Call(call_expr) = &mut **expr {
                            if let ast::Callee::Expr(callee) = &mut call_expr.callee {
                                if let Expr::Ident(callee_ident) = &mut **callee{
                                    if callee_ident.sym == "$state" && call_expr.args.len() == 1 {
                                        decl.name = ast::Pat::Array(ast::ArrayPat {
                                            span: ident.span(),
                                            optional: false,
                                            type_ann: None,
                                            elems: vec![
                                                Some(ast::Pat::Ident(ast::BindingIdent {
                                                    id: Ident::new(ident.sym.clone().into(), DUMMY_SP, ident.ctxt),
                                                    type_ann: None,
                                                })),
                                                Some(ast::Pat::Ident(ast::BindingIdent {
                                                    id: Ident::new(format!("set{}", capitalize_first(&ident.sym)).into(), ident.span, ident.ctxt),
                                                    type_ann: None,
                                                })),
                                            ],
                                        });
                                        decl.init = Some(Box::new(Expr::Call(ast::CallExpr {
                                            callee: ast::Callee::Expr(
                                                Box::new(Expr::Ident(Ident::new("useState".into(), call_expr.span(), SyntaxContext::empty())))),
                                            args: vec![call_expr.args[0].clone().into()],
                                            type_args: None,
                                            span: expr.span(),
                                            ..Default::default()
                                        })));
                                        self.states.insert(ident.sym.to_string());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

impl VisitMut for TransformVisitor {
    // Implement necessary visit_mut_* methods for actual custom transform.
    // A comprehensive list of possible visitor methods can be found here:
    // https://rustdoc.swc.rs/swc_ecma_visit/trait.VisitMut.html

    fn visit_mut_module(&mut self, node: &mut ast::Module) {
        Vec::insert(&mut node.body, 0, 
            ast::ModuleItem::ModuleDecl(ast::ModuleDecl::Import(ast::ImportDecl {
                span: DUMMY_SP,
                type_only: false,
                with: None,
                phase: ast::ImportPhase::Evaluation,
                specifiers: vec![
                    ast::ImportSpecifier::Named(
                        ast::ImportNamedSpecifier {
                            span: DUMMY_SP,
                            is_type_only: false,
                            local: Ident {
                                ctxt: SyntaxContext::empty(),
                                span: DUMMY_SP,
                                sym: "useState".into(),
                                optional: false,
                            },
                            imported: None,
                    })],
                src: Box::new(swc_core::ecma::ast::Str {
                    raw: None,
                    value: Atom::new("react"),
                    span: DUMMY_SP,
                }),
            }))
        );
        node.visit_mut_children_with(self);
    }

    fn visit_mut_export_default_decl(&mut self, node: &mut ast::ExportDefaultDecl) {
        if let ast::DefaultDecl::Fn(func) = &mut node.decl {
            if let Some(body) = &mut func.function.body {
                self.handle_func(body);
            }
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_export_decl(&mut self, node: &mut ast::ExportDecl) {
        if let Decl::Fn(func) = &mut node.decl {
            if let Some(body) = &mut func.function.body {
                self.handle_func(body);
            }
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_module_item(&mut self, item: &mut ast::ModuleItem) {
        if let ast::ModuleItem::Stmt(stmt) = item {
            if let ast::Stmt::Decl(Decl::Fn(func)) = stmt {
                if let Some(body) = &mut func.function.body {
                    self.handle_func(body);
                }
            }
        }
        item.visit_mut_children_with(self);
    }

    fn visit_mut_arrow_expr(&mut self, node: &mut ast::ArrowExpr) {
        let mut body = node.body.clone();
        if let ast::BlockStmtOrExpr::Expr(expr) = &mut *body {
            let mut expr1 = expr.clone();
            if let Expr::Assign(assign_expr) = &mut *expr1 {
                if let ast::AssignTarget::Simple(target) = &mut assign_expr.left {
                    if let ast::SimpleAssignTarget::Ident(ident) = target {
                        node.body = Box::new(ast::BlockStmtOrExpr::BlockStmt(ast::BlockStmt {
                            ctxt: ident.ctxt,
                            span: expr.span(),
                            stmts: vec![
                                ast::Stmt::Expr(ast::ExprStmt {
                                    span: expr.span(),
                                    expr: expr.clone(),
                                }),
                                ast::Stmt::Expr(ast::ExprStmt {
                                    span: expr.span(),
                                    expr: Box::new(Expr::Call(ast::CallExpr {
                                                callee: ast::Callee::Expr(
                                                    Box::new(Expr::Ident(
                                                        Ident::new(format!("set{}", capitalize_first(&ident.sym)).into(), expr.span(), ident.ctxt)))),
                                                args: vec![
                                                    ast::ExprOrSpread {
                                                        spread: None,
                                                        expr: Box::new(Expr::Ident(Ident::new(ident.sym.clone(), expr.span(), ident.ctxt)))
                                                    }],
                                                type_args: None,
                                                span: expr.span(),
                                                ..Default::default()
                                            }))
                            })],
                        }));
                    }
                };
            }
        };
        node.visit_mut_children_with(self);
    }
}

/// An example plugin function with macro support.
/// `plugin_transform` macro interop pointers into deserialized structs, as well
/// as returning ptr back to host.
///
/// It is possible to opt out from macro by writing transform fn manually
/// if plugin need to handle low-level ptr directly via
/// `__transform_plugin_process_impl(
///     ast_ptr: *const u8, ast_ptr_len: i32,
///     unresolved_mark: u32, should_enable_comments_proxy: i32) ->
///     i32 /*  0 for success, fail otherwise.
///             Note this is only for internal pointer interop result,
///             not actual transform result */`
///
/// This requires manual handling of serialization / deserialization from ptrs.
/// Refer swc_plugin_macro to see how does it work internally.
#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    program.fold_with(&mut as_folder(TransformVisitor::new()))
}

// An example to test plugin transform.
// Recommended strategy to test plugin's transform is verify
// the Visitor's behavior, instead of trying to run `process_transform` with mocks
// unless explicitly required to do so.
test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    use_state,
    // Input codes
    r#"function App() {
    const count = $state(0);
}"#,
    // Output codes after transformed with plugin
    r#"import { useState } from "react";
function App() {
    const [count, setCount] = useState(0);
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    export,
    // Input codes
    r#"export function App() {
    const count = $state(0);
}"#,
    // Output codes after transformed with plugin
    r#"import { useState } from "react";
export function App() {
    const [count, setCount] = useState(0);
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    default_export,
    // Input codes
    r#"export default function App() {
    const count = $state(0);
}"#,
    // Output codes after transformed with plugin
    r#"import { useState } from "react";
export default function App() {
    const [count, setCount] = useState(0);
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    object,
    // Input codes
    r#"function App() {
    const state = $state({foo: 1, bar: []});
}"#,
    // Output codes after transformed with plugin
    r#"import { useState } from "react";
function App() {
    const [state, setState] = useState({foo: 1, bar: []});
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    component,
    // Input codes
    r#"function App() {
  let count = $state(0);
  let clickHandler = () => count += 1;
  return null;
}"#,
    // Output codes after transformed with plugin
    r#"import { useState } from "react";
function App() {
    let [count, setCount] = useState(0);
    let clickHandler = () => {
        count += 1;
        setCount(count);
    }
    return null;
}"#
);