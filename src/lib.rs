mod util;
mod dep;

use std::collections::HashSet;
use dep::Dep;
use swc_core::{
    atoms::Atom,
    common::{Spanned, SyntaxContext, DUMMY_SP},
    ecma::{
        ast::{self, Decl, Expr, ExprOrSpread, Ident, Program},
        transforms::testing::test_inline,
    visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
}};
use swc_core::plugin::{
    plugin_transform,
    proxies::TransformPluginProgramMetadata};


pub struct TransformVisitor {
    declared: Vec<HashSet<Atom>>,
    deps: Vec<HashSet<Dep>>,
    last_deps: HashSet<Dep>,
    member: Option<ast::MemberExpr>,
}

impl TransformVisitor {
    fn new() -> Self {
        let mut declared = Vec::new();
        declared.push(HashSet::new());
        let mut deps= Vec::new();
        deps.push(HashSet::new());
        Self {
            declared: declared,
            deps: deps,
            last_deps: HashSet::new(),
            member: None,
        }
    }

    fn push_scope(&mut self) {
        self.declared.push(HashSet::new());
        self.deps.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.declared.pop();
        self.last_deps = self.deps.pop().unwrap();
        // println!("{:?} {:?}", self.declared.pop(), self.deps.pop());
    }

    fn add_to_scope(&mut self, name: Atom) {
        let mut last = self.declared.pop().unwrap();
        last.insert(name);
        self.declared.push(last);
    }

    fn add_to_deps(&mut self, name: Dep) {
        let mut last = self.deps.pop().unwrap();
        last.insert(name);
        self.deps.push(last);
    }

    fn current_scope(&mut self) -> &HashSet<Atom> {
        self.declared.last().unwrap()
    }

    fn declared_in_parent_scope(&mut self, name: &Atom) -> bool {
        if self.current_scope().contains(name) {
            return false
        }
        if self.declared.len() > 1 {
            return self.declared[self.declared.len() - 2].contains(name)
        }
        return false
    }

}

impl VisitMut for TransformVisitor {

    fn visit_mut_ident(&mut self, node: &mut Ident) {
        // println!("{:?}", node.to_id());
        if self.declared_in_parent_scope(&node.sym.clone()) {
            self.add_to_deps(Dep::Ident(node.clone()));
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_member_expr(&mut self, node: &mut ast::MemberExpr) {
        if self.member == None {
            self.member = Some(node.clone());
        }
        match &node.prop {
            ast::MemberProp::Computed(prop) => {
                match &*prop.expr {
                    Expr::Lit(_) => {}
                    _ => {
                        self.member = None;
                    }
                }
            }
            ast::MemberProp::Ident(_) => {
                match &*node.obj {
                    ast::Expr::Ident(ident) => {
                        // println!("done {:?}", self.member);
                        if self.declared_in_parent_scope(&ident.sym.clone()) {
                            self.add_to_deps(Dep::MemberExpr(self.member.to_owned().unwrap()));
                        }
                        self.member = None;
                        return
                    }
                    ast::Expr::Member(_) => { }
                    _ => {
                        self.member = None;
                    }
                }
            },
            ast::MemberProp::PrivateName(_) => {},
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_assign_expr(&mut self, node: &mut ast::AssignExpr) {
        match &node.left {
            ast::AssignTarget::Simple(target) => {
                match target {
                    ast::SimpleAssignTarget::Ident(ident) => self.add_to_scope(ident.sym.clone()),
                    ast::SimpleAssignTarget::Member(_) => {},
                    _ => {},
                }
            }
            ast::AssignTarget::Pat(pat) => {
                match pat {
                    ast::AssignTargetPat::Array(array) => {
                        for declared in util::find_declared(&ast::Pat::Array(array.clone())) {
                            self.add_to_scope(declared);
                        }
                    }
                    ast::AssignTargetPat::Object(obj) => {
                        for declared in util::find_declared(&ast::Pat::Object(obj.clone())) {
                            self.add_to_scope(declared);
                        }
                    }
                    ast::AssignTargetPat::Invalid(_) => {}
                }

            }
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_decl(&mut self, node: &mut Decl) {
        match node {
            Decl::Class(cls) => {
                self.push_scope();
                self.add_to_scope(cls.ident.sym.clone());
                self.pop_scope();
            }
            Decl::Fn(f) => {
                self.add_to_scope(f.ident.sym.clone());
            }
            Decl::Var(v) => {
                for decl in &v.decls {
                    for name in util::find_declared(&decl.name) {
                        self.add_to_scope(name);
                    }
                }
            }
            _ => {}
        }
        node.visit_mut_children_with(self);
    }

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

    // fn visit_mut_export_default_decl(&mut self, node: &mut ast::ExportDefaultDecl) {
    //     if let ast::DefaultDecl::Fn(func) = &mut node.decl {
    //         if let Some(body) = &mut func.function.body {
    //         }
    //     }
    //     node.visit_mut_children_with(self);
    // }

    // fn visit_mut_export_decl(&mut self, node: &mut ast::ExportDecl) {
    //     if let Decl::Fn(func) = &mut node.decl {
    //         if let Some(body) = &mut func.function.body {
    //         }
    //     }
    //     node.visit_mut_children_with(self);
    // }

    // fn visit_mut_module_item(&mut self, item: &mut ast::ModuleItem) {
    //     if let ast::ModuleItem::Stmt(stmt) = item {
    //         if let ast::Stmt::Decl(Decl::Fn(func)) = stmt {
    //             if let Some(body) = &mut func.function.body {
    //             }
    //         }
    //     }
    //     item.visit_mut_children_with(self);
    // }

    fn visit_mut_function(&mut self, node: &mut ast::Function) {
        self.push_scope();
        for param in &node.params {
            for name in util::find_declared(&param.pat) {
                self.add_to_scope(name.clone());
            }
        }
        node.visit_mut_children_with(self);
        self.pop_scope();
    }

    fn visit_mut_arrow_expr(&mut self, node: &mut ast::ArrowExpr) {
        self.push_scope();
        for param in &node.params {
            for name in util::find_declared(&param) {
                self.add_to_scope(name.clone());
            }
        }
        let mut body = node.body.clone();
        if let ast::BlockStmtOrExpr::Expr(expr) = &mut *body {
            let mut expr1 = expr.clone();
            if let Expr::Assign(assign_expr) = &mut *expr1 {
                if let ast::AssignTarget::Simple(target) = &mut assign_expr.left {
                    if let ast::SimpleAssignTarget::Ident(ident) = target {
                        node.body = Box::new(ast::BlockStmtOrExpr::BlockStmt(ast::BlockStmt {
                            ctxt: SyntaxContext::empty(),
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
                                                        Ident::new(format!("set{}", util::capitalize_first(&ident.sym)).into(), expr.span(), SyntaxContext::empty())))),
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
        self.pop_scope();
    }

    fn visit_mut_call_expr(&mut self, node: &mut ast::CallExpr) {
        node.visit_mut_children_with(self);
        if let ast::Callee::Expr(callee) = &mut node.callee {
            if let Expr::Ident(callee_ident) = &mut **callee{
                if callee_ident.sym == "$effect" && node.args.len() == 1 {
                    callee_ident.sym = "useEffect".into();
                    // println!("deps {:?}", &self.last_deps);
                    node.args = vec![
                        node.args[0].clone().into(),
                        ExprOrSpread {
                            spread: None,
                            expr: Box::new(Expr::Array(ast::ArrayLit {
                                span: DUMMY_SP,
                                elems: self.last_deps.clone().into_iter().map(|x| Some(ExprOrSpread {
                                    spread: None,
                                    expr: Box::new(
                                        match x {
                                            Dep::Ident(x) => Expr::Ident(x),
                                            Dep::MemberExpr(x) => Expr::Member(x),
                                        })
                                })).collect(),
                            }))
                        }
                    ];
                }
            }
        }
    }

    fn visit_mut_block_stmt(&mut self, node: &mut ast::BlockStmt) {
        node.visit_mut_children_with(self);
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
                                                    id: Ident::new(ident.sym.clone().into(), DUMMY_SP, name_ident.ctxt),
                                                    type_ann: None,
                                                })),
                                                Some(ast::Pat::Ident(ast::BindingIdent {
                                                    id: Ident::new(format!("set{}", util::capitalize_first(&ident.sym)).into(), ident.span, name_ident.ctxt),
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

#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    program.fold_with(&mut as_folder(TransformVisitor::new()))
}

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    use_state,
    r#"function App() {
    const count = $state(0);
}"#,
    r#"import { useState } from "react";
function App() {
    const [count, setCount] = useState(0);
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    export,
    r#"export function App() {
    const count = $state(0);
}"#,
    r#"import { useState } from "react";
export function App() {
    const [count, setCount] = useState(0);
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    default_export,
    r#"export default function App() {
    const count = $state(0);
}"#,
    r#"import { useState } from "react";
export default function App() {
    const [count, setCount] = useState(0);
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    object,
    r#"function App() {
    const state = $state({foo: 1, bar: []});
}"#,
    r#"import { useState } from "react";
function App() {
    const [state, setState] = useState({foo: 1, bar: []});
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    assign,
    r#"function App() {
  let count = $state(0);
  let clickHandler = () => count += 1;
  return null;
}"#,
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

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    effect,
    r#"function App() {
  let pageTitle = $state("");
  $effect(() => {
    pageTitle = document.title;
  });
  return null;
}"#,
    r#"import { useState } from "react";
function App() {
    let [pageTitle, setPageTitle] = useState("");
    useEffect(() => {
      pageTitle = document.title;
    }, [])
    return null;
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    effect_with_deps,
    r#"function App(props) {
  let data = $state({});
  $effect(() => {
    console.log(props.url)
    fetch(props.url.toString()).then(x => x.json()).then(val => data = val)
  });
  return null;
}"#,
    r#"import { useState } from "react";
function App(props) {
    let [data, setData] = useState({});
    useEffect(() => {
      console.log(props.url)
      fetch(props.url.toString()).then(x => x.json()).then(val => {
        data = val
        setData(data)
      })
    }, [props.url, props.url.toString])
    return null;
}"#
);