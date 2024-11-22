mod util;
mod dep;
mod immer;

use std::collections::HashSet;
use dep::Dep;
use immer::immutize_update;
use swc_core::{
    atoms::Atom,
    common::{Spanned, SyntaxContext, DUMMY_SP},
    ecma::{
        ast::{self, BlockStmtOrExpr, Decl, Expr, ExprOrSpread, Ident, Program },
        transforms::testing::test_inline,
    visit::{as_folder, FoldWith, VisitMut, VisitMutWith},
}};
use swc_core::plugin::{
    plugin_transform,
    proxies::TransformPluginProgramMetadata};
use util::member_root;


pub struct TransformVisitor {
    declared: Vec<HashSet<Atom>>,
    deps: Vec<HashSet<Dep>>,
    last_deps: HashSet<Dep>,
    states: HashSet<ast::Id>,
    imports: HashSet<String>,
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
            states: HashSet::new(),
            last_deps: HashSet::new(),
            imports: HashSet::new(),
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

    fn add_state(&mut self, name: ast::Id) {
        self.states.insert(name);
    }

    fn process_expr(&mut self, expr: &Expr) -> Option<Expr> {
        match &expr {
            Expr::Assign(assign) => {
                match &assign.left {
                    ast::AssignTarget::Simple(target) => {
                        match target {
                            ast::SimpleAssignTarget::Ident(ident) => {
                                if self.states.contains(&ident.id.to_id()) {
                                    // foo = foo + 1  =>  (foo = foo + 1, setFoo(foo), foo)
                                    Some(Expr::Seq(ast::SeqExpr {
                                        exprs: vec![
                                            Box::new(expr.clone()),
                                            Box::new(Expr::Call(ast::CallExpr {
                                                callee: ast::Callee::Expr(
                                                    Box::new(Expr::Ident(
                                                        Ident::new(format!("set{}", util::capitalize_first(&ident.sym)).into(), DUMMY_SP, SyntaxContext::empty())))),
                                                args: vec![
                                                    ast::ExprOrSpread {
                                                        spread: None,
                                                        expr: Box::new(Expr::Ident(Ident::new(ident.sym.clone(), DUMMY_SP, ident.ctxt)))
                                                    }],
                                                type_args: None,
                                                span: DUMMY_SP,
                                                ctxt: SyntaxContext::empty(),
                                            })),
                                            Box::new(Expr::Ident(Ident::new(ident.sym.clone(), DUMMY_SP, ident.ctxt))),
                                        ],
                                        span: expr.span(),
                                    }))
                                } else {
                                    None
                                }
                            }
                            ast::SimpleAssignTarget::Member(member_expr) => {
                                if let Some(ident) = member_root(member_expr) {
                                    if self.states.contains(&ident.to_id()) {
                                        return immer::immutize_member_assignment(member_expr, &assign.right)
                                    }
                                }
                                None
                            },
                            _ => None
                        }
                    }
                    ast::AssignTarget::Pat(_) => None,
                }
            },
            Expr::Update(update) => {
                match &*update.arg {
                    Expr::Ident(ident) => {
                        if self.states.contains(&ident.to_id()) {
                            immutize_update(&expr, &ident, &update.op, update.prefix)
                        } else {
                            None
                        }
                    }
                    Expr::Member(member_expr) => {
                        if let Some(ident) = member_root(member_expr) {
                            if self.states.contains(&ident.to_id()) {
                                return immer::immutize_member_update(member_expr, &update.op, update.prefix)
                            }
                        }
                        None
                    }
                    _ => None
                }
            }
            _ => None
        }
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

    fn visit_mut_arrow_expr(&mut self, node: &mut ast::ArrowExpr) {
        self.push_scope();
        for param in &node.params {
            for name in util::find_declared(&param) {
                self.add_to_scope(name.clone());
            }
        }
        if let BlockStmtOrExpr::Expr(expr) = &mut *node.body {
            if let Some(new_expr) = self.process_expr(&expr) {
                *node.body = BlockStmtOrExpr::Expr(Box::new(new_expr));
            }
        }
        node.visit_mut_children_with(self);
        self.pop_scope();
    }

    fn visit_mut_expr_stmt(&mut self, node: &mut ast::ExprStmt) {
        if let Some(new_expr) = self.process_expr(&node.expr) {
            *node.expr = new_expr;
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_assign_expr(&mut self, node: &mut ast::AssignExpr) {
        match &node.left {
            ast::AssignTarget::Simple(target) => {
                match target {
                    ast::SimpleAssignTarget::Ident(ident) => {
                        self.add_to_scope(ident.sym.clone());
                    },
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
                for decl in &mut v.decls {
                    for name in util::find_declared(&decl.name) {
                        self.add_to_scope(name);
                    }

                    if let (ast::Pat::Ident(name_ident), Some(expr)) = (&mut decl.name, &mut decl.init) {
                        let ident = name_ident.clone();

                        if let Expr::Call(call_expr) = &mut **expr {
                            if let ast::Callee::Expr(callee) = &mut call_expr.callee {
                                if let Expr::Ident(callee_ident) = &mut **callee{
                                    if callee_ident.sym == "$state" && call_expr.args.len() == 1 {
                                        self.add_state(ident.id.to_id());
                                        self.imports.insert("useState".into());
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
            _ => {}
        }
        node.visit_mut_children_with(self);
    }

    fn visit_mut_module(&mut self, node: &mut ast::Module) {
        node.visit_mut_children_with(self);
        if self.imports.len() > 0 {
            let mut imports: Vec<String> = self.imports.clone().into_iter().collect();
            imports.sort();
            Vec::insert(&mut node.body, 0, 
                ast::ModuleItem::ModuleDecl(ast::ModuleDecl::Import(ast::ImportDecl {
                    span: DUMMY_SP,
                    type_only: false,
                    with: None,
                    phase: ast::ImportPhase::Evaluation,
                    specifiers: imports.into_iter().map(|x|
                        ast::ImportSpecifier::Named(
                            ast::ImportNamedSpecifier {
                                span: DUMMY_SP,
                                is_type_only: false,
                                local: Ident::new(x.to_string().into(), DUMMY_SP, SyntaxContext::empty()),
                                imported: None,
                            }),
                    ).collect(),
                    src: Box::new(swc_core::ecma::ast::Str {
                        raw: None,
                        value: Atom::new("react"),
                        span: DUMMY_SP,
                    }),
                })))
        }
    }

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

    fn visit_mut_call_expr(&mut self, node: &mut ast::CallExpr) {
        node.visit_mut_children_with(self);
        if let ast::Callee::Expr(callee) = &mut node.callee {
            if let Expr::Ident(callee_ident) = &mut **callee{
                if callee_ident.sym == "$effect" && node.args.len() == 1 {
                    callee_ident.sym = "useEffect".into();
                    callee_ident.ctxt = SyntaxContext::empty();
                    self.imports.insert("useEffect".into());
                    // println!("deps {:?}", &self.last_deps);
                    let mut deps: Vec<Dep> = self.last_deps.clone().into_iter().collect();
                    deps.sort();
                    node.args = vec![
                        node.args[0].clone().into(),
                        ExprOrSpread {
                            spread: None,
                            expr: Box::new(Expr::Array(ast::ArrayLit {
                                span: DUMMY_SP,
                                elems: deps.into_iter().map(|x| Some(ExprOrSpread {
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
    let clickHandler = () => (count += 1, setCount(count), count);
    return null;
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    assign2,
    r#"function App() {
  let count = $state(0);
  let clickHandler = () => {
    count+=1;
    console.log(count);
  }
  return null;
}"#,
    r#"import { useState } from "react";
function App() {
    let [count, setCount] = useState(0);
    let clickHandler = () => {
        count += 1, setCount(count), count;
        console.log(count);
    }
    return null;
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    update,
    r#"function App() {
  let count = $state(0);
  let clickHandler = () => count ++
  return null;
}"#,
    r#"import { useState } from "react";
function App() {
    let [count, setCount] = useState(0);
    let clickHandler = () => (count ++, setCount(count), count - 1);
    return null;
}"#
);

test_inline!(
    Default::default(),
    |_| as_folder(TransformVisitor::new()),
    member_update,
    r#"function App() {
  let count = $state({value: 0});
  let clickHandler = () => count.value ++
  return null;
}"#,
    r#"import { useState } from "react";
function App() {
    let [count, setCount] = useState({value: 0});
    let clickHandler = () => (count = {... count, value: count.value + 1}, setCount(count), count.value - 1);
    return null;
}"#
);

test_inline!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsSyntax {
        jsx: true,
        ..Default::default()
    }),
    |_| as_folder(TransformVisitor::new()),
    jsx,
    r#"function App() {
  let count = $state(0);
  return <div onClick={() => ++count}>{count}</div>
}"#,
    r#"import { useState } from "react";
function App() {
    let [count, setCount] = useState(0);
    return <div onClick={() => (++count, setCount(count), count)}>{count}</div>
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
    r#"import { useEffect, useState } from "react";
function App() {
    let [pageTitle, setPageTitle] = useState("");
    useEffect(() => {pageTitle = document.title, setPageTitle(pageTitle), pageTitle}, [])
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
    fetch(props.url.toString()).then(x => x.json()).then(val => {
      data = val
    })
  });
  return null;
}"#,
    r#"import { useEffect, useState } from "react";
function App(props) {
    let [data, setData] = useState({});
    useEffect(() => {
      console.log(props.url)
      fetch(props.url.toString()).then(x => x.json()).then(val => {
        data = val, setData(data), data;
      })
    }, [props.url, props.url.toString])
    return null;
}"#);

test_inline!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsSyntax {
        jsx: true,
        ..Default::default()
    }),
    |_| as_folder(TransformVisitor::new()),
    example, 
    r#"function App() {
  let count = $state(0)
  $effect(() => {
    console.log(`count: ${count}`)
  })
  return <div onClick={() => count += 1}>{count}</div>
}"#,
    r#"import { useEffect, useState } from "react";
function App() {
  let [count, setCount] = useState(0)
  useEffect(() => {
    console.log(`count: ${count}`)
  }, [count])
  return <div onClick={() => (count += 1, setCount(count), count)}>{count}</div>
}"#);

test_inline!(
    swc_ecma_parser::Syntax::Es(swc_ecma_parser::EsSyntax {
        jsx: true,
        ..Default::default()
    }),
    |_| as_folder(TransformVisitor::new()),
    example_object, 
    r#"function App() {
  let state = $state({count:0})
  $effect(() => {
    console.log(`count: ${state.count}`)
  })
  return <div onClick={() => state.count = state.count + 1}>{state.count}</div>
}"#,
    r#"import { useEffect, useState } from "react";
function App() {
  let [state, setState] = useState({count:0})
  useEffect(() => {
    console.log(`count: ${state.count}`)
  }, [state.count])
  return <div onClick={() => (state = {... state, count: state.count + 1}, setState(state), state.count)}>{state.count}</div>
}"#);