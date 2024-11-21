use std::cmp::Ordering;
use swc_core::ecma::ast::{Ident, MemberExpr};

use crate::util::member_expr_to_string;


#[derive(Debug, Clone, Eq)]
pub enum Dep {
  Ident(Ident),
  MemberExpr(MemberExpr),
}

impl std::hash::Hash for Dep {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    match self {
        Dep::Ident(ident) => ident.to_id().hash(state),
        Dep::MemberExpr(expr) => member_expr_to_string(expr).hash(state),
    }
  }
}

impl PartialEq for Dep {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
        (Dep::Ident(ident), Dep::Ident(ident2)) => ident.to_string() == ident2.to_string(),
        (Dep::MemberExpr(expr), Dep::MemberExpr(expr2)) => member_expr_to_string(expr) == member_expr_to_string(expr2),
        (_, _) => false
    }
  }
}

impl PartialOrd for Dep {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match (self, other) {
      (Dep::Ident(ident), Dep::Ident(ident2)) => Some(ident.to_string().cmp(&ident2.to_string())),
      (Dep::MemberExpr(expr), Dep::MemberExpr(expr2)) => Some(member_expr_to_string(expr).cmp(&member_expr_to_string(expr2))),
      (Dep::Ident(_), Dep::MemberExpr(_)) => Some(Ordering::Less),
      _ => Some(Ordering::Greater),
    }
  }
}

impl Ord for Dep {
  fn cmp(&self, other: &Self) -> Ordering {
    match (self, other) {
      (Dep::Ident(ident), Dep::Ident(ident2)) => ident.to_string().cmp(&ident2.to_string()),
      (Dep::MemberExpr(expr), Dep::MemberExpr(expr2)) => member_expr_to_string(expr).cmp(&member_expr_to_string(expr2)),
      (Dep::Ident(_), Dep::MemberExpr(_)) => Ordering::Less,
      _ => Ordering::Greater,
    }
  }
}