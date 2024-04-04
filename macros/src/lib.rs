#![allow(internal_features)]
#![feature(allow_internal_unstable)]

mod block;
mod expr;
mod func;

use syn::{parse::Parse, parse_macro_input, punctuated::Punctuated, spanned::Spanned, Token};

struct Args {
    is_static: Option<Token![static]>,
    is_move: Option<Token![move]>,
    effects: Punctuated<syn::Ident, Token![,]>,
}

impl Parse for Args {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let is_static: Option<Token![static]> = input.parse()?;

        let is_move: Option<Token![move]> = input.parse()?;
        if input.peek(Token![static]) {
            return Err(syn::Error::new(
                is_move.span(),
                "the keyword `static` must come before `move`",
            ));
        }

        if is_static.is_some() || is_move.is_some() {
            input.parse::<Token![;]>()?;
        }
        let effects = input.parse_terminated(syn::Ident::parse, Token![,])?;
        Ok(Args { is_static, is_move, effects })
    }
}

struct DesugarExpr {
    is_static: bool,
}

impl syn::visit_mut::VisitMut for DesugarExpr {
    fn visit_expr_mut(&mut self, i: &mut syn::Expr) {
        match i {
            syn::Expr::Await(syn::ExprAwait { base, await_token, .. }) => {
                *i = crate::expr::expand_await(await_token.span, base, self.is_static)
            }
            syn::Expr::Yield(syn::ExprYield { yield_token, expr, .. }) => {
                *i = crate::expr::expand_yield(yield_token.span, expr.clone())
            }
            _ => syn::visit_mut::visit_expr_mut(self, i),
        }
    }
}

#[proc_macro]
#[allow_internal_unstable(coroutine_trait, coroutines)]
pub fn effectful_block(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let block = parse_macro_input!(input as block::EffectfulBlock);
    block::expand_block(block).into()
}

#[proc_macro_attribute]
#[allow_internal_unstable(coroutine_trait, coroutines)]
pub fn effectful(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as Args);
    let input = parse_macro_input!(input as syn::ItemFn);
    func::expand_func(args, input).into()
}
