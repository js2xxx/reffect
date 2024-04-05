#![allow(internal_features)]
#![feature(allow_internal_unstable)]

mod block;
mod expr;
mod func;

use proc_macro2::Span;
use quote::ToTokens;
use syn::{parse::Parse, parse_macro_input, punctuated::Punctuated, spanned::Spanned, Token};

#[derive(Default)]
struct Args {
    is_static: Option<Token![static]>,
    is_move: Option<Token![move]>,
    effects: Punctuated<syn::Type, Token![,]>,
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
        let effects = input.parse_terminated(syn::Type::parse, Token![,])?;
        Ok(Args { is_static, is_move, effects })
    }
}

struct DesugarExpr<'a> {
    is_static: bool,
    effect_list: &'a syn::Type,
}

impl syn::visit_mut::VisitMut for DesugarExpr<'_> {
    fn visit_expr_mut(&mut self, i: &mut syn::Expr) {
        match i {
            syn::Expr::Await(syn::ExprAwait { base, await_token, .. }) => {
                *i = crate::expr::expand_await(
                    await_token.span,
                    base,
                    self.is_static,
                    Some(self.effect_list),
                )
            }
            syn::Expr::Yield(syn::ExprYield { yield_token, expr, .. }) => {
                *i =
                    crate::expr::expand_yield(yield_token.span, expr.take(), Some(self.effect_list))
            }
            _ => syn::visit_mut::visit_expr_mut(self, i),
        }
    }
}

#[proc_macro]
#[allow(non_snake_case)]
pub fn EffectList(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    fn parse_effects(
        input: syn::parse::ParseStream,
    ) -> syn::Result<Punctuated<syn::Type, Token![,]>> {
        input.parse_terminated(syn::Type::parse, Token![,])
    }
    match syn::parse::Parser::parse(parse_effects, input) {
        Ok(effects) => {
            let expand_effect = crate::expr::expand_effect(effects);
            expand_effect.into_token_stream().into()
        }
        Err(e) => e.to_compile_error().into(),
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

#[proc_macro]
pub fn do_await(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let expr = parse_macro_input!(input as syn::Expr);
    let expand_await = crate::expr::expand_await(Span::call_site(), &expr, false, None);
    expand_await.into_token_stream().into()
}

#[proc_macro]
pub fn do_yield(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let expr = if !input.is_empty() {
        Some(parse_macro_input!(input as Box<syn::Expr>))
    } else {
        None
    };
    let expand_yield = crate::expr::expand_yield(Span::call_site(), expr, None);
    expand_yield.into_token_stream().into()
}
