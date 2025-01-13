#![allow(internal_features)]
#![feature(allow_internal_unstable)]
#![feature(non_exhaustive_omitted_patterns_lint)]

mod block;
mod expr;
mod func;
mod group;
mod handler;

use proc_macro2::Span;
use quote::ToTokens;
use syn::{Token, parse::Parse, parse_macro_input, punctuated::Punctuated, spanned::Spanned};

#[derive(Clone, PartialEq, Eq)]
enum Effect {
    Group(syn::Type),
    List(syn::Type),
}

impl Parse for Effect {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![...]) {
            let _: Token![...] = input.parse()?;
            let ty = input.parse()?;
            Ok(Effect::List(ty))
        } else {
            let ty = input.parse()?;
            Ok(Effect::Group(ty))
        }
    }
}

impl Effect {
    fn to_list(&self) -> syn::Type {
        match self {
            Effect::Group(g) => syn::parse_quote!(<#g as reffect::effect::EffectGroup>::Effects),
            Effect::List(l) => l.clone(),
        }
    }
}

#[derive(Default, Clone)]
struct Args {
    is_static: Option<Token![static]>,
    is_move: Option<Token![move]>,
    effects: Punctuated<Effect, Token![,]>,
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
        let effects = input.parse_terminated(Effect::parse, Token![,])?;
        Ok(Args { is_static, is_move, effects })
    }
}

fn parse_attrs<T: Parse + Default>(
    attrs: &mut Vec<syn::Attribute>,
    ident: &str,
) -> Option<syn::Result<T>> {
    let ret = attrs.iter().enumerate().find_map(|(index, attr)| {
        attr.path().is_ident(ident).then(|| {
            if matches!(attr.meta, syn::Meta::Path(_)) {
                Ok((index, T::default()))
            } else {
                attr.parse_args().map(|a| (index, a))
            }
        })
    });

    ret.map(|a| {
        a.map(|(index, args)| {
            attrs.remove(index);
            args
        })
    })
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

fn parse_terminated<T, P>(input: syn::parse::ParseStream) -> syn::Result<Punctuated<T, P>>
where
    T: syn::parse::Parse,
    P: syn::parse::Parse,
{
    Punctuated::parse_terminated_with(input, T::parse)
}

#[proc_macro]
#[allow(non_snake_case)]
pub fn EffectList(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match syn::parse::Parser::parse(parse_terminated::<Effect, Token![,]>, input) {
        Ok(effects) => {
            let expand_effect = crate::expr::expand_effect(effects);
            expand_effect.into_token_stream().into()
        }
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro]
#[allow_internal_unstable(coroutine_trait, coroutines, stmt_expr_attributes)]
pub fn effectful_block(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let block = parse_macro_input!(input as block::EffectfulBlock);
    block::expand_block(block).into()
}

#[proc_macro_attribute]
#[allow_internal_unstable(coroutine_trait, coroutines, stmt_expr_attributes)]
pub fn effectful(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut args = parse_macro_input!(args as Args);
    let input = parse_macro_input!(input as syn::ItemFn);
    match func::expand_func(&mut args, input) {
        Ok(output) => output.to_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_attribute]
pub fn group(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    if !args.is_empty() {
        return quote::quote!(compile_error!("arguments not be empty")).into();
    }
    let input = parse_macro_input!(input as syn::ItemTrait);
    match group::expand_group(input) {
        Ok(output) => output.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro]
#[allow_internal_unstable(coroutine_trait, coroutines, stmt_expr_attributes)]
pub fn handler(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let handlers = parse_macro_input!(input as handler::Handlers);
    handler::expand_handlers(handlers).into()
}

#[proc_macro_attribute]
#[allow_internal_unstable(
    coroutine_trait,
    coroutines,
    impl_trait_in_assoc_type,
    stmt_expr_attributes
)]
pub fn group_handler(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let break_ty = match (!args.is_empty()).then(|| syn::parse(args)).transpose() {
        Ok(ty) => ty,
        Err(e) => return e.to_compile_error().into(),
    };
    let input = parse_macro_input!(input as syn::ItemImpl);
    match group::expand_group_handler(break_ty, input) {
        Ok(output) => output.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro]
#[allow_internal_unstable(coroutine_trait, coroutines)]
pub fn catch(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let catch = parse_macro_input!(input as crate::expr::Catch);
    crate::expr::expand_catch(catch).into()
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
