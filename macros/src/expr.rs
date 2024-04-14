use std::borrow::Borrow;

use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parse::Parse, parse_quote, parse_quote_spanned, spanned::Spanned, Expr, Ident, Pat, Stmt,
    Token, Type,
};

use crate::handler::Handler;

fn concat_list<P>(effects: impl IntoIterator<Item = P>) -> Type
where
    P: Borrow<Type>,
{
    effects.into_iter().fold(parse_quote!(()), |acc, path| {
        let path = path.borrow();
        let concat_list: Type = parse_quote! {
            reffect::util::ConcatList<#path>
        };
        parse_quote! {
            <#acc as #concat_list>::Output
        }
    })
}

pub(crate) fn expand_effect<P>(effects: impl IntoIterator<Item = P>) -> Type
where
    P: Borrow<Type>,
{
    let list = effects.into_iter().map(|path| {
        let path = path.borrow();
        let path: Type = parse_quote! { <#path as reffect::effect::EffectGroup>::Effects };
        path
    });
    concat_list(list)
}

pub(crate) fn expand_resume<P>(effects: impl IntoIterator<Item = P>) -> Type
where
    P: Borrow<Type>,
{
    let resume_ty = effects.into_iter().map(|path| {
        let path = path.borrow();
        let path: Type = parse_quote! {
            <
                <#path as reffect::effect::EffectGroup>::Effects
                    as reffect::effect::EffectList
            >::ResumeList
        };
        path
    });
    let list = concat_list(resume_ty);
    parse_quote! {
        reffect::util::Sum<(reffect::adapter::Begin, #list)>
    }
}

pub(crate) fn expand_yield(
    span: Span,
    expr: Option<Box<Expr>>,
    effect_list: Option<&Type>,
) -> Expr {
    let inferred = syn::Type::Infer(syn::TypeInfer {
        underscore_token: <Token![_]>::default(),
    });
    let effect_list = effect_list.unwrap_or(&inferred);

    let token = syn::token::Yield { span };
    let expr = expr.unwrap_or_else(|| parse_quote_spanned! { span => () });
    parse_quote! {{
        let eff = reffect::util::Sum::from(#expr);
        let marker = eff.type_marker();
        let r = reffect::util::narrow_effect(#token eff.broaden::<#effect_list, _>(), marker);
        reffect::util::untag_effect(r, marker)
    }}
}

pub(crate) fn expand_await(
    span: Span,
    expr: &Expr,
    is_static: bool,
    effect_list: Option<&Type>,
) -> Expr {
    expand_catch_expr(span, expr, &[], is_static, &mut [], true, effect_list)
}

fn expand_catch_expr(
    span: Span,
    expr: &Expr,
    attrs: &[syn::Attribute],
    is_static: bool,
    handlers: &mut [Handler],
    yield_rest: bool,
    effect_list: Option<&Type>,
) -> Expr {
    let inferred = syn::Type::Infer(syn::TypeInfer {
        underscore_token: <Token![_]>::default(),
    });
    let effect_list = effect_list.unwrap_or(&inferred);

    let full_span = span.join(expr.span()).unwrap();

    let awaitee = Ident::new("__awaitee", Span::call_site());
    let awaitee_pat: Pat = parse_quote_spanned!(full_span => #awaitee);
    let awaitee_expr: Expr = parse_quote_spanned!(span => #awaitee);

    let call: Ident = parse_quote_spanned!(span.resolved_at(Span::mixed_site()) => resume);

    let (pre_pin, post_pin): (Stmt, Expr) = if is_static {
        (
            parse_quote_spanned!(span => let mut pinned = pin!(#awaitee_expr);),
            parse_quote_spanned!(span => pinned.as_mut()),
        )
    } else {
        (
            parse_quote_spanned!(span => let _ = ();),
            parse_quote_spanned!(span => Pin::new(&mut #awaitee_expr)),
        )
    };

    let base_ident = Ident::new("__effect_list", Span::call_site());

    let heffect_list = Handler::heffect_list(handlers);
    let handler_body = if !handlers.is_empty() {
        crate::handler::expand_handler_body(
            attrs,
            handlers,
            &None,
            &base_ident,
            &heffect_list,
            &None,
            !yield_rest,
        )
    } else {
        quote!(core::ops::ControlFlow::Continue(reffect::util::narrow_effect_prefixed(
            yield #base_ident.broaden::<#effect_list, _>(),
            _eff_marker,
        )))
    };

    parse_quote! {
        (match (
            reffect::effect::IntoCoroutine::into_coroutine(#expr),
            reffect::util::Sum::new(reffect::adapter::Begin),
        ) {
            (mut #awaitee_pat, mut state) => {
                use core::pin::{Pin, pin};

                #pre_pin
                loop {
                    fn resume<R, T: core::ops::Coroutine<R>>(
                        coro: core::pin::Pin<&mut T>,
                        state: R
                    ) -> core::ops::CoroutineState<T::Yield, T::Return> {
                        T::#call(coro, state)
                    }

                    // SAFETY: `coro` won't be moved.
                    let #base_ident = match resume(#post_pin, state) {
                        core::ops::CoroutineState::Yielded(eff) => eff,
                        core::ops::CoroutineState::Complete(ret) => break ret,
                    };
                    let _eff_marker = #base_ident.type_marker();

                    state = match #handler_body {
                        core::ops::ControlFlow::Continue(state) => state.broaden(),
                        core::ops::ControlFlow::Break(ret) => break ret,
                    };
                }
            }
        })
    }
}

#[derive(Default)]
struct NonExhaustive;

impl Parse for NonExhaustive {
    fn parse(_: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(NonExhaustive)
    }
}

pub struct Catch {
    expr: syn::ExprAwait,
    is_static: bool,
    attrs: Vec<syn::Attribute>,
    handlers: Vec<Handler>,
    yield_rest: bool,
}

impl Parse for Catch {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let is_static: Option<Token![static]> = input.parse()?;
        let expr = input.parse()?;

        let content;
        syn::braced!(content in input);

        let mut attrs = syn::Attribute::parse_inner(&content)?;
        let yield_rest: Option<NonExhaustive> =
            crate::parse_attrs(&mut attrs, "non_exhaustive").transpose()?;

        let mut handlers = Vec::new();
        while !content.is_empty() {
            handlers.push(content.parse()?);
        }

        Ok(Catch {
            expr,
            is_static: is_static.is_some(),
            attrs,
            handlers,
            yield_rest: yield_rest.is_some(),
        })
    }
}

pub fn expand_catch(input: Catch) -> proc_macro2::TokenStream {
    let Catch {
        expr,
        is_static,
        attrs,
        mut handlers,
        yield_rest,
    } = input;

    expand_catch_expr(
        expr.await_token.span,
        &expr.base,
        &attrs,
        is_static,
        &mut handlers,
        yield_rest,
        None,
    )
    .to_token_stream()
}
