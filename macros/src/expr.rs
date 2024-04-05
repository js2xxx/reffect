use std::borrow::Borrow;

use proc_macro2::Span;
use syn::{
    parse_quote, parse_quote_spanned, spanned::Spanned, Expr, Ident, Pat, Stmt, Token, Type,
};

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

pub(crate) fn expand_await(
    span: Span,
    expr: &Expr,
    is_static: bool,
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
                    let eff = match resume(#post_pin, state) {
                        core::ops::CoroutineState::Yielded(eff) => eff,
                        core::ops::CoroutineState::Complete(ret) => break ret,
                    };
                    let eff_marker = eff.type_marker();
                    state = reffect::util::narrow_effect_prefixed(
                        yield eff.broaden::<#effect_list, _>(),
                        eff_marker,
                    );
                }
            }
        })
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
