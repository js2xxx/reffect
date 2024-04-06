use std::iter;

use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parse::Parse,
    parse_quote,
    punctuated::Punctuated,
    visit::{self, Visit},
    visit_mut::{self, VisitMut},
    Expr, Ident, Lifetime, Pat, Token, Type,
};

// use crate::Args;

#[derive(Default)]
struct HandlerArgs {
    root_ident: Option<syn::PatIdent>,
    heffects: Vec<Type>,
    heffect_pats: Vec<Pat>,

    err: Option<syn::Error>,
}

impl Visit<'_> for HandlerArgs {
    fn visit_pat(&mut self, i: &'_ syn::Pat) {
        match i {
            #![cfg_attr(test, deny(non_exhaustive_omitted_patterns))]
            Pat::Ident(pi) => {
                let mut pat_ident = pi.clone();
                pat_ident.subpat = None;
                self.root_ident = Some(pat_ident);
                visit::visit_pat(self, i)
            }

            Pat::Struct(syn::PatStruct { qself, path, .. })
            | Pat::TupleStruct(syn::PatTupleStruct { qself, path, .. })
            | Pat::Path(syn::PatPath { qself, path, .. }) => {
                let ty = Type::Path(syn::TypePath {
                    qself: qself.clone(),
                    path: path.clone(),
                });
                self.heffects.push(ty);
                self.heffect_pats
                    .push(if let Some(mut pi) = self.root_ident.take() {
                        pi.subpat = Some((<Token![@]>::default(), Box::new(i.clone())));
                        Pat::Ident(pi)
                    } else {
                        i.clone()
                    });
            }

            Pat::Paren(_) => visit::visit_pat(self, i),
            Pat::Or(_) => {
                if let Some(pi) = self.root_ident.take() {
                    self.err = Some(syn::Error::new_spanned(
                        pi,
                        "root ident bindings on different effects are not supported",
                    ));
                    return;
                }
                visit::visit_pat(self, i)
            }

            Pat::Const(_)
            | Pat::Lit(_)
            | Pat::Macro(_)
            | Pat::Range(_)
            | Pat::Reference(_)
            | Pat::Rest(_)
            | Pat::Slice(_)
            | Pat::Type(_)
            | Pat::Verbatim(_)
            | Pat::Tuple(_) => {
                self.err = Some(syn::Error::new_spanned(
                    i,
                    format_args!("pattern {} are not supported", i.to_token_stream()),
                ))
            }

            Pat::Wild(_) => self.err = Some(syn::Error::new_spanned(i, "cannot infer effect type")),

            _ => visit::visit_pat(self, i),
        }
    }
}

pub struct Handler {
    // args: Option<Args>,
    root_label: Option<Lifetime>,
    hargs: HandlerArgs,
    expr: Expr,
}

impl Parse for Handler {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // let args = syn::Attribute::parse_outer(input).map(|attr| {
        //     if matches!(attr.meta, syn::Meta::Path(_)) {
        //         Ok(Args::default())
        //     } else {
        //         attr.parse_args()
        //     }
        // })??;

        let root_label: Option<Lifetime> = input.parse()?;
        if root_label.is_some() {
            let _: Token![:] = input.parse()?;
        }

        let pat = Pat::parse_multi(input)?;
        let mut hargs = HandlerArgs::default();
        hargs.visit_pat(&pat);

        if let Some(err) = hargs.err.take() {
            return Err(err);
        }

        if hargs.heffects.is_empty() {
            return Err(syn::Error::new(
                Span::call_site(),
                "cannot infer effect types; please specify at least one effect in the pattern",
            ));
        }

        let _: Token![=>] = input.parse()?;
        let expr: Expr = input.parse()?;

        Ok(Handler { root_label, hargs, expr })
    }
}

struct DesugarHandlerExpr {
    root_label: Option<Lifetime>,
}

impl VisitMut for DesugarHandlerExpr {
    fn visit_expr_mut(&mut self, i: &mut syn::Expr) {
        match i {
            Expr::Break(syn::ExprBreak { label, expr, .. })
                if *label == self.root_label || label.is_none() =>
            {
                *i = parse_quote!(return core::ops::ControlFlow::Break(#expr));
            }

            Expr::Async(_)
            | Expr::Block(_)
            | Expr::Closure(_)
            | Expr::Const(_)
            | Expr::ForLoop(_)
            | Expr::Loop(_)
            | Expr::TryBlock(_)
            | Expr::While(_) => {}

            _ => visit_mut::visit_expr_mut(self, i),
        }
    }
}

pub fn expand_handler(mut handlers: Punctuated<Handler, Token![,]>) -> proc_macro2::TokenStream {
    let base_ident = Ident::new("__effect_list", Span::call_site());

    let effect_list = crate::expr::expand_effect(handlers.iter().flat_map(|h| &h.hargs.heffects));

    let branches = handlers.iter_mut().flat_map(|handler| {
        let Handler { root_label, hargs, expr } = handler;
        let HandlerArgs { heffects, heffect_pats, .. } = hargs;

        DesugarHandlerExpr { root_label: root_label.take() }.visit_expr_mut(expr);

        let iter = heffects.iter().zip(heffect_pats).zip(iter::repeat(&*expr));
        iter.map(|((effect, pat), expr)| {
            quote! {
                let #base_ident = match #base_ident.try_unwrap::<#effect, _>() {
                    Ok(#pat) => return core::ops::ControlFlow::Continue(
                        reffect::util::Sum::new(<#effect as reffect::EffectExt>::tag(#expr))
                    ),
                    Err(rem) => rem,
                }
            }
        })
    });

    quote! {
        |#base_ident: reffect::util::Sum<#effect_list>| {
            #(#branches;)*
            #base_ident.unreachable()
        }
    }
}
