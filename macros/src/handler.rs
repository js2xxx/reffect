use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parse::Parse,
    parse_quote,
    visit::{self, Visit},
    visit_mut::{self, VisitMut},
    Expr, Ident, Lifetime, Pat, Token, Type,
};

#[derive(Default)]
struct HandlerArgs {
    root_ident: Option<syn::PatIdent>,
    effects: Vec<Type>,
    effect_pats: Vec<Pat>,

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
                self.effects.push(ty);
                self.effect_pats
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
    root_label: Option<Lifetime>,
    args: HandlerArgs,
    expr: Expr,
}

impl Parse for Handler {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let root_label: Option<Lifetime> = input.parse()?;
        if root_label.is_some() {
            let _: Token![:] = input.parse()?;
        }

        let pat = Pat::parse_multi(input)?;
        let mut args = HandlerArgs::default();
        args.visit_pat(&pat);

        if let Some(err) = args.err.take() {
            return Err(err);
        }

        if args.effects.is_empty() {
            return Err(syn::Error::new(
                Span::call_site(),
                "cannot infer effect types; please specify at least one effect in the pattern",
            ));
        }

        let _: Token![=>] = input.parse()?;
        let expr: Expr = input.parse()?;

        Ok(Handler { root_label, args, expr })
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

pub fn expand_handler(handler: Handler) -> proc_macro2::TokenStream {
    let Handler { root_label, args, mut expr } = handler;
    let HandlerArgs { mut effects, mut effect_pats, .. } = args;

    let base_ident = Ident::new("__effect_list", Span::call_site());

    let effect_list = crate::expr::expand_effect(&effects);

    DesugarHandlerExpr { root_label }.visit_expr_mut(&mut expr);

    let last_branch = effects.pop().zip(effect_pats.pop()).map(|(effect, pat)| {
        quote! {
            let #pat = reffect::util::Sum::into_inner(#base_ident);
            core::ops::ControlFlow::Continue(
                reffect::util::Sum::new(<#effect as reffect::EffectExt>::tag(#expr))
            )
        }
    });

    let branches = effects.iter().zip(&effect_pats).map(|(effect, pat)| {
        quote! {
            let #base_ident = match #base_ident.try_unwrap::<#effect, _>() {
                Ok(#pat) => return core::ops::ControlFlow::Continue(
                    reffect::util::Sum::new(<#effect as reffect::EffectExt>::tag(#expr))
                ),
                Err(rem) => rem,
            }
        }
    });

    quote! {
        |#base_ident: reffect::util::Sum<#effect_list>| {
            #(#branches;)*
            #last_branch
        }
    }
}
