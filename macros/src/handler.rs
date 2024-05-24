use std::iter;

use convert_case::{Case, Casing};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse::Parse,
    parse_quote,
    spanned::Spanned,
    visit::{self, Visit},
    visit_mut::{self, VisitMut},
    Expr, Ident, Lifetime, Pat, Token, Type,
};

use crate::Args;

#[derive(Default)]
struct HandlerPat {
    root_ident: Option<syn::PatIdent>,
    heffects: Vec<Type>,
    heffect_pats: Vec<Pat>,
    is_non_exhaustive: bool,

    is_in_subpat: bool,
    err: Option<syn::Error>,
}

impl HandlerPat {
    fn check_ty(&mut self, ty: &Type) -> bool {
        if self.heffects.iter().any(|d| d == ty) {
            self.err = Some(syn::Error::new_spanned(
                ty,
                "splitting the same effect type into multiple patterns is not supported",
            ));
            return false;
        }
        true
    }

    fn check_ty_with_other(&self, other: &Self) -> Option<syn::Error> {
        self.heffects.iter().find_map(|d| {
            let any = other.heffects.iter().any(|e| d == e);
            if any && !self.is_non_exhaustive && !other.is_non_exhaustive {
                return Some(syn::Error::new_spanned(
                    d,
                    "multiple exhaustive patterns on the same effect type are not supported",
                ));
            }
            None
        })
    }

    fn push_ty(&mut self, ty: Type, root_ident: Option<syn::PatIdent>, i: &syn::Pat) {
        self.heffects.push(ty);
        self.heffect_pats.push(match root_ident {
            Some(mut pi) => {
                pi.subpat = Some((<Token![@]>::default(), Box::new(i.clone())));
                Pat::Ident(pi)
            }
            None => i.clone(),
        });
    }
}

impl Visit<'_> for HandlerPat {
    fn visit_pat(&mut self, i: &'_ syn::Pat) {
        match i {
            #![cfg_attr(test, deny(non_exhaustive_omitted_patterns))]
            Pat::Ident(pi) => {
                if !self.is_in_subpat {
                    let s = pi.ident.to_string();
                    if pi.subpat.is_none() && s.is_case(Case::Pascal) {
                        let ty = Type::Path(syn::TypePath {
                            qself: None,
                            path: syn::Path::from(pi.ident.clone()),
                        });

                        if self.check_ty(&ty) {
                            let root_ident = self.root_ident.take();
                            self.push_ty(ty, root_ident, i);
                        }

                        return;
                    }

                    let mut pat_ident = pi.clone();
                    pat_ident.subpat = None;
                    self.root_ident = Some(pat_ident);
                }
                visit::visit_pat(self, i);
            }

            Pat::Struct(syn::PatStruct { qself, path, .. })
            | Pat::TupleStruct(syn::PatTupleStruct { qself, path, .. })
            | Pat::Path(syn::PatPath { qself, path, .. }) => {
                assert!(!self.is_in_subpat);

                let ty = Type::Path(syn::TypePath {
                    qself: qself.clone(),
                    path: path.clone(),
                });

                if self.check_ty(&ty) {
                    let root_ident = self.root_ident.take();

                    self.is_in_subpat = true;
                    visit::visit_pat(self, i);
                    self.is_in_subpat = false;

                    self.push_ty(ty, root_ident, i);
                }
            }

            Pat::Paren(_) => visit::visit_pat(self, i),
            Pat::Or(_) => {
                if !self.is_in_subpat {
                    if let Some(pi) = self.root_ident.take() {
                        self.err = Some(syn::Error::new_spanned(
                            pi,
                            "root ident bindings on different effects are not supported",
                        ));
                        return;
                    }
                }
                visit::visit_pat(self, i)
            }

            Pat::Const(_)
            | Pat::Lit(_)
            | Pat::Range(_)
            | Pat::Macro(_)
            | Pat::Reference(_)
            | Pat::Rest(_)
            | Pat::Slice(_)
            | Pat::Type(_)
            | Pat::Verbatim(_)
            | Pat::Tuple(_)
                if !self.is_in_subpat =>
            {
                self.err = Some(syn::Error::new_spanned(
                    i,
                    format_args!("pattern {} are not supported", i.to_token_stream()),
                ))
            }

            Pat::Const(_) | Pat::Lit(_) => {
                self.is_non_exhaustive = true;
                visit::visit_pat(self, i)
            }
            Pat::Range(syn::ExprRange { start, end, .. }) if start.is_some() || end.is_some() => {
                self.is_non_exhaustive = true;
                visit::visit_pat(self, i)
            }

            Pat::Wild(_) if !self.is_in_subpat => {
                self.err = Some(syn::Error::new_spanned(i, "cannot infer effect type"))
            }

            _ => visit::visit_pat(self, i),
        }
    }
}

pub struct Handler {
    hargs: HandlerPat,
    guard: Option<Box<Expr>>,
    expr: Box<Expr>,
}

impl Parse for Handler {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let syn::Arm { attrs, pat, guard, body, .. } = input.parse()?;

        if let (Some(first), Some(last)) = (attrs.first(), attrs.last()) {
            return Err(syn::Error::new(
                first.span().join(last.span()).unwrap(),
                "custom attributes are not supported on handler arms",
            ));
        }

        let mut hargs = HandlerPat::default();
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

        if guard.is_some() {
            hargs.is_non_exhaustive = true;
        }

        Ok(Handler {
            hargs,
            guard: guard.map(|g| g.1),
            expr: body,
        })
    }
}

impl Handler {
    pub fn heffect_list(this: &[Self]) -> syn::Type {
        let effects = this.iter().flat_map(|h| {
            if !h.hargs.is_non_exhaustive {
                &*h.hargs.heffects
            } else {
                &[]
            }
        });
        crate::expr::expand_effect(effects.map(|ty| crate::Effect::Group(ty.clone())))
    }
}

pub struct DesugarHandlerExpr<'a> {
    pub root_label: &'a Lifetime,
}

impl VisitMut for DesugarHandlerExpr<'_> {
    fn visit_expr_mut(&mut self, i: &mut syn::Expr) {
        match i {
            Expr::Break(syn::ExprBreak { label, expr, .. })
                if label.as_ref() == Some(self.root_label) || label.is_none() =>
            {
                *i = parse_quote!(break '__effectful_handler_body core::ops::ControlFlow::Break(#expr));
            }

            Expr::Return(syn::ExprReturn { expr, .. }) => {
                let root_label = self.root_label;
                let e = expr.take();
                *i = parse_quote!(break #root_label #e);
            }

            Expr::Async(_)
            | Expr::Closure(_)
            | Expr::Const(_)
            | Expr::ForLoop(_)
            | Expr::Loop(_)
            | Expr::TryBlock(_)
            | Expr::While(_) => {}

            Expr::Block(syn::ExprBlock { label, .. }) if label.is_some() => {}

            _ => visit_mut::visit_expr_mut(self, i),
        }
    }
}

#[derive(Default)]
struct HandlerArgs {
    is_move: Option<Token![move]>,
}

impl Parse for HandlerArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(HandlerArgs { is_move: input.parse()? })
    }
}

pub struct Handlers {
    hargs: HandlerArgs,
    args: Option<Args>,
    attrs: Vec<syn::Attribute>,
    root_label: Option<Lifetime>,
    handlers: Vec<Handler>,
}

impl Parse for Handlers {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut attrs = syn::Attribute::parse_inner(input)?;
        let args: Option<Args> = crate::parse_attrs(&mut attrs, "effectful").transpose()?;
        let hargs = crate::parse_attrs(&mut attrs, "handler").transpose()?;

        let root_label: Option<Lifetime> = input.parse()?;
        if root_label.is_some() {
            let _: Token![:] = input.parse()?;
        }

        let mut handlers = Vec::new();
        while !input.is_empty() {
            handlers.push(input.parse()?);
        }

        if let Some(ref args) = args {
            if args.is_static.is_some() {
                return Err(syn::Error::new_spanned(
                    args.is_static,
                    "effectful handlers are immovable by default",
                ));
            }
        }

        Ok(Handlers {
            hargs: hargs.unwrap_or_default(),
            args,
            attrs,
            root_label,
            handlers,
        })
    }
}

pub fn expand_handler_body(
    attrs: &[syn::Attribute],
    handlers: &mut [Handler],
    root_label: &Option<Lifetime>,
    base_ident: &Ident,
    continue_marker: &TokenStream,
    args: &Option<crate::Args>,
    unreachable_rest: bool,
) -> TokenStream {
    let mut effect_list = parse_quote!(_);
    let mut desugar = match args {
        Some(args) => {
            effect_list = crate::expr::expand_effect(&args.effects);
            Some(crate::DesugarExpr {
                is_static: args.is_static.is_some(),
                effect_list: &effect_list,
            })
        }
        None => None,
    };

    let slot;
    let root_label = match root_label {
        Some(label) => label,
        None => {
            slot = parse_quote!('__root_label);
            &slot
        }
    };

    let branches = handlers.iter_mut().flat_map(|handler| {
        let Handler { hargs, guard, expr } = handler;
        let HandlerPat {
            heffects,
            heffect_pats,
            is_non_exhaustive,
            ..
        } = hargs;

        DesugarHandlerExpr { root_label }.visit_expr_mut(expr);

        if let Some(ref mut desugar) = desugar {
            desugar.visit_expr_mut(expr);
        }

        let iter = (heffects.iter().zip(heffect_pats)).zip(iter::repeat((&*guard, &*expr)));
        iter.map(|((effect, pat), (guard, expr))| {
            let success = quote! {{
                #[warn(unreachable_code, clippy::diverging_sub_expression)]
                let ret = #root_label: { #expr };
                let tagged = <#effect as reffect::EffectExt>::tag(ret);
                let sum = reffect::util::Sum::new_marked(tagged, #continue_marker);
                break '__effectful_handler_body core::ops::ControlFlow::Continue(sum);
            }};
            match guard {
                Some(guard) => quote! {
                    let mut #base_ident = #base_ident;
                    #base_ident = match #base_ident.try_unwrap::<#effect, _>() {
                        #[allow(unreachable_code, clippy::diverging_sub_expression)]
                        Ok(#pat) if #guard => #success,
                        Ok(res) => reffect::util::Sum::new(res),
                        Err(rem) => rem.broaden(),
                    };
                },
                None if *is_non_exhaustive => quote! {
                    let mut #base_ident = #base_ident;
                    #base_ident = match #base_ident.try_unwrap::<#effect, _>() {
                        #[allow(unreachable_code, clippy::diverging_sub_expression)]
                        Ok(#pat) => #success,
                        Ok(res) => reffect::util::Sum::new(res),
                        Err(rem) => rem.broaden(),
                    };
                },
                None => quote! {
                    let #base_ident = match #base_ident.try_unwrap::<#effect, _>() {
                        #[allow(unreachable_code, clippy::diverging_sub_expression)]
                        Ok(#pat) => #success,
                        Err(rem) => rem,
                    };
                },
            }
        })
    });

    let rest = if unreachable_rest {
        quote! {
            let #base_ident: reffect::util::Sum<()> = #base_ident;
            #base_ident.unreachable()
        }
    } else {
        quote! {
            let _eff_marker = #base_ident.type_marker();
            core::ops::ControlFlow::Continue(reffect::util::narrow_effect_prefixed(
                yield #base_ident.broaden::<#effect_list, _>(),
                _eff_marker,
            ).broaden())
        }
    };

    quote! {'__effectful_handler_body: {
        #(#attrs)*
        #(#branches)*
        #rest
    }}
}

pub fn expand_handlers(handlers: Handlers) -> TokenStream {
    let Handlers {
        hargs,
        ref args,
        attrs,
        ref root_label,
        mut handlers,
    } = handlers;
    let HandlerArgs { is_move } = hargs;
    let base_ident = Ident::new("__effect_list", Span::call_site());

    if let Some(err) = (handlers.iter().enumerate())
        .flat_map(|(index, a)| handlers.iter().take(index).map(move |b| (a, b)))
        .find_map(|(a, b)| a.hargs.check_ty_with_other(&b.hargs))
    {
        return err.to_compile_error();
    }

    let heffect_list = Handler::heffect_list(&handlers);
    let marker = quote!(core::marker::PhantomData::<
        <#heffect_list as reffect::effect::EffectList>::ResumeList
    >);

    let body = expand_handler_body(
        &attrs,
        &mut handlers,
        root_label,
        &base_ident,
        &marker,
        args,
        true,
    );

    let body = match args {
        Some(args) => {
            let Args { is_move, effects, .. } = args;
            let resume_types = crate::expr::expand_resume(effects);

            quote!(#[coroutine] static #is_move |_: #resume_types| #body)
        }
        None => body,
    };

    quote!(#is_move |#base_ident: reffect::util::Sum<#heffect_list>| #body)
}
