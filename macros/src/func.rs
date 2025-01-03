use std::mem;

use proc_macro2::TokenStream;
use quote::format_ident;
use syn::{Error, Ident, ItemFn, parse_quote, spanned::Spanned, visit_mut::VisitMut};

use crate::{Args, DesugarExpr};

pub fn expand_func(args: &mut Args, mut item: ItemFn) -> syn::Result<ItemFn> {
    let Args { is_static, is_move, effects } = args;
    if is_move.is_some() {
        return Err(Error::new_spanned(
            is_move,
            "effectful functions moves by default",
        ));
    }

    let async_eff = crate::Effect::Group(parse_quote!(reffect::future::Async));
    if item.sig.asyncness.take().is_some() && effects.iter().all(|e| *e != async_eff) {
        effects.insert(0, async_eff);
    }

    let resume_types = crate::expr::expand_resume(&*effects);

    let iter = item.sig.inputs.iter_mut().enumerate().map(|(i, arg)| {
        let substitute = format_ident!("arg{}", i);
        let tt: TokenStream = match arg {
            syn::FnArg::Typed(typed) => {
                let pat = mem::replace(&mut typed.pat, parse_quote!(#substitute));
                parse_quote! {
                    let #substitute = #substitute;
                    let #pat = #substitute;
                }
            }
            syn::FnArg::Receiver(_) => {
                ReplaceReceiver(substitute.clone()).visit_block_mut(&mut item.block);
                parse_quote! {
                    let #substitute = self;
                }
            }
        };
        tt
    });
    let move_args: TokenStream = iter.collect();

    let output = match item.sig.output {
        syn::ReturnType::Default => parse_quote!(()),
        syn::ReturnType::Type(_, ty) => ty,
    };

    let effect_list = crate::expr::expand_effect(&*effects);
    item.sig.output = parse_quote! {
        -> impl reffect::Effectful<#effect_list, Return = #output>
    };

    let mut block = *item.block;

    DesugarExpr {
        is_static: is_static.is_some(),
        effect_list: &effect_list,
    }
    .visit_block_mut(&mut block);

    *item.block = parse_quote! {
        {
            #[coroutine]
            #is_static move |_: #resume_types| {
                #move_args
                #block
            }
        }
    };

    Ok(item)
}

struct ReplaceReceiver(Ident);

impl syn::visit_mut::VisitMut for ReplaceReceiver {
    fn visit_ident_mut(&mut self, i: &mut proc_macro2::Ident) {
        if i == "self" {
            let mut substitute = self.0.clone();
            substitute.set_span(i.span());
            *i = substitute;
        }
    }
}
