use std::mem;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_quote, spanned::Spanned, visit_mut::VisitMut, Error, Ident, ItemFn};

use crate::{Args, DesugarExpr};

pub fn expand_func(args: Args, mut item: ItemFn) -> TokenStream {
    let Args { is_static, is_move, mut effects } = args;
    if is_move.is_some() {
        return Error::new_spanned(is_move, "effectful functions moves by default")
            .into_compile_error();
    }

    if item.sig.asyncness.take().is_some() {
        effects.insert(1, parse_quote!(reffect::future::Await));
    }

    let ei = effects.iter();
    let resume_types: syn::Type = parse_quote!(reffect::Resumes![#(#ei),*]);

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
    let ei = effects.iter();
    item.sig.output = parse_quote!(-> impl reffect::Effectful<(#(#ei,)*), Return = #output>);

    let mut block = item.block;
    DesugarExpr { is_static: is_static.is_some() }.visit_block_mut(&mut block);
    item.block = parse_quote! {
        {
            #is_static |_: #resume_types| {
                #move_args
                #block
            }
        }
    };

    quote!(#item)
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
