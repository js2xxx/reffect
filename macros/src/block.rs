use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse::Parse, visit_mut::VisitMut, Attribute, Stmt};

use crate::{Args, DesugarExpr};

pub(crate) struct EffectfulBlock {
    args: Args,
    stmts: Vec<Stmt>,
}

impl Parse for EffectfulBlock {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let attr = Attribute::parse_inner(input)?;
        let args = attr.into_iter().find_map(|attr| {
            let is_ident = attr.path().is_ident("effectful");
            is_ident.then(|| attr.parse_args_with(Args::parse))
        });

        Ok(EffectfulBlock {
            args: args.ok_or_else(|| {
                syn::Error::new(Span::call_site(), "expect a `#![effectful]` attribute")
            })??,
            stmts: syn::Block::parse_within(input)?,
        })
    }
}

pub(crate) fn expand_block(block: EffectfulBlock) -> TokenStream {
    let EffectfulBlock {
        args,
        // args_span,
        mut stmts,
    } = block;
    let Args { is_static, is_move, effects } = args;

    stmts
        .iter_mut()
        .for_each(|stmt| DesugarExpr { is_static: is_static.is_some() }.visit_stmt_mut(stmt));

    let resume_types = crate::expr::expand_resume(effects);

    quote! {
        #is_static #is_move |_: #resume_types| {
            #(#stmts)*
        }
    }
}
