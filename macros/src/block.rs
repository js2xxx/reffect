use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse::Parse, visit_mut::VisitMut, Attribute, Stmt};

use crate::{Args, DesugarExpr};

pub(crate) struct EffectfulBlock {
    args: Args,
    attrs: Vec<Attribute>,
    stmts: Vec<Stmt>,
}

impl Parse for EffectfulBlock {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut attrs = Attribute::parse_inner(input)?;
        let args = Args::parse_attrs(&mut attrs).ok_or_else(|| {
            syn::Error::new(Span::call_site(), "expect a `#![effectful]` attribute")
        })??;

        Ok(EffectfulBlock {
            args,
            attrs,
            stmts: syn::Block::parse_within(input)?,
        })
    }
}

pub(crate) fn expand_block(block: EffectfulBlock) -> TokenStream {
    let EffectfulBlock { args, attrs, mut stmts } = block;
    let Args { is_static, is_move, effects } = args;

    let effect_list = crate::expr::expand_effect(&effects);

    stmts.iter_mut().for_each(|stmt| {
        DesugarExpr {
            is_static: is_static.is_some(),
            effect_list: &effect_list,
        }
        .visit_stmt_mut(stmt)
    });

    let resume_types = crate::expr::expand_resume(effects);

    quote! {
        #is_static #is_move |_: #resume_types| {
            #(#attrs)*
            #(#stmts)*
        }
    }
}
