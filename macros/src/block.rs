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
            attr.path().is_ident("effectful").then(|| {
                if matches!(attr.meta, syn::Meta::Path(_)) {
                    Ok(Args::default())
                } else {
                    attr.parse_args()
                }
            })
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
            #(#stmts)*
        }
    }
}
