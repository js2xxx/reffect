use convert_case::{Case, Casing};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_quote, parse_quote_spanned, spanned::Spanned, visit_mut::VisitMut, Ident, ItemImpl,
    ItemTrait, Signature, TraitItemFn, Type, TypePath,
};

use crate::Args;

fn expand_group_effect_ty(
    group_ident: &Ident,
    sig: &Signature,
    check_self: bool,
) -> syn::Result<Type> {
    let Signature {
        constness,
        asyncness,
        generics,
        inputs,
        ..
    } = &sig;

    if check_self
        && inputs
            .iter()
            .any(|arg| matches!(arg, syn::FnArg::Receiver(_)))
    {
        return Err(syn::Error::new_spanned(
            &sig.inputs,
            "#[group]ed effects cannot have self parameters",
        ));
    }

    if constness.is_some() {
        return Err(syn::Error::new_spanned(
            constness,
            "#[group] effects cannot be const",
        ));
    }

    if asyncness.is_some() {
        return Err(syn::Error::new_spanned(
            asyncness,
            "#[group]ed effects cannot be async",
        ));
    }

    if !generics.params.is_empty() {
        return Err(syn::Error::new_spanned(
            generics,
            "#[group]ed effects cannot have generic parameters",
        ));
    }

    let ident = format_ident!(
        "{group_ident}{}",
        sig.ident.to_string().to_case(Case::Pascal)
    );

    Ok(parse_quote!(#ident))
}

fn expand_group_effect(
    group_ident: &Ident,
    vis: &syn::Visibility,
    item: TraitItemFn,
    effects: &mut (Vec<Type>, Vec<TokenStream>),
) -> syn::Result<TokenStream> {
    let TraitItemFn { attrs, sig, default, .. } = item;

    if default.is_some() {
        return Err(syn::Error::new_spanned(
            default,
            "#[group] effects cannot have default implementations",
        ));
    }

    let inputs = &sig.inputs;
    let output_ty = match &sig.output {
        syn::ReturnType::Type(_, ty) => ty.clone(),
        _ => Box::new(parse_quote!(())),
    };

    let ty = expand_group_effect_ty(group_ident, &sig, true)?;

    let definition = quote! {
        #vis struct #ty { #inputs }

        impl reffect::Effect for #ty {
            type Resume = #output_ty;
        }
    };

    let call_args = inputs.iter().map(|arg| match arg {
        syn::FnArg::Receiver(_) => unreachable!(),
        syn::FnArg::Typed(pat_type) => &pat_type.pat,
    });
    let func = quote! {
        #(#attrs)*
        # [reffect::effectful(#ty)]
        #vis #sig { yield #ty { #(#call_args,)* } }
    };

    effects.extend([(ty, definition)]);
    Ok(func)
}

fn expand_group_effect_impl(
    args: Option<&Args>,
    self_ty: &Type,
    group_ident: &Ident,
    item: syn::ImplItemFn,
    break_ty: Option<&Type>,
    effects: &mut (Vec<Type>, Vec<crate::handler::Handler>),
) -> syn::Result<TokenStream> {
    let syn::ImplItemFn {
        attrs,
        vis: vis0,
        defaultness,
        mut sig,
        mut block,
    } = item;

    if vis0 != syn::Visibility::Inherited {
        return Err(syn::Error::new_spanned(
            vis0,
            "visibilities of #[group]ed effects impls must be inherited",
        ));
    }

    if defaultness.is_some() {
        return Err(syn::Error::new_spanned(
            defaultness,
            "#[group] effects impls cannot have specializations",
        ));
    }

    let ty = expand_group_effect_ty(group_ident, &sig, false)?;

    let slot;
    let break_ty = match break_ty {
        Some(ty) => ty,
        None => {
            slot = parse_quote!(__BREAK_TYPE);
            sig.generics
                .params
                .push(syn::GenericParam::Type(parse_quote!(#slot)));
            &slot
        }
    };

    let output_ty = match sig.output {
        syn::ReturnType::Default => parse_quote!(()),
        syn::ReturnType::Type(_, ty) => ty,
    };
    let output_ty: Box<Type> =
        Box::new(parse_quote!(core::ops::ControlFlow<#break_ty, #output_ty>));

    crate::handler::DesugarHandlerExpr { root_label: &None }.visit_block_mut(&mut block);
    let block: syn::Block = parse_quote!({ core::ops::ControlFlow::Continue(#block) });
    sig.output = parse_quote!(-> #output_ty);

    let mut func: syn::ItemFn = parse_quote! {
        #(#attrs)* #sig #block
    };

    if let Some(args) = args {
        func = crate::func::expand_func(args.clone(), func)?;
    }
    let syn::Signature { ident, inputs, .. } = &sig;

    let call_args = inputs.iter().filter_map(|arg| match arg {
        syn::FnArg::Receiver(_) => None,
        syn::FnArg::Typed(syn::PatType { pat, .. }) => Some(pat),
    });
    let call_args: Vec<_> = call_args.collect();
    let has_receiver = inputs
        .iter()
        .any(|arg| matches!(arg, syn::FnArg::Receiver(_)));
    let mut expr = if has_receiver {
        quote!(self.#ident(#(#call_args,)*))
    } else {
        quote!(<#self_ty>::#ident(#(#call_args,)*))
    };
    if args.is_some() {
        expr = quote!(#expr.await);
    }

    let arm = parse_quote! {
        #ty { #(#call_args,)* } => #expr?,
    };

    effects.extend([(ty, arm)]);
    Ok(func.to_token_stream())
}

pub fn expand_group(item: ItemTrait) -> syn::Result<TokenStream> {
    let ItemTrait {
        attrs,
        vis,
        unsafety,
        auto_token,
        ident,
        generics,
        supertraits,
        items,
        ..
    } = item;

    if auto_token.is_some() {
        return Err(syn::Error::new_spanned(
            auto_token,
            "#[group] cannot be auto",
        ));
    }

    if !generics.params.is_empty() {
        return Err(syn::Error::new_spanned(
            generics,
            "#[group] cannot have generic parameters",
        ));
    }

    let is_unsafe = unsafety.is_some();
    let unsafe_const: syn::ItemConst = parse_quote_spanned! { unsafety.span() =>
        pub const __IS_UNSAFE: bool = #is_unsafe;
    };

    let definition = quote! {
        #(#attrs)*
        #vis struct #ident #generics;
    };

    let mut effects = (Vec::new(), Vec::new());
    let iter = items.iter().map(|item| match item {
        syn::TraitItem::Fn(item) => expand_group_effect(&ident, &vis, item.clone(), &mut effects),
        _ => Ok(quote!(#item)),
    });
    let items = iter.collect::<Result<Vec<_>, _>>()?;
    let (effect_ty, effect_def) = effects;

    let super_effect_groups = supertraits
        .iter()
        .map(|st| syn::parse2::<TypePath>(st.to_token_stream()).map(Type::Path))
        .collect::<Result<Vec<_>, _>>()?;

    let effect_list = crate::expr::expand_effect(effect_ty.iter().chain(&super_effect_groups));

    let imp = quote! {
        impl #ident {
            #unsafe_const

            #(#items)*
        }

        impl reffect::effect::EffectGroup for #ident {
            type Effects = #effect_list;
        }

        #(#effect_def)*
    };

    Ok(quote!(#definition #imp))
}

pub fn expand_group_handler(break_ty: Option<Type>, item: ItemImpl) -> syn::Result<TokenStream> {
    let ItemImpl {
        mut attrs,
        defaultness,
        unsafety,
        impl_token,
        mut generics,
        trait_,
        self_ty,
        items,
        ..
    } = item;

    if defaultness.is_some() {
        return Err(syn::Error::new_spanned(
            defaultness,
            "#[group] effect handlers cannot have specializations",
        ));
    }

    let group_path = match trait_ {
        Some((None, path, _)) => path,
        Some((Some(tok), ..)) => {
            return Err(syn::Error::new_spanned(tok, "unsupported negative impl"))
        }
        None => return Err(syn::Error::new_spanned(impl_token, "expected trait impl")),
    };
    let group_ident = match group_path.segments.last() {
        Some(seg) => &seg.ident,
        None => return Err(syn::Error::new_spanned(group_path, "empty trait path")),
    };

    let args = crate::parse_attrs::<Args>(&mut attrs, "effectful").transpose()?;

    let unsafety = unsafety.is_some();
    let check_unsafety = quote! {
        const __ASSERT_MATCH_UNSAFE: () = match (#unsafety, <#group_path>::__IS_UNSAFE) {
            (false, true) => panic!("this trait handler must be unsafe"),
            (true, false) => panic!("this trait handler must not be unsafe"),
            _ => {},
        };
    };

    let mut effects = (Vec::new(), Vec::new());
    let iter = items.into_iter().map(|item| match item {
        syn::ImplItem::Fn(func) => expand_group_effect_impl(
            args.as_ref(),
            &self_ty,
            group_ident,
            func,
            break_ty.as_ref(),
            &mut effects,
        ),
        _ => Ok(quote!(#item)),
    });
    let items = iter.collect::<Result<Vec<_>, _>>()?;
    let (effect_ty, mut handlers) = effects;

    let heffect_list = crate::expr::expand_effect(&effect_ty);
    let effect_list = match &args {
        Some(args) => crate::expr::expand_effect(&args.effects),
        None => parse_quote!(()),
    };

    let impl_ = quote! {
        #(#attrs)*
        impl #generics #self_ty {
            #check_unsafety

            #(#items)*
        }
    };

    let break_ty = match break_ty {
        Some(ty) => ty,
        None => {
            let slot = parse_quote!(__BREAK_TYPE);
            generics
                .params
                .push(syn::GenericParam::Type(parse_quote!(#slot)));
            slot
        }
    };

    let base_ident = Ident::new("__effect_list", Span::call_site());

    let body = crate::handler::expand_handler_body(
        &[],
        &mut handlers,
        &None,
        &base_ident,
        &heffect_list,
        &args,
    );

    let body = match args {
        Some(args) => {
            let resume_ty = crate::expr::expand_resume(&args.effects);
            quote!(static move |_: #resume_ty| #body)
        }
        None => quote!(reffect::effect::IntoCoroutine::into_coroutine(move || #body)),
    };

    let impl_handler = quote! {
        impl #generics reffect::effect::Handler<#break_ty, #heffect_list, #effect_list> for #self_ty {
            type Handler<'a> = impl reffect::effect::Effectful<
                #effect_list,
                Return = core::ops::ControlFlow<
                    #break_ty,
                    reffect::util::Sum<<#heffect_list as reffect::effect::EffectList>::ResumeList>,
                >
            >
            where
                Self: 'a;

            fn handle(
                mut self: core::pin::Pin<&mut Self>,
                #base_ident: reffect::util::Sum<#heffect_list>,
            ) -> Self::Handler<'_> {
                #body
            }
        }
    };

    Ok(quote!(#impl_ #impl_handler))
}
