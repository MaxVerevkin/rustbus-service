use proc_macro::TokenStream;
use proc_macro_crate::FoundCrate;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Args)]
pub fn derive_args(input: TokenStream) -> TokenStream {
    let rbs_path = match proc_macro_crate::crate_name("rustbus-service") {
        Ok(FoundCrate::Itself) => quote!(crate),
        Ok(FoundCrate::Name(name)) => {
            let ident = quote::format_ident!("{name}");
            quote!(::#ident)
        }
        Err(_) => return quote!(compile_error!("failed to get rustbus_service path");).into(),
    };

    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;

    let syn::Data::Struct(s) = input.data else {
        return quote!(compile_error!("Args can be derived only for structs");).into();
    };

    if input.generics.where_clause.is_some() {
        return quote!(compile_error!("Where clause not supported yet");).into();
    }

    let mut lt_generics = input
        .generics
        .params
        .iter()
        .flat_map(|x| match x {
            syn::GenericParam::Lifetime(lt) => Some(lt),
            _ => None,
        })
        .collect::<Vec<_>>();
    if lt_generics.len() > 1 {
        return quote!(compile_error!("More than one lifetime generic not supported");).into();
    }
    if input.generics.params.len() != lt_generics.len() {
        return quote!(compile_error!("Non-lifetime gerecis not supported yet");).into();
    }
    let lt_generic = lt_generics.pop();

    let mut fields = Vec::new();
    match s.fields {
        syn::Fields::Named(named) => {
            for x in named.named.iter() {
                fields.push((x.ident.clone(), x.ty.clone()));
            }
        }
        syn::Fields::Unnamed(_) => todo!("unnamed"),
        syn::Fields::Unit => todo!("unit"),
    }

    let generics = lt_generic.map(|x| quote!(<#x>));
    let ty_generic = lt_generic.map(|_| quote!(<'__ty_a>));

    let parse = fields.iter().map(|(name, _ty)| {
        quote! {
            #name: parser.get()?,
        }
    });

    let introspect = fields.iter().map(|(name, ty)| {
        let name = name.as_ref().unwrap().to_string();
        quote! {
            buf.push(#rbs_path::MethodArgument {
                name: #name,
                is_out: false,
                signature: <#ty as #rbs_path::rustbus::Signature>::signature(),
            });
        }
    });

    let result = quote! {
        impl #generics #rbs_path::Args for #struct_name #generics {
            type Ty<'__ty_a> = #struct_name #ty_generic;

            fn parse(
                mut parser: #rbs_path::rustbus::message_builder::MessageBodyParser
            ) -> ::std::result::Result<Self::Ty<'_>, #rbs_path::rustbus::wire::errors::UnmarshalError> {
                ::std::result::Result::Ok(#struct_name {
                    #(#parse)*
                })
            }

            fn introspect(buf: &mut ::std::vec::Vec<#rbs_path::MethodArgument>) {
                #(#introspect)*
            }
        }
    };

    result.into()
}

#[proc_macro_derive(ReturnArgs)]
pub fn derive_return_args(input: TokenStream) -> TokenStream {
    let rbs_path = match proc_macro_crate::crate_name("rustbus-service") {
        Ok(FoundCrate::Itself) => quote!(crate),
        Ok(FoundCrate::Name(name)) => {
            let ident = quote::format_ident!("{name}");
            quote!(::#ident)
        }
        Err(_) => return quote!(compile_error!("failed to get rustbus_service path");).into(),
    };

    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;

    let syn::Data::Struct(s) = input.data else {
        return quote!(compile_error!("Args can be derived only for structs");).into();
    };

    if input.generics.where_clause.is_some() {
        return quote!(compile_error!("Where clause not supported yet");).into();
    }

    let mut lt_generics = input
        .generics
        .params
        .iter()
        .flat_map(|x| match x {
            syn::GenericParam::Lifetime(lt) => Some(lt),
            _ => None,
        })
        .collect::<Vec<_>>();
    if lt_generics.len() > 1 {
        return quote!(compile_error!("More than one lifetime generic not supported");).into();
    }
    if input.generics.params.len() != lt_generics.len() {
        return quote!(compile_error!("Non-lifetime gerecis not supported yet");).into();
    }
    let lt_generic = lt_generics.pop();

    let mut fields = Vec::new();
    match s.fields {
        syn::Fields::Named(named) => {
            for x in named.named.iter() {
                fields.push((x.ident.clone(), x.ty.clone()));
            }
        }
        syn::Fields::Unnamed(_) => todo!("unnamed"),
        syn::Fields::Unit => todo!("unit"),
    }

    let generics = lt_generic.map(|x| quote!(<#x>));
    let ty_generic = lt_generic.map(|_| quote!(<'__ty_a>));

    let parse = fields.iter().map(|(name, _ty)| {
        quote! {
            msg.push_param(val.#name)?;
        }
    });

    let introspect = fields.iter().map(|(name, ty)| {
        let name = name.as_ref().unwrap().to_string();
        quote! {
            buf.push(#rbs_path::MethodArgument {
                name: #name,
                is_out: true,
                signature: <#ty as #rbs_path::rustbus::Signature>::signature(),
            });
        }
    });

    let result = quote! {
        impl #generics #rbs_path::ReturnArgs for #struct_name #generics {
            type Ty<'__ty_a> = #struct_name #ty_generic;

            fn push(
                val: Self::Ty<'_>,
                msg: &mut #rbs_path::rustbus::message_builder::MarshalledMessageBody,
            ) -> ::std::result::Result<(), #rbs_path::rustbus::wire::errors::MarshalError> {
                #(#parse)*
                ::std::result::Result::Ok(())
            }

            fn introspect(buf: &mut ::std::vec::Vec<#rbs_path::MethodArgument>) {
                #(#introspect)*
            }
        }
    };

    result.into()
}
