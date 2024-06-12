use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, parse_quote, spanned::Spanned, Data, DeriveInput, GenericParam, Generics, TypeParamBound};

#[proc_macro_derive(InterpSend)]
pub fn derive_interp_send(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let generics = add_trait_bounds(input.generics, parse_quote!(InterpSend));

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let impl_generics = if generics.lifetimes().count() == 0 {
        quote!(<'p>)
    } else {
        quote!(#impl_generics)
    };

    let get_type = get_type(&input.ident, &input.data);
    let definition = get_definition(&input.data);

    let expanded = quote! {
        impl #impl_generics crate::ffi::InterpSend<'p> for #name #ty_generics #where_clause {

            fn get_type_key() -> u128 {
                // # Safety: trust me bro
                unsafe { std::mem::transmute(std::any::TypeId::of::<#name>()) }
            }

            fn create_type(program: &mut crate::ast::Program) -> TypeId {
                #get_type
            }

            fn definition() -> String {
                #definition.to_string()
            }

            fn name() -> String {
                stringify!(#name).to_string()
            }
        }
    };

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(expanded)
}

fn get_definition(data: &Data) -> TokenStream {
    match data {
        syn::Data::Struct(data) => get_definition_fields(&data.fields, false),
        syn::Data::Enum(data) => {
            let s = format!("@tagged(\n{})", "    {}: {},\n".repeat(data.variants.len()));
            let name_field = data.variants.iter().map(|f| {
                let name = &f.ident;
                let fields = get_definition_fields(&f.fields, true);
                quote! {
                    stringify!(#name), #fields,
                }
            });
            quote! {
                format!(#s, #(#name_field)*)
            }
        }
        syn::Data::Union(_) => todo!(),
    }
}

fn get_definition_fields(fields: &syn::Fields, in_enum: bool) -> TokenStream {
    match fields {
        syn::Fields::Named(ref fields) => {
            let s = if in_enum {
                format!("@struct({})", "{}: {}, ".repeat(fields.named.len()))
            } else {
                format!("@struct(\n{})", "    {}: {}, \n".repeat(fields.named.len()))
            };
            let name_field = fields.named.iter().map(|f| {
                let ty = &f.ty;
                let name_str = f.ident.as_ref().unwrap().to_string();
                quote! {
                    #name_str, <#ty as crate::ffi::InterpSend>::name(),
                }
            });
            quote! {
                format!(#s, #(#name_field)*)
            }
        }
        syn::Fields::Unnamed(ref fields) => {
            let s = format!("({})", "{}, ".repeat(fields.unnamed.len()));
            let name_field = fields.unnamed.iter().map(|f| {
                let ty = &f.ty;
                quote! {
                    <#ty as crate::ffi::InterpSend>::name(),
                }
            });
            quote! {
                format!(#s, #(#name_field)*)
            }
        }
        syn::Fields::Unit => {
            quote! {
                String::from("Unit")
            }
        }
    }
}
fn get_type(name: &Ident, data: &Data) -> TokenStream {
    match data {
        syn::Data::Struct(data) => get_type_fields(name, &data.fields),
        syn::Data::Enum(data) => {
            let recurse = data.variants.iter().map(|f| get_type_fields(&f.ident, &f.fields));
            let name_str = name.to_string();
            quote! {
                let mut fields = vec![];
                #(fields.push({#recurse});)*
                program.enum_type(#name_str, &fields)
            }
        }
        syn::Data::Union(_) => todo!(),
    }
}

fn get_type_fields(name: &Ident, fields: &syn::Fields) -> TokenStream {
    match fields {
        syn::Fields::Named(ref fields) => {
            let recurse = fields.named.iter().map(|f| {
                let ty = &f.ty;
                let name_str = f.ident.as_ref().unwrap().to_string();
                // <this thing> because generics and i cant put ::<this_around_them>::
                quote_spanned! {f.span()=>
                    (#name_str, <#ty as crate::ffi::InterpSend>::get_or_create_type(program))
                }
            });
            let name_str = name.to_string();
            quote! {
                let mut fields = vec![];
                #(fields.push(#recurse);)*
                program.struct_type(#name_str, &fields)
            }
        }
        syn::Fields::Unnamed(ref fields) => {
            let recurse = fields.unnamed.iter().map(|f| {
                let ty = &f.ty;
                quote_spanned! {f.span()=>
                    <#ty as crate::ffi::InterpSend>::get_or_create_type(program)
                }
            });
            let name_str = name.to_string();
            quote! {
                let mut fields = vec![];
                #(fields.push(#recurse);)*
                program.named_tuple(#name_str, fields)
            }
        }
        syn::Fields::Unit => {
            let name_str = name.to_string();
            quote! {
                program.named_tuple(#name_str, vec![])
            }
        }
    }
}

fn add_trait_bounds(mut generics: Generics, bounds: TypeParamBound) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(bounds.clone());
        }
    }

    generics
}
