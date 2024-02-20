use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Data, DeriveInput, FieldsNamed,
    FieldsUnnamed, GenericParam, Generics,
};

#[proc_macro_derive(InterpSend)]
pub fn derive_interp_send(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    // Add a bound `T: HeapSize` to every type parameter T.
    let generics = add_trait_bounds(input.generics);

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let impl_generics = if generics.lifetimes().count() == 0 {
        quote!(<'p>)
    } else {
        quote!(#impl_generics)
    };

    let get_type = get_type(&input.ident, &input.data);
    let deserialize = deserialize(&input.ident, &input.data);
    let serialize = serialize(&input.ident, &input.data);

    let expanded = quote! {
        // The generated impl.
        impl #impl_generics crate::ffi::InterpSend<'p> for #name #ty_generics #where_clause {
            fn get_type_key() -> u128 {
                // # Safety: trust me bro
                unsafe { std::mem::transmute(std::any::TypeId::of::<#name>()) }
            }

            fn create_type(program: &mut crate::ast::Program<'p>) -> TypeId {
                #get_type
            }
            fn serialize(self, values: &mut Vec<Value>) {
                #serialize
            }
            #[allow(unused_braces)]
            fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self> {
                #deserialize
            }
        }
    };

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(expanded)
}

fn get_type(name: &Ident, data: &Data) -> TokenStream {
    match data {
        syn::Data::Struct(data) => get_type_fields(name, &data.fields),
        syn::Data::Enum(data) => {
            let recurse = data
                .variants
                .iter()
                .map(|f| get_type_fields(&f.ident, &f.fields));
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
                    (#name_str, <#ty as crate::ffi::InterpSend>::get_type(program))
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
                    <#ty as crate::ffi::InterpSend>::get_type(program)
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

fn deserialize(name: &Ident, data: &Data) -> TokenStream {
    match data {
        syn::Data::Struct(data) => {
            let rest = deserialize_fields(quote!(#name), &data.fields);
            quote!(
                #rest
            )
        }
        syn::Data::Enum(data) => {
            let recurse = data.variants.iter().enumerate().map(|(i, f)| {
                let varient = &f.ident;
                let fields = deserialize_fields(quote!(#name :: #varient), &f.fields);
                quote_spanned! {f.span()=>
                    #i => { { #fields } },
                }
            });

            quote! {
                let tag = usize::deserialize(values)?;
                match tag {
                    #(#recurse)*
                    _ => None
                }
            }
        }
        syn::Data::Union(_) => todo!(),
    }
}

fn deserialize_fields(prefix: TokenStream, fields: &syn::Fields) -> TokenStream {
    match fields {
        syn::Fields::Named(FieldsNamed {
            named: ref fields, ..
        }) => {
            let recurse = fields.iter().map(|f| {
                let name = &f.ident;
                quote_spanned! {f.span()=>
                    #name: Value::deserialize_from(values)?,
                }
            });
            quote! {
                Some(#prefix {
                    #(#recurse)*
                })
            }
        }
        syn::Fields::Unnamed(FieldsUnnamed {
            unnamed: ref fields,
            ..
        }) => {
            let recurse = fields.iter().map(|f| {
                quote_spanned! {f.span()=>
                    Value::deserialize_from(values)?,
                }
            });
            quote! {
                Some(#prefix(
                    #(#recurse)*
                ))
            }
        }
        syn::Fields::Unit => {
            quote! {
                Some(#prefix)
            }
        }
    }
}

fn serialize(name: &Ident, data: &Data) -> TokenStream {
    match data {
        syn::Data::Struct(data) => {
            let rest = serialize_fields(name, &data.fields, quote!(self.));
            quote! {
                #rest
            }
        }
        syn::Data::Enum(data) => {
            let recurse_tag = data.variants.iter().enumerate().map(|(i, f)| {
                let left = enum_match_left(&f.ident, &f.fields);
                quote_spanned! {f.span()=>
                    #left => usize::serialize(#i, values),
                }
            });
            let recurse = data.variants.iter().map(|f| {
                let left = enum_match_left(&f.ident, &f.fields);
                let (extra, prefix) = if let syn::Fields::Unnamed(unnamed) = &f.fields {
                    let recurse = unnamed.unnamed.iter().enumerate().map(|(i, f)| {
                        let ident = Ident::new(&format!("_{i}"), f.span());
                        quote_spanned! {f.span()=>
                            #ident,
                        }
                    });
                    (
                        quote! {
                            let fields = ( #(#recurse)* );
                        },
                        quote!(fields.),
                    )
                } else {
                    (quote!(), quote!())
                };
                let fields = serialize_fields(name, &f.fields, prefix);
                quote_spanned! {f.span()=>
                    #left => { #extra #fields },
                }
            });
            quote! {
                #[allow(unused_variables)]
                match &self {  #(#recurse_tag)* };
                match self {  #(#recurse)* }
                
            }
        }
        syn::Data::Union(_) => todo!(),
    }
}

fn enum_match_left(ident: &Ident, fields: &syn::Fields) -> TokenStream {
    let payload = match fields {
        syn::Fields::Named(ref fields) => {
            let recurse = fields.named.iter().map(|f| {
                let name = &f.ident;
                quote_spanned! {f.span()=>
                    #name
                }
            });
            quote! {
                { #(#recurse ,)* }
            }
        }
        syn::Fields::Unnamed(ref fields) => {
            let recurse = fields.unnamed.iter().enumerate().map(|(i, f)| {
                let ident = Ident::new(&format!("_{i}"), f.span());
                quote_spanned! {f.span()=>
                    #ident
                }
            });
            quote! {
               ( #(#recurse ,)* )
            }
        }
        syn::Fields::Unit => quote!(),
    };
    quote!(Self::#ident #payload)
}

fn serialize_fields(_name: &Ident, fields: &syn::Fields, prefix: TokenStream) -> TokenStream {
    match fields {
        syn::Fields::Named(ref fields) => {
            let recurse = fields.named.iter().map(|f| {
                let name = &f.ident;
                quote_spanned! {f.span()=>
                    crate::ffi::InterpSend::serialize(#prefix #name, values)
                }
            });
            quote! {
                #(#recurse;)*
            }
        }
        syn::Fields::Unnamed(ref fields) => {
            let recurse = fields.unnamed.iter().enumerate().map(|(i, f)| {
                let i = syn::Index::from(i);
                quote_spanned! {f.span()=>
                    crate::ffi::InterpSend::serialize(#prefix #i, values)
                }
            });
            quote! {
                #(#recurse;)*
            }
        }
        syn::Fields::Unit => quote! {},
    }
}

// Add a bound `T: HeapSize` to every type parameter T.
fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(InterpSend));
        }
    }

    generics
}

// TODO: derive debug printing with my string pool
