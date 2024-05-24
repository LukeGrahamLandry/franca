use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, parse_quote, spanned::Spanned, Data, DeriveInput, FieldsNamed, FieldsUnnamed, GenericParam, Generics, TypeParamBound};

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
    let deserialize_from_ints = deserialize(&input.ident, &input.data);
    let serialize_to_ints = serialize(&input.ident, &input.data);
    let size = size_for(&input.ident, &input.data);

    let definition = get_definition(&input.data);

    let expanded = quote! {
        impl #impl_generics crate::ffi::InterpSend<'p> for #name #ty_generics #where_clause {

            const SIZE_BYTES: usize = #size;
            fn get_type_key() -> u128 {
                // # Safety: trust me bro
                unsafe { std::mem::transmute(std::any::TypeId::of::<#name>()) }
            }

            fn create_type(program: &mut crate::ast::Program<'p>) -> TypeId {
                #get_type
            }

            fn serialize_to_ints(self, values: &mut crate::bc::WriteBytes) {
                #serialize_to_ints
            }

            fn deserialize_from_ints(values: &mut crate::bc::ReadBytes) -> Option<Self> {
                #deserialize_from_ints
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
                let size = size_for_fields(&f.ident, &f.fields);
                quote_spanned! {f.span()=>
                    #i => {
                        let result: Option<Self> = #fields;
                        let size: usize = #size;
                        (result, size)
                    },
                }
            });

            let varient_count = data.variants.len();

            let t = quote!(usize::deserialize_from_ints(values)?);
            quote! {
                let tag = #t;
                let (result, varient_size): (Option<Self>, usize) = match tag {
                    #(#recurse)*
                    t => {
                        println!("err: bad enum tag {} for {} expected 0..={}", t, stringify!(#name), #varient_count);
                        return None
                    }
                };

                let payload_size = Self::SIZE_BYTES - 8;
                for _ in 0..(payload_size-varient_size) {
                    let _pad = values.next_i64()?;
                }

                result
            }
        }
        syn::Data::Union(_) => todo!(),
    }
}

fn deserialize_fields(prefix: TokenStream, fields: &syn::Fields) -> TokenStream {
    let t = quote!(crate::ffi::deserialize_from_ints(values)?);
    match fields {
        syn::Fields::Named(FieldsNamed { named: ref fields, .. }) => {
            let recurse = fields.iter().map(|f| {
                let name = &f.ident;
                quote_spanned! {f.span()=>
                    #name: #t,
                }
            });
            quote! {
                Some(#prefix {
                    #(#recurse)*
                })
            }
        }
        syn::Fields::Unnamed(FieldsUnnamed { unnamed: ref fields, .. }) => {
            let recurse = fields.iter().map(|f| {
                quote_spanned! {f.span()=>
                    #t,
                }
            });
            quote! {
                Some(#prefix(
                    #(#recurse)*
                ))
            }
        }
        syn::Fields::Unit => {
            quote! {{
                let val = #prefix;
                Some(val)
            }}
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
                let size = size_for_fields(&f.ident, &f.fields);
                let t = quote!(usize::serialize_to_ints(#i, values););
                quote_spanned! {f.span()=>
                    #left => {
                        #t
                        #size
                    },
                }
            });
            let recurse_fields = data.variants.iter().map(|f| {
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
            let pad = quote!(values.push_u8(0););
            quote! {
                #[allow(unused_variables)]
                let varient_size = match &self {  #(#recurse_tag)* };
                match self {  #(#recurse_fields)* };
                let payload_size = Self::SIZE_BYTES - 8;
                for _ in 0..(payload_size-varient_size) {
                    #pad  // TODO: less dumb padding
                }

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
    let t = quote!(crate::ffi::InterpSend::serialize_to_ints);
    match fields {
        syn::Fields::Named(ref fields) => {
            let recurse = fields.named.iter().map(|f| {
                let name = &f.ident;

                quote_spanned! {f.span()=>
                    #t(#prefix #name, values)
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
                    #t(#prefix #i, values)
                }
            });
            quote! {
                #(#recurse;)*
            }
        }
        syn::Fields::Unit => {
            quote! {}
        }
    }
}

fn size_for(name: &Ident, data: &Data) -> TokenStream {
    match data {
        syn::Data::Struct(data) => {
            let rest = size_for_fields(name, &data.fields);
            quote! {
                #rest
            }
        }
        syn::Data::Enum(data) => {
            let recurse = data.variants.iter().map(|f| {
                let fields = size_for_fields(name, &f.fields);
                quote_spanned! {f.span()=>
                    ( #fields )
                }
            });
            quote! {{
                use crate::ffi::const_max;
                const_max(&[0, #( #recurse, )* ]) + 8
            }}
        }
        syn::Data::Union(_) => todo!(),
    }
}

fn size_for_fields(_name: &Ident, fields: &syn::Fields) -> TokenStream {
    match fields {
        syn::Fields::Named(ref fields) => {
            let recurse = fields.named.iter().map(|f| {
                let ty = &f.ty;
                quote_spanned! {f.span()=>
                    <#ty as InterpSend>::SIZE_BYTES
                }
            });
            quote! {
                0 #(+ #recurse)*
            }
        }
        syn::Fields::Unnamed(ref fields) => {
            let recurse = fields.unnamed.iter().map(|f| {
                let ty = &f.ty;
                quote_spanned! {f.span()=>
                     <#ty as InterpSend>::SIZE_BYTES
                }
            });
            quote! {
                0 #(+ #recurse)*
            }
        }
        syn::Fields::Unit => quote! { 0 },
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

// TODO: derive debug printing with my string pool

#[proc_macro_derive(Reflect)]
pub fn derive_reflect(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let (fields, fields_copy) = get_field_data(&input.ident, &input.data);
    let expanded = quote! {
        impl #impl_generics crate::reflect::Reflect for #name #ty_generics #where_clause {
            const TYPE_INFO: &'static crate::reflect::RsType<'static> = &crate::reflect::RsType {
                name: stringify!(#name),
                align: std::mem::align_of::<Self>(),
                stride: std::mem::size_of::<Self>(),
                data: #fields,
                is_linear: #fields_copy,
            };
        }
    };
    proc_macro::TokenStream::from(expanded)
}

fn get_field_data(name: &Ident, data: &Data) -> (TokenStream, TokenStream) {
    match data {
        syn::Data::Struct(data) => match data.fields {
            syn::Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let f_name = &f.ident;
                    let f_ty = &f.ty;
                    quote_spanned! {f.span()=>
                        crate::reflect::RsField {
                            name: stringify!(#f_name),
                            offset: crate::reflect::field_offset!(#name, #f_name),
                            ty: <#f_ty>::get_ty,
                        }
                    }
                });
                let linear = fields.named.iter().map(|f| {
                    let ty = &f.ty;
                    quote_spanned! {f.span()=>
                        <#ty as crate::reflect::Reflect>::TYPE_INFO.is_linear
                    }
                });

                (
                    quote! {
                        {
                            const F: &[crate::reflect::RsField] = &[#(#recurse,)*];
                            crate::reflect::RsData::Struct(F)
                        }
                    },
                    quote! {
                        false #(|| #linear)*
                    },
                )
            }
            syn::Fields::Unnamed(ref _fields) => {
                todo!()
            }
            syn::Fields::Unit => (quote!(), quote!(true)), // dont care
        },
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => todo!(),
    }
}
