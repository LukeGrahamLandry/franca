use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Data, DeriveInput, GenericParam, Generics,
};

#[proc_macro_derive(InterpSend)]
pub fn derive_interp_send(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    // Add a bound `T: HeapSize` to every type parameter T.
    let generics = add_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let get_type = get_type(&input.ident, &input.data);
    let deserialize = deserialize(&input.ident, &input.data);
    let serialize = serialize(&input.ident, &input.data);

    let expanded = quote! {
        // The generated impl.
        impl<'p> #impl_generics InterpSend<'p> for #name #ty_generics #where_clause {
            fn get_type(program: &mut Program<'p>) -> TypeId {
                #get_type
            }
            fn serialize(self) -> Value {
                #serialize
            }
            fn deserialize(value: Value) -> Option<Self> {
                #deserialize
            }
        }
    };

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(expanded)
}

fn get_type(name: &Ident, data: &Data) -> TokenStream {
    match data {
        syn::Data::Struct(data) => match data.fields {
            syn::Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let ty = &f.ty;
                    let name_str = f.ident.as_ref().unwrap().to_string();
                    quote_spanned! {f.span()=>
                        (#name_str, #ty::get_type(program))
                    }
                });
                let name_str = name.to_string();
                quote! {
                    let mut fields = vec![];
                    #(fields.push(#recurse);)*
                    program.struct_type(#name_str, &fields)
                }
            }
            syn::Fields::Unnamed(_) => todo!(),
            syn::Fields::Unit => todo!(),
        },
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => todo!(),
    }
}

fn deserialize(name: &Ident, data: &Data) -> TokenStream {
    match data {
        syn::Data::Struct(data) => match data.fields {
            syn::Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let ty = &f.ty;
                    let name = &f.ident;
                    quote_spanned! {f.span()=>
                        #name: #ty::deserialize(fields.next()?)?,
                    }
                });
                quote! {
                    let mut fields = value.to_tuple()?.into_iter();
                    Some(#name {
                        #(#recurse)*
                    })
                }
            }
            syn::Fields::Unnamed(_) => todo!(),
            syn::Fields::Unit => todo!(),
        },
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => todo!(),
    }
}

fn serialize(_name: &Ident, data: &Data) -> TokenStream {
    match data {
        syn::Data::Struct(data) => match data.fields {
            syn::Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let ty = &f.ty;
                    let name = &f.ident;
                    quote_spanned! {f.span()=>
                        #ty::serialize(self.#name)
                    }
                });
                quote! {
                    let mut values = vec![];
                    #(values.push(#recurse);)*
                    Value::Tuple { values, container_type: TypeId::any() }  // TODO
                }
            }
            syn::Fields::Unnamed(_) => todo!(),
            syn::Fields::Unit => todo!(),
        },
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => todo!(),
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
