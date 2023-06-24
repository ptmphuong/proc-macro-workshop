use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};
use quote::quote;

fn option_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(ref p) = ty {
       if p.path.segments.len() != 1 || p.path.segments[0].ident != "Option" {
           return None;
       }

       if let syn::PathArguments::AngleBracketed(ref inner_ty_args) = p.path.segments[0].arguments {
           if inner_ty_args.args.len() != 1 {
               return None;
           } 

           let arg = inner_ty_args.args.first().unwrap();
           if let syn::GenericArgument::Type(ref inner_ty) = arg {
               return Some(inner_ty);
           }
       }
    }
    None
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    //eprintln!("{:#?}", ast);
    
    let struct_name = &ast.ident;
    let builder_name = format!("{}Builder", struct_name);
    let builder_ident = syn::Ident::new(&builder_name, struct_name.span());
    let fields = match ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
            ..
        }) => named,
        _ => unimplemented!(),
    };
    
    let ty_is_some = |ty: &syn::Type| {
        if let syn::Type::Path(ref p) = ty {
            return p.path.segments.len() == 1 && p.path.segments[0].ident == "Option";
        }
        false
    };

    let optionized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! { #name: std::option::Option<#ty>, }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let inner_ty = option_inner_type(ty);
        if ty_is_some(&ty) {
            quote! { 
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(Some(#name));
                    self
                }
            }
        } else {
            quote! { 
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_is_some(&ty) {
            quote! { 
                #name: self.#name.clone().unwrap_or_else(|| None),
            }
        } else {
            quote! { 
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?,
            }
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { 
            #name: None,
        }
    });

    let expanded = quote! {
        pub struct #builder_ident {
            #(#optionized)*
        }

        impl #builder_ident {
            #(#methods)*

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                Ok(#struct_name {
                    #(#build_fields)*
                })
            }
        }

        impl #struct_name {
            fn builder() -> #builder_ident {
                #builder_ident{
                    #(#build_empty)*
                }
            }
        }
    };

    expanded.into()
}
