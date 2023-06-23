use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};
use quote::quote;

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
    
    //let fields = if let syn::Data::Struct(syn::DataStruct {
        //fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        //..
    //}) = ast.data
    //{ 
        //named 
    //} else {
        //unimplemented!();
    //};

    let optionized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty= &f.ty;
        quote! { #name: std::option::Option<#ty> }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty= &f.ty;
        quote! { 
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { 
            #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?,
        }
    });

    let expanded = quote! {
        pub struct #builder_ident {
            #(#optionized,)*
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
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }
        }
    };

    expanded.into()
}
