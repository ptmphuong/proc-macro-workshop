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

    let expanded = quote! {
        struct #builder_ident {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
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
