use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use syn::{parse_macro_input, DeriveInput};
use quote::quote;

#[proc_macro_derive(Builder, attributes(builder))]
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

    let builder_field = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if inner_type("Option", ty).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty>, }
        }
    });


    let get_each_arg = |f: &syn::Field| -> (Option<syn::Ident>, Option<proc_macro2::TokenStream>) {
        for attr in &f.attrs {
            if let syn::Meta::List( ref metalist ) = attr.meta {
                if metalist.path.segments.len() == 1 && metalist.path.segments[0].ident == "builder" {
                    let tokenstream = & mut metalist.tokens.clone().into_iter();

                    match tokenstream.next().unwrap() {
                        TokenTree::Ident(ref i) => {
                            if i != "each" {
                                let err = syn::Error::new_spanned(metalist, r#"expected `builder(each = "...")`"#).to_compile_error();
                                return (None, Some(err));
                            }
                        },
                        unknown_token => panic!("expected each, found {}", unknown_token),
                    }

                    match tokenstream.next().unwrap() {
                        TokenTree::Punct(ref p) => assert_eq!(p.as_char(), '='),
                        unknown_token => panic!("expected '=', found {}", unknown_token),
                    }

                    let literal = match tokenstream.next().unwrap() {
                        TokenTree::Literal(ref l) => l.clone(),
                        unknown_token => panic!("expected '=', found {}", unknown_token),
                    };

                    match syn::Lit::new(literal) {
                        syn::Lit::Str(s) => {
                            let arg = &f.ident;
                            let each_arg = syn::Ident::new(&s.value(), s.span());
                            let vec_inner_type = inner_type("Vec", &f.ty).unwrap();
                            let set_each_method = quote! { 
                                pub fn #each_arg(&mut self, #each_arg: #vec_inner_type) -> &mut Self {
                                    if let Some(ref mut values) = self.#arg {
                                        values.push(#each_arg);
                                    } else {
                                        self.#arg = Some(vec![#each_arg]);
                                    }
                                    self
                                }
                            };
                            return (Some(each_arg), Some(set_each_method));
                        },
                        unknown_token => panic!("expected string literal, found {:?}", unknown_token),
                    }
                }
            }
        }
        (None, None)
    };

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let original_ty = &f.ty;
        let inner_ty = inner_type("Option", original_ty);
        let set_method = if inner_ty.is_some() {
            quote! { 
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! { 
                pub fn #name(&mut self, #name: #original_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        };

        let (each_arg, set_each_method) = get_each_arg(f);

        if each_arg.is_none() && set_each_method.is_some() {
            return set_each_method.unwrap();
            //return set_method;
        } else if each_arg.is_none() {
            return set_method;
        } else {
            let conflict = &each_arg == name;
            let set_each_method = set_each_method;
            eprintln!("\nset_each_method of {:?} \n {:#?}", name, &set_each_method);
            if conflict {
                return set_each_method.unwrap();
            } else {
                return quote! {
                    #set_method
                    #set_each_method
                };
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if inner_type("Option", &ty).is_some() {
            quote! { 
                #name: self.#name.clone(),
            }
        } else {
            quote! { 
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?,
            }
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        let (each_arg, _) = get_each_arg(f);
        if each_arg.is_some() && inner_type("Vec", &f.ty).is_some() {
            quote! { 
                #name: Some(Vec::new()),
            }
        } else {
            quote! { 
                #name: None,
            }
        }
    });

    let expanded = quote! {
        pub struct #builder_ident {
            #(#builder_field)*
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

fn inner_type<'a>(wrapper_ty: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
       if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper_ty {
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
