use quote::ToTokens;
use std::io::Write;

mod types {
    use indexmap::IndexMap;
    use proc_macro2::TokenStream;
    use quote::{quote, ToTokens, TokenStreamExt};
    use serde::{Deserialize, Deserializer};

    #[derive(Debug, PartialEq, Eq, Hash, Deserialize)]
    pub struct Ident(String);

    impl ToTokens for Ident {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            proc_macro2::Ident::new(&self.0, proc_macro2::Span::call_site()).to_tokens(tokens)
        }
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub struct Definitions {
        /// The Syn version used to generate the introspection file.
        pub types: Vec<Node>,
        pub tokens: IndexMap<Ident, String>,
    }

    impl ToTokens for Definitions {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.types);
            for key in self.tokens.keys() {
                let key_as_str = &key.0;

                tokens.append_all(quote! {
                    impl ToJS for syn::token::#key {
                        fn to_js(self) -> JsValue {
                            object! {
                                type: #key_as_str,
                                span: self.span()
                            }
                        }
                    }
                });
            }
        }
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub struct Node {
        pub ident: Ident,
        #[serde(flatten, deserialize_with = "private_if_absent")]
        pub data: Data,
    }

    impl ToTokens for Node {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let ident = &self.ident;
            let ident_as_str = &ident.0;

            let data = match &self.data {
                Data::Private => {
                    quote! {
                        object! { type: #ident_as_str }
                    }
                }
                Data::Struct(fields) => {
                    let fields = fields
                        .iter()
                        .filter(|(_field, ty)| match ty {
                            // Skip externals for now.
                            Type::Ext(_) => false,
                            _ => true,
                        })
                        .map(|(field, _ty)| {
                            quote! {
                                #field: self.#field
                            }
                        });

                    quote! {
                        object! {
                            type: #ident_as_str,
                            #(#fields,)*
                        }
                    }
                }
                Data::Enum(variants) => {
                    let matches = variants.iter().map(|(variant, types)| {
                        let variant_as_str = &variant.0;

                        let variant = quote! {
                            syn::#ident::#variant
                        };

                        match types.len() {
                            0 => quote! {
                               #variant => object! {
                                   type: #variant_as_str
                               }
                            },
                            1 => quote! {
                               #variant(x) => x.to_js()
                            },
                            _ => {
                                let payload = (0..types.len()).map(|i| Ident(format!("x{}", i)));
                                let payload = quote! { (#(#payload),*) };

                                quote! {
                                    #variant #payload => object! {
                                        type: #variant_as_str,
                                        values: #payload
                                    }
                                }
                            }
                        }
                    });
                    quote! {
                        match self {
                            #(#matches,)*
                        }
                    }
                }
            };

            tokens.append_all(quote! {
                impl ToJS for syn::#ident {
                    fn to_js(self) -> JsValue {
                        #data
                    }
                }
            });
        }
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub enum Data {
        Private,
        #[serde(rename = "fields")]
        Struct(Fields),
        #[serde(rename = "variants")]
        Enum(Variants),
    }

    pub type Fields = IndexMap<Ident, Type>;
    pub type Variants = IndexMap<Ident, Vec<Type>>;

    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(rename_all = "lowercase")]
    pub enum Type {
        /// Type defined by `syn`
        Syn(Ident),

        /// Type defined in `std`.
        Std(Ident),

        /// Type external to `syn`
        #[serde(rename = "proc_macro2")]
        Ext(Ident),

        /// Token type
        Token(Ident),

        /// Token group
        Group(Ident),

        /// Punctuated list
        Punctuated(Punctuated),

        Option(Box<Type>),
        Box(Box<Type>),
        Vec(Box<Type>),
        Tuple(Vec<Type>),
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub struct Punctuated {
        pub element: Box<Type>,
        pub punct: String,
    }

    fn private_if_absent<'de, D>(deserializer: D) -> Result<Data, D::Error>
    where
        D: Deserializer<'de>,
    {
        let option = Option::deserialize(deserializer)?;
        Ok(option.unwrap_or(Data::Private))
    }
}

fn main() {
    let body: types::Definitions = serde_json::from_str(include_str!("syn/syn.json")).unwrap();

    let mut out = std::fs::File::create("out.rs").unwrap();
    writeln!(out, "{}", body.into_token_stream()).unwrap();

    let _ = std::process::Command::new("rustfmt").arg("out.rs").status();
}
