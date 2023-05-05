use proc_macro2::TokenStream;
use quote::{format_ident, quote, TokenStreamExt};
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use syn_codegen::{Data, Definitions, Node, Type};

// Manual blacklist; see https://github.com/dtolnay/syn/issues/607#issuecomment-475905135.
fn has_spanned(ty: &str) -> bool {
    !matches!(
        ty,
        "DataStruct" | "DataEnum" | "DataUnion" | "QSelf" | "LocalInit"
    )
}

fn definition_tokens(definitions: &Definitions, tokens: &mut TokenStream) {
    for node in &definitions.types {
        node_tokens(node, tokens);
    }
    for syn_token in definitions.tokens.keys() {
        let ident = format_ident!("{}", syn_token);
        tokens.append_all(quote! {
            impl ToJS for syn::token::#ident {
                fn to_js(&self) -> JsValue {
                    js!(#ident {
                        span: self.span()
                    })
                }
            }
        });
    }
}

fn node_tokens(node: &Node, tokens: &mut TokenStream) {
    let ident = format_ident!("{}", node.ident);

    let data = match &node.data {
        Data::Private => match node.ident.as_str() {
            "LitStr" | "LitByteStr" | "LitByte" | "LitChar" => {
                quote! {
                    js!(#ident {
                        value: self.value(),
                        suffix: self.suffix(),
                        span: self.span()
                    })
                }
            }
            "LitInt" | "LitFloat" => {
                quote! {
                    js!(#ident {
                        digits: self.base10_digits(),
                        suffix: self.suffix(),
                        span: self.span()
                    })
                }
            }
            _ => unreachable!(),
        },
        Data::Struct(fields) => {
            let mut fields = fields.iter().collect::<Vec<_>>();

            fields.sort_by_key(|(_field, ty)| match ty {
                // Move groups down or they will be the target of any locations.
                Type::Group(_) | Type::Punctuated(_) => 2,
                // Tokens are the smallest, so make sure they're matched when searching for locations first.
                Type::Std(_) | Type::Token(_) => 0,
                // All the rest is somewhere in the middle.
                _ => 1,
            });

            let fields = fields
                .into_iter()
                .map(|(field, _ty)| {
                    let field = format_ident!("{}", field);
                    quote! {
                        #field: self.#field
                    }
                })
                .chain(if has_spanned(&node.ident) {
                    Some(quote! {
                        span: self.span()
                    })
                } else {
                    None
                });

            quote! {
                js!(#ident {
                    #(#fields,)*
                })
            }
        }
        Data::Enum(variants) => {
            let matches = variants.iter().map(|(variant, types)| {
                let variant = format_ident!("{}", variant);
                let variant = quote! {
                    #ident::#variant
                };

                let variant_path = quote! {
                    syn::#variant
                };

                match types.len() {
                    0 => quote! {
                        #variant_path => js!(#variant {})
                    },
                    1 => quote! {
                        #variant_path(x) => x.to_js()
                    },
                    len => {
                        let payload = (0..len).map(|i| format_ident!("x{}", i));
                        let payload = quote! { #(#payload),* };

                        quote! {
                            #variant_path(#payload) => js!(#variant { span: self.span() } [#payload])
                        }
                    }
                }
            });
            let wildcard = if node.exhaustive {
                None
            } else {
                Some(quote!(unreachable!()))
            };
            if matches.len() == 0 {
                quote!(#wildcard)
            } else {
                let wildcard = wildcard.map(|w| quote!(_ => #w));

                quote! {
                    match self {
                        #(#matches,)*
                        #wildcard
                    }
                }
            }
        }
    };

    tokens.append_all(quote! {
        impl ToJS for syn::#ident {
            fn to_js(&self) -> JsValue {
                #data
            }
        }
    });
}

fn main() {
    let definitions: Definitions = serde_json::from_str(include_str!("syn/syn.json")).unwrap();

    let mut generated = TokenStream::new();
    definition_tokens(&definitions, &mut generated);

    let path = &Path::new(&env::var_os("OUT_DIR").unwrap()).join("to_js.rs");

    {
        let mut out = File::create(path).unwrap();
        writeln!(out, "{}", generated).unwrap();
    }

    let _ = std::process::Command::new("rustfmt").arg(path).status();
}
