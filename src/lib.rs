use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn parse(rust: &str) -> String {
    format!("{:#?}", syn::parse_str::<syn::File>(rust))
}
