use syn::spanned::Spanned;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    type Object;

    #[wasm_bindgen(constructor)]
    fn new() -> Object;

    #[wasm_bindgen(method, indexing_setter)]
    fn set(this: &Object, key: &str, value: JsValue);

    #[wasm_bindgen(method, indexing_setter)]
    fn set_i(this: &Object, key: u32, value: JsValue);
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(extends = Object)]
    type Array;

    #[wasm_bindgen(constructor)]
    fn new() -> Array;

    #[wasm_bindgen(method)]
    fn push(this: &Array, value: JsValue);
}

fn new_object_with_type(ty: &'static str) -> Object {
    let obj = Object::new();
    obj.set("_type", ty.to_js());
    obj.into()
}

macro_rules! js {
    ([$($value:expr),* $(,)?]) => {{
        let arr = Array::new();
        $(arr.push($value.to_js());)*
        JsValue::from(arr)
    }};

    ($ty:ident $(:: $variant:ident)? { $($name:ident: $value:expr),* $(,)? } $([$($item:expr),* $(,)?])?) => {{
        let obj = new_object_with_type(concat!(stringify!($ty) $(, "::", stringify!($variant))?));
        $(obj.set(stringify!($name), $value.to_js());)*
        $(
            let mut i = 0;
            $(
                obj.set_i(i, $item.to_js());
                i += 1;
            )*
            obj.set("length", i.to_js());
        )?
        JsValue::from(obj)
    }};
}

#[wasm_bindgen]
extern "C" {
    type SyntaxError;

    #[wasm_bindgen(constructor)]
    fn new(msg: &str) -> SyntaxError;

    #[wasm_bindgen(method, setter = lineNumber)]
    pub fn set_line_number(this: &SyntaxError, line: u32);
}

trait ToJS {
    fn to_js(&self) -> JsValue;
}

impl<T: ToJS> ToJS for &'_ T {
    fn to_js(&self) -> JsValue {
        (**self).to_js()
    }
}

impl<T: ToJS> ToJS for Option<T> {
    fn to_js(&self) -> JsValue {
        match self {
            Some(value) => value.to_js(),
            None => JsValue::UNDEFINED,
        }
    }
}

impl<T: ToJS> ToJS for Box<T> {
    fn to_js(&self) -> JsValue {
        (&**self).to_js()
    }
}

impl<T: ToJS> ToJS for [T] {
    fn to_js(&self) -> JsValue {
        let arr = Array::new();
        for item in self {
            arr.push(item.to_js());
        }
        arr.into()
    }
}

impl<T: ToJS> ToJS for Vec<T> {
    fn to_js(&self) -> JsValue {
        self.as_slice().to_js()
    }
}

impl ToJS for () {
    fn to_js(&self) -> JsValue {
        JsValue::UNDEFINED
    }
}

impl ToJS for bool {
    fn to_js(&self) -> JsValue {
        JsValue::from(*self)
    }
}

impl ToJS for u32 {
    fn to_js(&self) -> JsValue {
        JsValue::from(*self)
    }
}

impl ToJS for f64 {
    fn to_js(&self) -> JsValue {
        JsValue::from(*self)
    }
}

impl ToJS for u64 {
    fn to_js(&self) -> JsValue {
        // Potentially lossy if over 2^53.
        (*self as f64).to_js()
    }
}

impl ToJS for usize {
    fn to_js(&self) -> JsValue {
        (*self as f64).to_js()
    }
}

impl ToJS for str {
    fn to_js(&self) -> JsValue {
        JsValue::from_str(self)
    }
}

impl ToJS for u8 {
    fn to_js(&self) -> JsValue {
        (*self as f64).to_js()
    }
}

impl ToJS for char {
    fn to_js(&self) -> JsValue {
        let mut buf = [0; 4];
        self.encode_utf8(&mut buf).to_js()
    }
}

impl ToJS for String {
    fn to_js(&self) -> JsValue {
        self.as_str().to_js()
    }
}

impl<A: ToJS, B: ToJS> ToJS for (A, B) {
    fn to_js(&self) -> JsValue {
        js!([self.0, self.1])
    }
}

impl<A: ToJS, B: ToJS, C: ToJS> ToJS for (A, B, C) {
    fn to_js(&self) -> JsValue {
        js!([self.0, self.1, self.2])
    }
}

impl ToJS for proc_macro2::LineColumn {
    fn to_js(&self) -> JsValue {
        js!(LineColumn {
            line: self.line as u32,
            column: self.column as u32,
        })
    }
}

impl ToJS for proc_macro2::Span {
    fn to_js(&self) -> JsValue {
        js!(Span {
            start: self.start(),
            end: self.end(),
        })
    }
}

impl ToJS for proc_macro2::Ident {
    fn to_js(&self) -> JsValue {
        js!(Ident {
            to_string: self.to_string(),
            span: self.span(),
        })
    }
}

impl ToJS for proc_macro2::TokenStream {
    fn to_js(&self) -> JsValue {
        js!(TokenStream {
            to_string: self.to_string(),
            span: self.span(),
        })
    }
}

impl ToJS for proc_macro2::Literal {
    fn to_js(&self) -> JsValue {
        js!(Literal {
            to_string: self.to_string(),
            span: self.span(),
        })
    }
}

impl<T: ToJS, P> ToJS for syn::punctuated::Punctuated<T, P> {
    fn to_js(&self) -> JsValue {
        let arr = Array::new();
        for item in self {
            arr.push(item.to_js());
        }
        arr.into()
    }
}

impl ToJS for syn::token::Group {
    fn to_js(&self) -> JsValue {
        js!(Group { span: self.span })
    }
}

impl ToJS for syn::token::Paren {
    fn to_js(&self) -> JsValue {
        js!(Paren { span: self.span })
    }
}

impl ToJS for syn::token::Brace {
    fn to_js(&self) -> JsValue {
        js!(Brace { span: self.span })
    }
}

impl ToJS for syn::token::Bracket {
    fn to_js(&self) -> JsValue {
        js!(Bracket { span: self.span })
    }
}

include!(concat!(env!("OUT_DIR"), "/to_js.rs"));

impl ToJS for syn::Error {
    fn to_js(&self) -> JsValue {
        let err = SyntaxError::new(&self.to_string());
        err.set_line_number(self.span().start().line as u32);
        err.into()
    }
}

#[wasm_bindgen(js_name = "parseFile")]
pub fn parse_file(rust: &str) -> Result<JsValue, JsValue> {
    match syn::parse_file(rust) {
        Ok(ast) => Ok(ast.to_js()),
        Err(err) => Err(err.to_js()),
    }
}

#[wasm_bindgen(js_name = "parseDeriveInput")]
pub fn parse_derive_input(rust: &str) -> Result<JsValue, JsValue> {
    match syn::parse_str::<syn::DeriveInput>(rust) {
        Ok(ast) => Ok(ast.to_js()),
        Err(err) => Err(err.to_js()),
    }
}
