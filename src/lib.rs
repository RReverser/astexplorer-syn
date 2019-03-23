use wasm_bindgen::prelude::*;
use syn::spanned::Spanned;

#[wasm_bindgen]
extern "C" {
    type Object;

    #[wasm_bindgen(constructor)]
    fn new() -> Object;

    #[wasm_bindgen(method, indexing_setter)]
    fn set(this: &Object, key: &str, value: JsValue);
}

#[wasm_bindgen]
extern "C" {
    type Array;

    #[wasm_bindgen(constructor)]
    fn new() -> Array;

    #[wasm_bindgen(method)]
    fn push(this: &Array, value: JsValue);
}

macro_rules! js {
    ([$($value:expr),* $(,)?]) => {{
        let arr = Array::new();
        $(arr.push($value.to_js());)*
        JsValue::from(arr)
    }};

    ($ty:ident { $($name:ident: $value:expr),* $(,)? }) => {{
        let obj = Object::new();
        obj.set("type", stringify!($ty).to_js());
        $(obj.set(stringify!($name), $value.to_js());)*
        JsValue::from(obj)
    }};
}

#[wasm_bindgen]
extern {
    type Error;

    #[wasm_bindgen(constructor)]
    fn new(msg: &str) -> Error;
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

impl ToJS for usize {
    fn to_js(&self) -> JsValue {
        // Potentially lossy if over 2^53.
        JsValue::from(*self as f64)
    }
}

impl ToJS for str {
    fn to_js(&self) -> JsValue {
        JsValue::from_str(self)
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

impl ToJS for proc_macro2::Ident {
    fn to_js(&self) -> JsValue {
        self.to_string().to_js()
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

include!("../out.rs");

#[wasm_bindgen]
pub fn parse(rust: &str) -> Result<JsValue, JsValue> {
    match syn::parse_str::<syn::File>(rust) {
        Ok(ast) => Ok(ast.to_js()),
        Err(err) => Err(Error::new(&err.to_string()).into()),
    }
}
