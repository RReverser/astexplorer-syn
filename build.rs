mod types {
    use indexmap::IndexMap;
    use serde::{Deserialize, Deserializer};

    #[derive(Debug, PartialEq, Deserialize)]
    pub struct Definitions {
        /// The Syn version used to generate the introspection file.
        pub types: Vec<Node>,
        pub tokens: IndexMap<String, String>,
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub struct Node {
        pub ident: String,
        #[serde(
            flatten,
            deserialize_with = "private_if_absent"
        )]
        pub data: Data,
    }

    #[derive(Debug, PartialEq, Deserialize)]
    pub enum Data {
        Private,
        #[serde(rename = "fields")]
        Struct(Fields),
        #[serde(rename = "variants")]
        Enum(Variants),
    }

    pub type Fields = IndexMap<String, Type>;
    pub type Variants = IndexMap<String, Vec<Type>>;

    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(rename_all = "lowercase")]
    pub enum Type {
        /// Type defined by `syn`
        Syn(String),

        /// Type defined in `std`.
        Std(String),

        /// Type external to `syn`
        #[serde(rename = "proc_macro2")]
        Ext(String),

        /// Token type
        Token(String),

        /// Token group
        Group(String),

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
    panic!("{:#?}", body);
}
