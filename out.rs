impl ToJS for syn::Abi {
    fn to_js(&self) -> JsValue {
        object! { type : "Abi" , extern_token : self . extern_token , name : self . name , }
    }
}
impl ToJS for syn::AngleBracketedGenericArguments {
    fn to_js(&self) -> JsValue {
        object! { type : "AngleBracketedGenericArguments" , colon2_token : self . colon2_token , lt_token : self . lt_token , args : self . args , gt_token : self . gt_token , }
    }
}
impl ToJS for syn::ArgCaptured {
    fn to_js(&self) -> JsValue {
        object! { type : "ArgCaptured" , pat : self . pat , colon_token : self . colon_token , ty : self . ty , }
    }
}
impl ToJS for syn::ArgSelf {
    fn to_js(&self) -> JsValue {
        object! { type : "ArgSelf" , mutability : self . mutability , self_token : self . self_token , }
    }
}
impl ToJS for syn::ArgSelfRef {
    fn to_js(&self) -> JsValue {
        object! { type : "ArgSelfRef" , and_token : self . and_token , lifetime : self . lifetime , mutability : self . mutability , self_token : self . self_token , }
    }
}
impl ToJS for syn::Arm {
    fn to_js(&self) -> JsValue {
        object! { type : "Arm" , attrs : self . attrs , leading_vert : self . leading_vert , pats : self . pats , guard : self . guard , fat_arrow_token : self . fat_arrow_token , body : self . body , comma : self . comma , }
    }
}
impl ToJS for syn::AttrStyle {
    fn to_js(&self) -> JsValue {
        match self {
            syn::AttrStyle::Outer => object! { type : "Outer" },
            syn::AttrStyle::Inner(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::Attribute {
    fn to_js(&self) -> JsValue {
        object! { type : "Attribute" , pound_token : self . pound_token , style : self . style , bracket_token : self . bracket_token , path : self . path , }
    }
}
impl ToJS for syn::BareFnArg {
    fn to_js(&self) -> JsValue {
        object! { type : "BareFnArg" , name : self . name , ty : self . ty , }
    }
}
impl ToJS for syn::BareFnArgName {
    fn to_js(&self) -> JsValue {
        match self {
            syn::BareFnArgName::Named(x) => x.to_js(),
            syn::BareFnArgName::Wild(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::BinOp {
    fn to_js(&self) -> JsValue {
        match self {
            syn::BinOp::Add(x) => x.to_js(),
            syn::BinOp::Sub(x) => x.to_js(),
            syn::BinOp::Mul(x) => x.to_js(),
            syn::BinOp::Div(x) => x.to_js(),
            syn::BinOp::Rem(x) => x.to_js(),
            syn::BinOp::And(x) => x.to_js(),
            syn::BinOp::Or(x) => x.to_js(),
            syn::BinOp::BitXor(x) => x.to_js(),
            syn::BinOp::BitAnd(x) => x.to_js(),
            syn::BinOp::BitOr(x) => x.to_js(),
            syn::BinOp::Shl(x) => x.to_js(),
            syn::BinOp::Shr(x) => x.to_js(),
            syn::BinOp::Eq(x) => x.to_js(),
            syn::BinOp::Lt(x) => x.to_js(),
            syn::BinOp::Le(x) => x.to_js(),
            syn::BinOp::Ne(x) => x.to_js(),
            syn::BinOp::Ge(x) => x.to_js(),
            syn::BinOp::Gt(x) => x.to_js(),
            syn::BinOp::AddEq(x) => x.to_js(),
            syn::BinOp::SubEq(x) => x.to_js(),
            syn::BinOp::MulEq(x) => x.to_js(),
            syn::BinOp::DivEq(x) => x.to_js(),
            syn::BinOp::RemEq(x) => x.to_js(),
            syn::BinOp::BitXorEq(x) => x.to_js(),
            syn::BinOp::BitAndEq(x) => x.to_js(),
            syn::BinOp::BitOrEq(x) => x.to_js(),
            syn::BinOp::ShlEq(x) => x.to_js(),
            syn::BinOp::ShrEq(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::Binding {
    fn to_js(&self) -> JsValue {
        object! { type : "Binding" , eq_token : self . eq_token , ty : self . ty , }
    }
}
impl ToJS for syn::Block {
    fn to_js(&self) -> JsValue {
        object! { type : "Block" , brace_token : self . brace_token , stmts : self . stmts , }
    }
}
impl ToJS for syn::BoundLifetimes {
    fn to_js(&self) -> JsValue {
        object! { type : "BoundLifetimes" , for_token : self . for_token , lt_token : self . lt_token , lifetimes : self . lifetimes , gt_token : self . gt_token , }
    }
}
impl ToJS for syn::ConstParam {
    fn to_js(&self) -> JsValue {
        object! { type : "ConstParam" , attrs : self . attrs , const_token : self . const_token , colon_token : self . colon_token , ty : self . ty , eq_token : self . eq_token , default : self . default , }
    }
}
impl ToJS for syn::Constraint {
    fn to_js(&self) -> JsValue {
        object! { type : "Constraint" , colon_token : self . colon_token , bounds : self . bounds , }
    }
}
impl ToJS for syn::Data {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Data::Struct(x) => x.to_js(),
            syn::Data::Enum(x) => x.to_js(),
            syn::Data::Union(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::DataEnum {
    fn to_js(&self) -> JsValue {
        object! { type : "DataEnum" , enum_token : self . enum_token , brace_token : self . brace_token , variants : self . variants , }
    }
}
impl ToJS for syn::DataStruct {
    fn to_js(&self) -> JsValue {
        object! { type : "DataStruct" , struct_token : self . struct_token , fields : self . fields , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::DataUnion {
    fn to_js(&self) -> JsValue {
        object! { type : "DataUnion" , union_token : self . union_token , fields : self . fields , }
    }
}
impl ToJS for syn::DeriveInput {
    fn to_js(&self) -> JsValue {
        object! { type : "DeriveInput" , attrs : self . attrs , vis : self . vis , generics : self . generics , data : self . data , }
    }
}
impl ToJS for syn::Expr {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Expr::Box(x) => x.to_js(),
            syn::Expr::InPlace(x) => x.to_js(),
            syn::Expr::Array(x) => x.to_js(),
            syn::Expr::Call(x) => x.to_js(),
            syn::Expr::MethodCall(x) => x.to_js(),
            syn::Expr::Tuple(x) => x.to_js(),
            syn::Expr::Binary(x) => x.to_js(),
            syn::Expr::Unary(x) => x.to_js(),
            syn::Expr::Lit(x) => x.to_js(),
            syn::Expr::Cast(x) => x.to_js(),
            syn::Expr::Type(x) => x.to_js(),
            syn::Expr::Let(x) => x.to_js(),
            syn::Expr::If(x) => x.to_js(),
            syn::Expr::While(x) => x.to_js(),
            syn::Expr::ForLoop(x) => x.to_js(),
            syn::Expr::Loop(x) => x.to_js(),
            syn::Expr::Match(x) => x.to_js(),
            syn::Expr::Closure(x) => x.to_js(),
            syn::Expr::Unsafe(x) => x.to_js(),
            syn::Expr::Block(x) => x.to_js(),
            syn::Expr::Assign(x) => x.to_js(),
            syn::Expr::AssignOp(x) => x.to_js(),
            syn::Expr::Field(x) => x.to_js(),
            syn::Expr::Index(x) => x.to_js(),
            syn::Expr::Range(x) => x.to_js(),
            syn::Expr::Path(x) => x.to_js(),
            syn::Expr::Reference(x) => x.to_js(),
            syn::Expr::Break(x) => x.to_js(),
            syn::Expr::Continue(x) => x.to_js(),
            syn::Expr::Return(x) => x.to_js(),
            syn::Expr::Macro(x) => x.to_js(),
            syn::Expr::Struct(x) => x.to_js(),
            syn::Expr::Repeat(x) => x.to_js(),
            syn::Expr::Paren(x) => x.to_js(),
            syn::Expr::Group(x) => x.to_js(),
            syn::Expr::Try(x) => x.to_js(),
            syn::Expr::Async(x) => x.to_js(),
            syn::Expr::TryBlock(x) => x.to_js(),
            syn::Expr::Yield(x) => x.to_js(),
            syn::Expr::Verbatim(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::ExprArray {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprArray" , attrs : self . attrs , bracket_token : self . bracket_token , elems : self . elems , }
    }
}
impl ToJS for syn::ExprAssign {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprAssign" , attrs : self . attrs , left : self . left , eq_token : self . eq_token , right : self . right , }
    }
}
impl ToJS for syn::ExprAssignOp {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprAssignOp" , attrs : self . attrs , left : self . left , op : self . op , right : self . right , }
    }
}
impl ToJS for syn::ExprAsync {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprAsync" , attrs : self . attrs , async_token : self . async_token , capture : self . capture , block : self . block , }
    }
}
impl ToJS for syn::ExprBinary {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprBinary" , attrs : self . attrs , left : self . left , op : self . op , right : self . right , }
    }
}
impl ToJS for syn::ExprBlock {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprBlock" , attrs : self . attrs , label : self . label , block : self . block , }
    }
}
impl ToJS for syn::ExprBox {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprBox" , attrs : self . attrs , box_token : self . box_token , expr : self . expr , }
    }
}
impl ToJS for syn::ExprBreak {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprBreak" , attrs : self . attrs , break_token : self . break_token , label : self . label , expr : self . expr , }
    }
}
impl ToJS for syn::ExprCall {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprCall" , attrs : self . attrs , func : self . func , paren_token : self . paren_token , args : self . args , }
    }
}
impl ToJS for syn::ExprCast {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprCast" , attrs : self . attrs , expr : self . expr , as_token : self . as_token , ty : self . ty , }
    }
}
impl ToJS for syn::ExprClosure {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprClosure" , attrs : self . attrs , asyncness : self . asyncness , movability : self . movability , capture : self . capture , or1_token : self . or1_token , inputs : self . inputs , or2_token : self . or2_token , output : self . output , body : self . body , }
    }
}
impl ToJS for syn::ExprContinue {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprContinue" , attrs : self . attrs , continue_token : self . continue_token , label : self . label , }
    }
}
impl ToJS for syn::ExprField {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprField" , attrs : self . attrs , base : self . base , dot_token : self . dot_token , member : self . member , }
    }
}
impl ToJS for syn::ExprForLoop {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprForLoop" , attrs : self . attrs , label : self . label , for_token : self . for_token , pat : self . pat , in_token : self . in_token , expr : self . expr , body : self . body , }
    }
}
impl ToJS for syn::ExprGroup {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprGroup" , attrs : self . attrs , group_token : self . group_token , expr : self . expr , }
    }
}
impl ToJS for syn::ExprIf {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprIf" , attrs : self . attrs , if_token : self . if_token , cond : self . cond , then_branch : self . then_branch , else_branch : self . else_branch , }
    }
}
impl ToJS for syn::ExprInPlace {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprInPlace" , attrs : self . attrs , place : self . place , arrow_token : self . arrow_token , value : self . value , }
    }
}
impl ToJS for syn::ExprIndex {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprIndex" , attrs : self . attrs , expr : self . expr , bracket_token : self . bracket_token , index : self . index , }
    }
}
impl ToJS for syn::ExprLet {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprLet" , attrs : self . attrs , let_token : self . let_token , pats : self . pats , eq_token : self . eq_token , expr : self . expr , }
    }
}
impl ToJS for syn::ExprLit {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprLit" , attrs : self . attrs , lit : self . lit , }
    }
}
impl ToJS for syn::ExprLoop {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprLoop" , attrs : self . attrs , label : self . label , loop_token : self . loop_token , body : self . body , }
    }
}
impl ToJS for syn::ExprMacro {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprMacro" , attrs : self . attrs , mac : self . mac , }
    }
}
impl ToJS for syn::ExprMatch {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprMatch" , attrs : self . attrs , match_token : self . match_token , expr : self . expr , brace_token : self . brace_token , arms : self . arms , }
    }
}
impl ToJS for syn::ExprMethodCall {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprMethodCall" , attrs : self . attrs , receiver : self . receiver , dot_token : self . dot_token , turbofish : self . turbofish , paren_token : self . paren_token , args : self . args , }
    }
}
impl ToJS for syn::ExprParen {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprParen" , attrs : self . attrs , paren_token : self . paren_token , expr : self . expr , }
    }
}
impl ToJS for syn::ExprPath {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprPath" , attrs : self . attrs , qself : self . qself , path : self . path , }
    }
}
impl ToJS for syn::ExprRange {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprRange" , attrs : self . attrs , from : self . from , limits : self . limits , to : self . to , }
    }
}
impl ToJS for syn::ExprReference {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprReference" , attrs : self . attrs , and_token : self . and_token , mutability : self . mutability , expr : self . expr , }
    }
}
impl ToJS for syn::ExprRepeat {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprRepeat" , attrs : self . attrs , bracket_token : self . bracket_token , expr : self . expr , semi_token : self . semi_token , len : self . len , }
    }
}
impl ToJS for syn::ExprReturn {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprReturn" , attrs : self . attrs , return_token : self . return_token , expr : self . expr , }
    }
}
impl ToJS for syn::ExprStruct {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprStruct" , attrs : self . attrs , path : self . path , brace_token : self . brace_token , fields : self . fields , dot2_token : self . dot2_token , rest : self . rest , }
    }
}
impl ToJS for syn::ExprTry {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprTry" , attrs : self . attrs , expr : self . expr , question_token : self . question_token , }
    }
}
impl ToJS for syn::ExprTryBlock {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprTryBlock" , attrs : self . attrs , try_token : self . try_token , block : self . block , }
    }
}
impl ToJS for syn::ExprTuple {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprTuple" , attrs : self . attrs , paren_token : self . paren_token , elems : self . elems , }
    }
}
impl ToJS for syn::ExprType {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprType" , attrs : self . attrs , expr : self . expr , colon_token : self . colon_token , ty : self . ty , }
    }
}
impl ToJS for syn::ExprUnary {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprUnary" , attrs : self . attrs , op : self . op , expr : self . expr , }
    }
}
impl ToJS for syn::ExprUnsafe {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprUnsafe" , attrs : self . attrs , unsafe_token : self . unsafe_token , block : self . block , }
    }
}
impl ToJS for syn::ExprVerbatim {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprVerbatim" , }
    }
}
impl ToJS for syn::ExprWhile {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprWhile" , attrs : self . attrs , label : self . label , while_token : self . while_token , cond : self . cond , body : self . body , }
    }
}
impl ToJS for syn::ExprYield {
    fn to_js(&self) -> JsValue {
        object! { type : "ExprYield" , attrs : self . attrs , yield_token : self . yield_token , expr : self . expr , }
    }
}
impl ToJS for syn::Field {
    fn to_js(&self) -> JsValue {
        object! { type : "Field" , attrs : self . attrs , vis : self . vis , ident : self . ident , colon_token : self . colon_token , ty : self . ty , }
    }
}
impl ToJS for syn::FieldPat {
    fn to_js(&self) -> JsValue {
        object! { type : "FieldPat" , attrs : self . attrs , member : self . member , colon_token : self . colon_token , pat : self . pat , }
    }
}
impl ToJS for syn::FieldValue {
    fn to_js(&self) -> JsValue {
        object! { type : "FieldValue" , attrs : self . attrs , member : self . member , colon_token : self . colon_token , expr : self . expr , }
    }
}
impl ToJS for syn::Fields {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Fields::Named(x) => x.to_js(),
            syn::Fields::Unnamed(x) => x.to_js(),
            syn::Fields::Unit => object! { type : "Unit" },
        }
    }
}
impl ToJS for syn::FieldsNamed {
    fn to_js(&self) -> JsValue {
        object! { type : "FieldsNamed" , brace_token : self . brace_token , named : self . named , }
    }
}
impl ToJS for syn::FieldsUnnamed {
    fn to_js(&self) -> JsValue {
        object! { type : "FieldsUnnamed" , paren_token : self . paren_token , unnamed : self . unnamed , }
    }
}
impl ToJS for syn::File {
    fn to_js(&self) -> JsValue {
        object! { type : "File" , shebang : self . shebang , attrs : self . attrs , items : self . items , }
    }
}
impl ToJS for syn::FnArg {
    fn to_js(&self) -> JsValue {
        match self {
            syn::FnArg::SelfRef(x) => x.to_js(),
            syn::FnArg::SelfValue(x) => x.to_js(),
            syn::FnArg::Captured(x) => x.to_js(),
            syn::FnArg::Inferred(x) => x.to_js(),
            syn::FnArg::Ignored(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::FnDecl {
    fn to_js(&self) -> JsValue {
        object! { type : "FnDecl" , fn_token : self . fn_token , generics : self . generics , paren_token : self . paren_token , inputs : self . inputs , variadic : self . variadic , output : self . output , }
    }
}
impl ToJS for syn::ForeignItem {
    fn to_js(&self) -> JsValue {
        match self {
            syn::ForeignItem::Fn(x) => x.to_js(),
            syn::ForeignItem::Static(x) => x.to_js(),
            syn::ForeignItem::Type(x) => x.to_js(),
            syn::ForeignItem::Macro(x) => x.to_js(),
            syn::ForeignItem::Verbatim(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::ForeignItemFn {
    fn to_js(&self) -> JsValue {
        object! { type : "ForeignItemFn" , attrs : self . attrs , vis : self . vis , decl : self . decl , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ForeignItemMacro {
    fn to_js(&self) -> JsValue {
        object! { type : "ForeignItemMacro" , attrs : self . attrs , mac : self . mac , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ForeignItemStatic {
    fn to_js(&self) -> JsValue {
        object! { type : "ForeignItemStatic" , attrs : self . attrs , vis : self . vis , static_token : self . static_token , mutability : self . mutability , colon_token : self . colon_token , ty : self . ty , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ForeignItemType {
    fn to_js(&self) -> JsValue {
        object! { type : "ForeignItemType" , attrs : self . attrs , vis : self . vis , type_token : self . type_token , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ForeignItemVerbatim {
    fn to_js(&self) -> JsValue {
        object! { type : "ForeignItemVerbatim" , }
    }
}
impl ToJS for syn::GenericArgument {
    fn to_js(&self) -> JsValue {
        match self {
            syn::GenericArgument::Lifetime(x) => x.to_js(),
            syn::GenericArgument::Type(x) => x.to_js(),
            syn::GenericArgument::Binding(x) => x.to_js(),
            syn::GenericArgument::Constraint(x) => x.to_js(),
            syn::GenericArgument::Const(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::GenericMethodArgument {
    fn to_js(&self) -> JsValue {
        match self {
            syn::GenericMethodArgument::Type(x) => x.to_js(),
            syn::GenericMethodArgument::Const(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::GenericParam {
    fn to_js(&self) -> JsValue {
        match self {
            syn::GenericParam::Type(x) => x.to_js(),
            syn::GenericParam::Lifetime(x) => x.to_js(),
            syn::GenericParam::Const(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::Generics {
    fn to_js(&self) -> JsValue {
        object! { type : "Generics" , lt_token : self . lt_token , params : self . params , gt_token : self . gt_token , where_clause : self . where_clause , }
    }
}
impl ToJS for syn::ImplItem {
    fn to_js(&self) -> JsValue {
        match self {
            syn::ImplItem::Const(x) => x.to_js(),
            syn::ImplItem::Method(x) => x.to_js(),
            syn::ImplItem::Type(x) => x.to_js(),
            syn::ImplItem::Existential(x) => x.to_js(),
            syn::ImplItem::Macro(x) => x.to_js(),
            syn::ImplItem::Verbatim(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::ImplItemConst {
    fn to_js(&self) -> JsValue {
        object! { type : "ImplItemConst" , attrs : self . attrs , vis : self . vis , defaultness : self . defaultness , const_token : self . const_token , colon_token : self . colon_token , ty : self . ty , eq_token : self . eq_token , expr : self . expr , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ImplItemExistential {
    fn to_js(&self) -> JsValue {
        object! { type : "ImplItemExistential" , attrs : self . attrs , existential_token : self . existential_token , type_token : self . type_token , generics : self . generics , colon_token : self . colon_token , bounds : self . bounds , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ImplItemMacro {
    fn to_js(&self) -> JsValue {
        object! { type : "ImplItemMacro" , attrs : self . attrs , mac : self . mac , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ImplItemMethod {
    fn to_js(&self) -> JsValue {
        object! { type : "ImplItemMethod" , attrs : self . attrs , vis : self . vis , defaultness : self . defaultness , sig : self . sig , block : self . block , }
    }
}
impl ToJS for syn::ImplItemType {
    fn to_js(&self) -> JsValue {
        object! { type : "ImplItemType" , attrs : self . attrs , vis : self . vis , defaultness : self . defaultness , type_token : self . type_token , generics : self . generics , eq_token : self . eq_token , ty : self . ty , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ImplItemVerbatim {
    fn to_js(&self) -> JsValue {
        object! { type : "ImplItemVerbatim" , }
    }
}
impl ToJS for syn::Index {
    fn to_js(&self) -> JsValue {
        object! { type : "Index" , index : self . index , }
    }
}
impl ToJS for syn::Item {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Item::ExternCrate(x) => x.to_js(),
            syn::Item::Use(x) => x.to_js(),
            syn::Item::Static(x) => x.to_js(),
            syn::Item::Const(x) => x.to_js(),
            syn::Item::Fn(x) => x.to_js(),
            syn::Item::Mod(x) => x.to_js(),
            syn::Item::ForeignMod(x) => x.to_js(),
            syn::Item::Type(x) => x.to_js(),
            syn::Item::Existential(x) => x.to_js(),
            syn::Item::Struct(x) => x.to_js(),
            syn::Item::Enum(x) => x.to_js(),
            syn::Item::Union(x) => x.to_js(),
            syn::Item::Trait(x) => x.to_js(),
            syn::Item::TraitAlias(x) => x.to_js(),
            syn::Item::Impl(x) => x.to_js(),
            syn::Item::Macro(x) => x.to_js(),
            syn::Item::Macro2(x) => x.to_js(),
            syn::Item::Verbatim(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::ItemConst {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemConst" , attrs : self . attrs , vis : self . vis , const_token : self . const_token , colon_token : self . colon_token , ty : self . ty , eq_token : self . eq_token , expr : self . expr , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ItemEnum {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemEnum" , attrs : self . attrs , vis : self . vis , enum_token : self . enum_token , generics : self . generics , brace_token : self . brace_token , variants : self . variants , }
    }
}
impl ToJS for syn::ItemExistential {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemExistential" , attrs : self . attrs , vis : self . vis , existential_token : self . existential_token , type_token : self . type_token , generics : self . generics , colon_token : self . colon_token , bounds : self . bounds , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ItemExternCrate {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemExternCrate" , attrs : self . attrs , vis : self . vis , extern_token : self . extern_token , crate_token : self . crate_token , rename : self . rename , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ItemFn {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemFn" , attrs : self . attrs , vis : self . vis , constness : self . constness , unsafety : self . unsafety , asyncness : self . asyncness , abi : self . abi , decl : self . decl , block : self . block , }
    }
}
impl ToJS for syn::ItemForeignMod {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemForeignMod" , attrs : self . attrs , abi : self . abi , brace_token : self . brace_token , items : self . items , }
    }
}
impl ToJS for syn::ItemImpl {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemImpl" , attrs : self . attrs , defaultness : self . defaultness , unsafety : self . unsafety , impl_token : self . impl_token , generics : self . generics , trait_ : self . trait_ , self_ty : self . self_ty , brace_token : self . brace_token , items : self . items , }
    }
}
impl ToJS for syn::ItemMacro {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemMacro" , attrs : self . attrs , ident : self . ident , mac : self . mac , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ItemMacro2 {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemMacro2" , attrs : self . attrs , vis : self . vis , macro_token : self . macro_token , paren_token : self . paren_token , brace_token : self . brace_token , }
    }
}
impl ToJS for syn::ItemMod {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemMod" , attrs : self . attrs , vis : self . vis , mod_token : self . mod_token , content : self . content , semi : self . semi , }
    }
}
impl ToJS for syn::ItemStatic {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemStatic" , attrs : self . attrs , vis : self . vis , static_token : self . static_token , mutability : self . mutability , colon_token : self . colon_token , ty : self . ty , eq_token : self . eq_token , expr : self . expr , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ItemStruct {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemStruct" , attrs : self . attrs , vis : self . vis , struct_token : self . struct_token , generics : self . generics , fields : self . fields , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ItemTrait {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemTrait" , attrs : self . attrs , vis : self . vis , unsafety : self . unsafety , auto_token : self . auto_token , trait_token : self . trait_token , generics : self . generics , colon_token : self . colon_token , supertraits : self . supertraits , brace_token : self . brace_token , items : self . items , }
    }
}
impl ToJS for syn::ItemTraitAlias {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemTraitAlias" , attrs : self . attrs , vis : self . vis , trait_token : self . trait_token , generics : self . generics , eq_token : self . eq_token , bounds : self . bounds , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ItemType {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemType" , attrs : self . attrs , vis : self . vis , type_token : self . type_token , generics : self . generics , eq_token : self . eq_token , ty : self . ty , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ItemUnion {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemUnion" , attrs : self . attrs , vis : self . vis , union_token : self . union_token , generics : self . generics , fields : self . fields , }
    }
}
impl ToJS for syn::ItemUse {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemUse" , attrs : self . attrs , vis : self . vis , use_token : self . use_token , leading_colon : self . leading_colon , tree : self . tree , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::ItemVerbatim {
    fn to_js(&self) -> JsValue {
        object! { type : "ItemVerbatim" , }
    }
}
impl ToJS for syn::Label {
    fn to_js(&self) -> JsValue {
        object! { type : "Label" , name : self . name , colon_token : self . colon_token , }
    }
}
impl ToJS for syn::Lifetime {
    fn to_js(&self) -> JsValue {
        object! { type : "Lifetime" , }
    }
}
impl ToJS for syn::LifetimeDef {
    fn to_js(&self) -> JsValue {
        object! { type : "LifetimeDef" , attrs : self . attrs , lifetime : self . lifetime , colon_token : self . colon_token , bounds : self . bounds , }
    }
}
impl ToJS for syn::Lit {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Lit::Str(x) => x.to_js(),
            syn::Lit::ByteStr(x) => x.to_js(),
            syn::Lit::Byte(x) => x.to_js(),
            syn::Lit::Char(x) => x.to_js(),
            syn::Lit::Int(x) => x.to_js(),
            syn::Lit::Float(x) => x.to_js(),
            syn::Lit::Bool(x) => x.to_js(),
            syn::Lit::Verbatim(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::LitBool {
    fn to_js(&self) -> JsValue {
        object! { type : "LitBool" , value : self . value , }
    }
}
impl ToJS for syn::LitByte {
    fn to_js(&self) -> JsValue {
        object! { type : "LitByte" }
    }
}
impl ToJS for syn::LitByteStr {
    fn to_js(&self) -> JsValue {
        object! { type : "LitByteStr" }
    }
}
impl ToJS for syn::LitChar {
    fn to_js(&self) -> JsValue {
        object! { type : "LitChar" }
    }
}
impl ToJS for syn::LitFloat {
    fn to_js(&self) -> JsValue {
        object! { type : "LitFloat" }
    }
}
impl ToJS for syn::LitInt {
    fn to_js(&self) -> JsValue {
        object! { type : "LitInt" }
    }
}
impl ToJS for syn::LitStr {
    fn to_js(&self) -> JsValue {
        object! { type : "LitStr" }
    }
}
impl ToJS for syn::LitVerbatim {
    fn to_js(&self) -> JsValue {
        object! { type : "LitVerbatim" , }
    }
}
impl ToJS for syn::Local {
    fn to_js(&self) -> JsValue {
        object! { type : "Local" , attrs : self . attrs , let_token : self . let_token , pats : self . pats , ty : self . ty , init : self . init , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::Macro {
    fn to_js(&self) -> JsValue {
        object! { type : "Macro" , path : self . path , bang_token : self . bang_token , delimiter : self . delimiter , }
    }
}
impl ToJS for syn::MacroDelimiter {
    fn to_js(&self) -> JsValue {
        match self {
            syn::MacroDelimiter::Paren(x) => x.to_js(),
            syn::MacroDelimiter::Brace(x) => x.to_js(),
            syn::MacroDelimiter::Bracket(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::Member {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Member::Named(x) => x.to_js(),
            syn::Member::Unnamed(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::Meta {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Meta::Word(x) => x.to_js(),
            syn::Meta::List(x) => x.to_js(),
            syn::Meta::NameValue(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::MetaList {
    fn to_js(&self) -> JsValue {
        object! { type : "MetaList" , paren_token : self . paren_token , nested : self . nested , }
    }
}
impl ToJS for syn::MetaNameValue {
    fn to_js(&self) -> JsValue {
        object! { type : "MetaNameValue" , eq_token : self . eq_token , lit : self . lit , }
    }
}
impl ToJS for syn::MethodSig {
    fn to_js(&self) -> JsValue {
        object! { type : "MethodSig" , constness : self . constness , unsafety : self . unsafety , asyncness : self . asyncness , abi : self . abi , decl : self . decl , }
    }
}
impl ToJS for syn::MethodTurbofish {
    fn to_js(&self) -> JsValue {
        object! { type : "MethodTurbofish" , colon2_token : self . colon2_token , lt_token : self . lt_token , args : self . args , gt_token : self . gt_token , }
    }
}
impl ToJS for syn::NestedMeta {
    fn to_js(&self) -> JsValue {
        match self {
            syn::NestedMeta::Meta(x) => x.to_js(),
            syn::NestedMeta::Literal(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::ParenthesizedGenericArguments {
    fn to_js(&self) -> JsValue {
        object! { type : "ParenthesizedGenericArguments" , paren_token : self . paren_token , inputs : self . inputs , output : self . output , }
    }
}
impl ToJS for syn::Pat {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Pat::Wild(x) => x.to_js(),
            syn::Pat::Ident(x) => x.to_js(),
            syn::Pat::Struct(x) => x.to_js(),
            syn::Pat::TupleStruct(x) => x.to_js(),
            syn::Pat::Path(x) => x.to_js(),
            syn::Pat::Tuple(x) => x.to_js(),
            syn::Pat::Box(x) => x.to_js(),
            syn::Pat::Ref(x) => x.to_js(),
            syn::Pat::Lit(x) => x.to_js(),
            syn::Pat::Range(x) => x.to_js(),
            syn::Pat::Slice(x) => x.to_js(),
            syn::Pat::Macro(x) => x.to_js(),
            syn::Pat::Verbatim(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::PatBox {
    fn to_js(&self) -> JsValue {
        object! { type : "PatBox" , box_token : self . box_token , pat : self . pat , }
    }
}
impl ToJS for syn::PatIdent {
    fn to_js(&self) -> JsValue {
        object! { type : "PatIdent" , by_ref : self . by_ref , mutability : self . mutability , subpat : self . subpat , }
    }
}
impl ToJS for syn::PatLit {
    fn to_js(&self) -> JsValue {
        object! { type : "PatLit" , expr : self . expr , }
    }
}
impl ToJS for syn::PatMacro {
    fn to_js(&self) -> JsValue {
        object! { type : "PatMacro" , mac : self . mac , }
    }
}
impl ToJS for syn::PatPath {
    fn to_js(&self) -> JsValue {
        object! { type : "PatPath" , qself : self . qself , path : self . path , }
    }
}
impl ToJS for syn::PatRange {
    fn to_js(&self) -> JsValue {
        object! { type : "PatRange" , lo : self . lo , limits : self . limits , hi : self . hi , }
    }
}
impl ToJS for syn::PatRef {
    fn to_js(&self) -> JsValue {
        object! { type : "PatRef" , and_token : self . and_token , mutability : self . mutability , pat : self . pat , }
    }
}
impl ToJS for syn::PatSlice {
    fn to_js(&self) -> JsValue {
        object! { type : "PatSlice" , bracket_token : self . bracket_token , front : self . front , middle : self . middle , dot2_token : self . dot2_token , comma_token : self . comma_token , back : self . back , }
    }
}
impl ToJS for syn::PatStruct {
    fn to_js(&self) -> JsValue {
        object! { type : "PatStruct" , path : self . path , brace_token : self . brace_token , fields : self . fields , dot2_token : self . dot2_token , }
    }
}
impl ToJS for syn::PatTuple {
    fn to_js(&self) -> JsValue {
        object! { type : "PatTuple" , paren_token : self . paren_token , front : self . front , dot2_token : self . dot2_token , comma_token : self . comma_token , back : self . back , }
    }
}
impl ToJS for syn::PatTupleStruct {
    fn to_js(&self) -> JsValue {
        object! { type : "PatTupleStruct" , path : self . path , pat : self . pat , }
    }
}
impl ToJS for syn::PatVerbatim {
    fn to_js(&self) -> JsValue {
        object! { type : "PatVerbatim" , }
    }
}
impl ToJS for syn::PatWild {
    fn to_js(&self) -> JsValue {
        object! { type : "PatWild" , underscore_token : self . underscore_token , }
    }
}
impl ToJS for syn::Path {
    fn to_js(&self) -> JsValue {
        object! { type : "Path" , leading_colon : self . leading_colon , segments : self . segments , }
    }
}
impl ToJS for syn::PathArguments {
    fn to_js(&self) -> JsValue {
        match self {
            syn::PathArguments::None => object! { type : "None" },
            syn::PathArguments::AngleBracketed(x) => x.to_js(),
            syn::PathArguments::Parenthesized(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::PathSegment {
    fn to_js(&self) -> JsValue {
        object! { type : "PathSegment" , arguments : self . arguments , }
    }
}
impl ToJS for syn::PredicateEq {
    fn to_js(&self) -> JsValue {
        object! { type : "PredicateEq" , lhs_ty : self . lhs_ty , eq_token : self . eq_token , rhs_ty : self . rhs_ty , }
    }
}
impl ToJS for syn::PredicateLifetime {
    fn to_js(&self) -> JsValue {
        object! { type : "PredicateLifetime" , lifetime : self . lifetime , colon_token : self . colon_token , bounds : self . bounds , }
    }
}
impl ToJS for syn::PredicateType {
    fn to_js(&self) -> JsValue {
        object! { type : "PredicateType" , lifetimes : self . lifetimes , bounded_ty : self . bounded_ty , colon_token : self . colon_token , bounds : self . bounds , }
    }
}
impl ToJS for syn::QSelf {
    fn to_js(&self) -> JsValue {
        object! { type : "QSelf" , lt_token : self . lt_token , ty : self . ty , position : self . position , as_token : self . as_token , gt_token : self . gt_token , }
    }
}
impl ToJS for syn::RangeLimits {
    fn to_js(&self) -> JsValue {
        match self {
            syn::RangeLimits::HalfOpen(x) => x.to_js(),
            syn::RangeLimits::Closed(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::ReturnType {
    fn to_js(&self) -> JsValue {
        match self {
            syn::ReturnType::Default => object! { type : "Default" },
            syn::ReturnType::Type(x0, x1) => object! { type : "Type" , values : ( x0 , x1 ) },
        }
    }
}
impl ToJS for syn::Stmt {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Stmt::Local(x) => x.to_js(),
            syn::Stmt::Item(x) => x.to_js(),
            syn::Stmt::Expr(x) => x.to_js(),
            syn::Stmt::Semi(x0, x1) => object! { type : "Semi" , values : ( x0 , x1 ) },
        }
    }
}
impl ToJS for syn::TraitBound {
    fn to_js(&self) -> JsValue {
        object! { type : "TraitBound" , paren_token : self . paren_token , modifier : self . modifier , lifetimes : self . lifetimes , path : self . path , }
    }
}
impl ToJS for syn::TraitBoundModifier {
    fn to_js(&self) -> JsValue {
        match self {
            syn::TraitBoundModifier::None => object! { type : "None" },
            syn::TraitBoundModifier::Maybe(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::TraitItem {
    fn to_js(&self) -> JsValue {
        match self {
            syn::TraitItem::Const(x) => x.to_js(),
            syn::TraitItem::Method(x) => x.to_js(),
            syn::TraitItem::Type(x) => x.to_js(),
            syn::TraitItem::Macro(x) => x.to_js(),
            syn::TraitItem::Verbatim(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::TraitItemConst {
    fn to_js(&self) -> JsValue {
        object! { type : "TraitItemConst" , attrs : self . attrs , const_token : self . const_token , colon_token : self . colon_token , ty : self . ty , default : self . default , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::TraitItemMacro {
    fn to_js(&self) -> JsValue {
        object! { type : "TraitItemMacro" , attrs : self . attrs , mac : self . mac , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::TraitItemMethod {
    fn to_js(&self) -> JsValue {
        object! { type : "TraitItemMethod" , attrs : self . attrs , sig : self . sig , default : self . default , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::TraitItemType {
    fn to_js(&self) -> JsValue {
        object! { type : "TraitItemType" , attrs : self . attrs , type_token : self . type_token , generics : self . generics , colon_token : self . colon_token , bounds : self . bounds , default : self . default , semi_token : self . semi_token , }
    }
}
impl ToJS for syn::TraitItemVerbatim {
    fn to_js(&self) -> JsValue {
        object! { type : "TraitItemVerbatim" , }
    }
}
impl ToJS for syn::Type {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Type::Slice(x) => x.to_js(),
            syn::Type::Array(x) => x.to_js(),
            syn::Type::Ptr(x) => x.to_js(),
            syn::Type::Reference(x) => x.to_js(),
            syn::Type::BareFn(x) => x.to_js(),
            syn::Type::Never(x) => x.to_js(),
            syn::Type::Tuple(x) => x.to_js(),
            syn::Type::Path(x) => x.to_js(),
            syn::Type::TraitObject(x) => x.to_js(),
            syn::Type::ImplTrait(x) => x.to_js(),
            syn::Type::Paren(x) => x.to_js(),
            syn::Type::Group(x) => x.to_js(),
            syn::Type::Infer(x) => x.to_js(),
            syn::Type::Macro(x) => x.to_js(),
            syn::Type::Verbatim(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::TypeArray {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeArray" , bracket_token : self . bracket_token , elem : self . elem , semi_token : self . semi_token , len : self . len , }
    }
}
impl ToJS for syn::TypeBareFn {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeBareFn" , lifetimes : self . lifetimes , unsafety : self . unsafety , abi : self . abi , fn_token : self . fn_token , paren_token : self . paren_token , inputs : self . inputs , variadic : self . variadic , output : self . output , }
    }
}
impl ToJS for syn::TypeGroup {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeGroup" , group_token : self . group_token , elem : self . elem , }
    }
}
impl ToJS for syn::TypeImplTrait {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeImplTrait" , impl_token : self . impl_token , bounds : self . bounds , }
    }
}
impl ToJS for syn::TypeInfer {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeInfer" , underscore_token : self . underscore_token , }
    }
}
impl ToJS for syn::TypeMacro {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeMacro" , mac : self . mac , }
    }
}
impl ToJS for syn::TypeNever {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeNever" , bang_token : self . bang_token , }
    }
}
impl ToJS for syn::TypeParam {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeParam" , attrs : self . attrs , colon_token : self . colon_token , bounds : self . bounds , eq_token : self . eq_token , default : self . default , }
    }
}
impl ToJS for syn::TypeParamBound {
    fn to_js(&self) -> JsValue {
        match self {
            syn::TypeParamBound::Trait(x) => x.to_js(),
            syn::TypeParamBound::Lifetime(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::TypeParen {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeParen" , paren_token : self . paren_token , elem : self . elem , }
    }
}
impl ToJS for syn::TypePath {
    fn to_js(&self) -> JsValue {
        object! { type : "TypePath" , qself : self . qself , path : self . path , }
    }
}
impl ToJS for syn::TypePtr {
    fn to_js(&self) -> JsValue {
        object! { type : "TypePtr" , star_token : self . star_token , const_token : self . const_token , mutability : self . mutability , elem : self . elem , }
    }
}
impl ToJS for syn::TypeReference {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeReference" , and_token : self . and_token , lifetime : self . lifetime , mutability : self . mutability , elem : self . elem , }
    }
}
impl ToJS for syn::TypeSlice {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeSlice" , bracket_token : self . bracket_token , elem : self . elem , }
    }
}
impl ToJS for syn::TypeTraitObject {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeTraitObject" , dyn_token : self . dyn_token , bounds : self . bounds , }
    }
}
impl ToJS for syn::TypeTuple {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeTuple" , paren_token : self . paren_token , elems : self . elems , }
    }
}
impl ToJS for syn::TypeVerbatim {
    fn to_js(&self) -> JsValue {
        object! { type : "TypeVerbatim" , }
    }
}
impl ToJS for syn::UnOp {
    fn to_js(&self) -> JsValue {
        match self {
            syn::UnOp::Deref(x) => x.to_js(),
            syn::UnOp::Not(x) => x.to_js(),
            syn::UnOp::Neg(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::UseGlob {
    fn to_js(&self) -> JsValue {
        object! { type : "UseGlob" , star_token : self . star_token , }
    }
}
impl ToJS for syn::UseGroup {
    fn to_js(&self) -> JsValue {
        object! { type : "UseGroup" , brace_token : self . brace_token , items : self . items , }
    }
}
impl ToJS for syn::UseName {
    fn to_js(&self) -> JsValue {
        object! { type : "UseName" , }
    }
}
impl ToJS for syn::UsePath {
    fn to_js(&self) -> JsValue {
        object! { type : "UsePath" , colon2_token : self . colon2_token , tree : self . tree , }
    }
}
impl ToJS for syn::UseRename {
    fn to_js(&self) -> JsValue {
        object! { type : "UseRename" , as_token : self . as_token , }
    }
}
impl ToJS for syn::UseTree {
    fn to_js(&self) -> JsValue {
        match self {
            syn::UseTree::Path(x) => x.to_js(),
            syn::UseTree::Name(x) => x.to_js(),
            syn::UseTree::Rename(x) => x.to_js(),
            syn::UseTree::Glob(x) => x.to_js(),
            syn::UseTree::Group(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::Variant {
    fn to_js(&self) -> JsValue {
        object! { type : "Variant" , attrs : self . attrs , fields : self . fields , discriminant : self . discriminant , }
    }
}
impl ToJS for syn::VisCrate {
    fn to_js(&self) -> JsValue {
        object! { type : "VisCrate" , crate_token : self . crate_token , }
    }
}
impl ToJS for syn::VisPublic {
    fn to_js(&self) -> JsValue {
        object! { type : "VisPublic" , pub_token : self . pub_token , }
    }
}
impl ToJS for syn::VisRestricted {
    fn to_js(&self) -> JsValue {
        object! { type : "VisRestricted" , pub_token : self . pub_token , paren_token : self . paren_token , in_token : self . in_token , path : self . path , }
    }
}
impl ToJS for syn::Visibility {
    fn to_js(&self) -> JsValue {
        match self {
            syn::Visibility::Public(x) => x.to_js(),
            syn::Visibility::Crate(x) => x.to_js(),
            syn::Visibility::Restricted(x) => x.to_js(),
            syn::Visibility::Inherited => object! { type : "Inherited" },
        }
    }
}
impl ToJS for syn::WhereClause {
    fn to_js(&self) -> JsValue {
        object! { type : "WhereClause" , where_token : self . where_token , predicates : self . predicates , }
    }
}
impl ToJS for syn::WherePredicate {
    fn to_js(&self) -> JsValue {
        match self {
            syn::WherePredicate::Type(x) => x.to_js(),
            syn::WherePredicate::Lifetime(x) => x.to_js(),
            syn::WherePredicate::Eq(x) => x.to_js(),
        }
    }
}
impl ToJS for syn::token::Abstract {
    fn to_js(&self) -> JsValue {
        object! { type : "Abstract" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Add {
    fn to_js(&self) -> JsValue {
        object! { type : "Add" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::AddEq {
    fn to_js(&self) -> JsValue {
        object! { type : "AddEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::And {
    fn to_js(&self) -> JsValue {
        object! { type : "And" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::AndAnd {
    fn to_js(&self) -> JsValue {
        object! { type : "AndAnd" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::AndEq {
    fn to_js(&self) -> JsValue {
        object! { type : "AndEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::As {
    fn to_js(&self) -> JsValue {
        object! { type : "As" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Async {
    fn to_js(&self) -> JsValue {
        object! { type : "Async" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::At {
    fn to_js(&self) -> JsValue {
        object! { type : "At" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Auto {
    fn to_js(&self) -> JsValue {
        object! { type : "Auto" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Bang {
    fn to_js(&self) -> JsValue {
        object! { type : "Bang" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Become {
    fn to_js(&self) -> JsValue {
        object! { type : "Become" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Box {
    fn to_js(&self) -> JsValue {
        object! { type : "Box" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Break {
    fn to_js(&self) -> JsValue {
        object! { type : "Break" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Caret {
    fn to_js(&self) -> JsValue {
        object! { type : "Caret" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::CaretEq {
    fn to_js(&self) -> JsValue {
        object! { type : "CaretEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Colon {
    fn to_js(&self) -> JsValue {
        object! { type : "Colon" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Colon2 {
    fn to_js(&self) -> JsValue {
        object! { type : "Colon2" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Comma {
    fn to_js(&self) -> JsValue {
        object! { type : "Comma" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Const {
    fn to_js(&self) -> JsValue {
        object! { type : "Const" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Continue {
    fn to_js(&self) -> JsValue {
        object! { type : "Continue" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Crate {
    fn to_js(&self) -> JsValue {
        object! { type : "Crate" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Default {
    fn to_js(&self) -> JsValue {
        object! { type : "Default" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Div {
    fn to_js(&self) -> JsValue {
        object! { type : "Div" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::DivEq {
    fn to_js(&self) -> JsValue {
        object! { type : "DivEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Do {
    fn to_js(&self) -> JsValue {
        object! { type : "Do" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Dot {
    fn to_js(&self) -> JsValue {
        object! { type : "Dot" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Dot2 {
    fn to_js(&self) -> JsValue {
        object! { type : "Dot2" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Dot3 {
    fn to_js(&self) -> JsValue {
        object! { type : "Dot3" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::DotDotEq {
    fn to_js(&self) -> JsValue {
        object! { type : "DotDotEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Dyn {
    fn to_js(&self) -> JsValue {
        object! { type : "Dyn" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Else {
    fn to_js(&self) -> JsValue {
        object! { type : "Else" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Enum {
    fn to_js(&self) -> JsValue {
        object! { type : "Enum" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Eq {
    fn to_js(&self) -> JsValue {
        object! { type : "Eq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::EqEq {
    fn to_js(&self) -> JsValue {
        object! { type : "EqEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Existential {
    fn to_js(&self) -> JsValue {
        object! { type : "Existential" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Extern {
    fn to_js(&self) -> JsValue {
        object! { type : "Extern" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::FatArrow {
    fn to_js(&self) -> JsValue {
        object! { type : "FatArrow" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Final {
    fn to_js(&self) -> JsValue {
        object! { type : "Final" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Fn {
    fn to_js(&self) -> JsValue {
        object! { type : "Fn" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::For {
    fn to_js(&self) -> JsValue {
        object! { type : "For" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Ge {
    fn to_js(&self) -> JsValue {
        object! { type : "Ge" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Gt {
    fn to_js(&self) -> JsValue {
        object! { type : "Gt" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::If {
    fn to_js(&self) -> JsValue {
        object! { type : "If" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Impl {
    fn to_js(&self) -> JsValue {
        object! { type : "Impl" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::In {
    fn to_js(&self) -> JsValue {
        object! { type : "In" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::LArrow {
    fn to_js(&self) -> JsValue {
        object! { type : "LArrow" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Le {
    fn to_js(&self) -> JsValue {
        object! { type : "Le" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Let {
    fn to_js(&self) -> JsValue {
        object! { type : "Let" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Loop {
    fn to_js(&self) -> JsValue {
        object! { type : "Loop" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Lt {
    fn to_js(&self) -> JsValue {
        object! { type : "Lt" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Macro {
    fn to_js(&self) -> JsValue {
        object! { type : "Macro" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Match {
    fn to_js(&self) -> JsValue {
        object! { type : "Match" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Mod {
    fn to_js(&self) -> JsValue {
        object! { type : "Mod" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Move {
    fn to_js(&self) -> JsValue {
        object! { type : "Move" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::MulEq {
    fn to_js(&self) -> JsValue {
        object! { type : "MulEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Mut {
    fn to_js(&self) -> JsValue {
        object! { type : "Mut" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Ne {
    fn to_js(&self) -> JsValue {
        object! { type : "Ne" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Or {
    fn to_js(&self) -> JsValue {
        object! { type : "Or" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::OrEq {
    fn to_js(&self) -> JsValue {
        object! { type : "OrEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::OrOr {
    fn to_js(&self) -> JsValue {
        object! { type : "OrOr" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Override {
    fn to_js(&self) -> JsValue {
        object! { type : "Override" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Pound {
    fn to_js(&self) -> JsValue {
        object! { type : "Pound" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Priv {
    fn to_js(&self) -> JsValue {
        object! { type : "Priv" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Pub {
    fn to_js(&self) -> JsValue {
        object! { type : "Pub" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Question {
    fn to_js(&self) -> JsValue {
        object! { type : "Question" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::RArrow {
    fn to_js(&self) -> JsValue {
        object! { type : "RArrow" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Ref {
    fn to_js(&self) -> JsValue {
        object! { type : "Ref" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Rem {
    fn to_js(&self) -> JsValue {
        object! { type : "Rem" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::RemEq {
    fn to_js(&self) -> JsValue {
        object! { type : "RemEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Return {
    fn to_js(&self) -> JsValue {
        object! { type : "Return" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::SelfType {
    fn to_js(&self) -> JsValue {
        object! { type : "SelfType" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::SelfValue {
    fn to_js(&self) -> JsValue {
        object! { type : "SelfValue" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Semi {
    fn to_js(&self) -> JsValue {
        object! { type : "Semi" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Shl {
    fn to_js(&self) -> JsValue {
        object! { type : "Shl" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::ShlEq {
    fn to_js(&self) -> JsValue {
        object! { type : "ShlEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Shr {
    fn to_js(&self) -> JsValue {
        object! { type : "Shr" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::ShrEq {
    fn to_js(&self) -> JsValue {
        object! { type : "ShrEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Star {
    fn to_js(&self) -> JsValue {
        object! { type : "Star" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Static {
    fn to_js(&self) -> JsValue {
        object! { type : "Static" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Struct {
    fn to_js(&self) -> JsValue {
        object! { type : "Struct" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Sub {
    fn to_js(&self) -> JsValue {
        object! { type : "Sub" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::SubEq {
    fn to_js(&self) -> JsValue {
        object! { type : "SubEq" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Super {
    fn to_js(&self) -> JsValue {
        object! { type : "Super" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Tilde {
    fn to_js(&self) -> JsValue {
        object! { type : "Tilde" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Trait {
    fn to_js(&self) -> JsValue {
        object! { type : "Trait" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Try {
    fn to_js(&self) -> JsValue {
        object! { type : "Try" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Type {
    fn to_js(&self) -> JsValue {
        object! { type : "Type" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Typeof {
    fn to_js(&self) -> JsValue {
        object! { type : "Typeof" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Underscore {
    fn to_js(&self) -> JsValue {
        object! { type : "Underscore" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Union {
    fn to_js(&self) -> JsValue {
        object! { type : "Union" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Unsafe {
    fn to_js(&self) -> JsValue {
        object! { type : "Unsafe" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Unsized {
    fn to_js(&self) -> JsValue {
        object! { type : "Unsized" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Use {
    fn to_js(&self) -> JsValue {
        object! { type : "Use" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Virtual {
    fn to_js(&self) -> JsValue {
        object! { type : "Virtual" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Where {
    fn to_js(&self) -> JsValue {
        object! { type : "Where" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::While {
    fn to_js(&self) -> JsValue {
        object! { type : "While" , span : self . span ( ) }
    }
}
impl ToJS for syn::token::Yield {
    fn to_js(&self) -> JsValue {
        object! { type : "Yield" , span : self . span ( ) }
    }
}
