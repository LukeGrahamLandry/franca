use std::fs;

use serde::Deserialize;

#[derive(Deserialize, Debug, Clone)]
#[serde(tag = "kind")]
enum ClangData {
    TranslationUnitDecl {
        inner: Vec<Option<ClangData>>,
    },
    TypedefDecl {
        name: String,
        r#type: ClangType,
        inner: Option<Vec<ClangData>>,
    },
    FieldDecl {
        name: Option<String>,
        r#type: ClangType,
    },
    RecordDecl {
        name: Option<String>,
        tagUsed: ClangTag,
        inner: Option<Vec<ClangData>>,
    },
    ElaboratedType {
        r#type: ClangType,
        inner: Option<Vec<ClangData>>,
    },
    FunctionDecl {
        name: String,
        mangledName: String,
        r#type: ClangType,
        inner: Option<Vec<ClangData>>,
    },
    ParmVarDecl {
        name: Option<String>,
        r#type: ClangType,
    },
    BuiltinType {
        r#type: ClangType,
    },
    PointerType {
        r#type: ClangType,
        inner: Option<Vec<ClangData>>,
    },
    ConstantArrayType {
        size: usize,
        r#type: ClangType,
        inner: Option<Vec<ClangData>>,
    },
    IncompleteArrayType {
        r#type: ClangType,
        inner: Option<Vec<ClangData>>,
    },
    RecordType {
        r#type: ClangType,
    },
    TypedefType {
        r#type: ClangType,
    },
    ParenType {
        r#type: ClangType,
        inner: Option<Vec<ClangData>>,
    },
    FunctionProtoType {
        r#type: ClangType,
        inner: Option<Vec<ClangData>>,
    },
    #[serde(other)]
    Other,
}

#[derive(Deserialize, Debug, Clone)]
enum ClangTag {
    r#struct,
    r#enum,
    r#union,
}

#[derive(Deserialize, Debug, Clone)]
struct ClangType {
    qualType: String,
}

//#[test]
fn clang_ast() {
    let ast = fs::read_to_string("/Users/luke/Downloads/mir-master 2/sokol/ast.json").unwrap();
    let ast: ClangData = serde_json::from_str(&ast).unwrap();
    panic!("{ast:#?}");
}
