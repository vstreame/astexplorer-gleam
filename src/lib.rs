use gleam_core::{ast::*, parse};
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

impl ToJS for SrcSpan {
    fn to_js(&self) -> JsValue {
        js!(Span {
            start: self.start,
            end: self.end,
        })
    }
}

impl<T: ToJS> ToJS for CallArg<T> {
    fn to_js(&self) -> JsValue {
        js!(CallArg {
            span: self.location,
            label: self.label,
            value: self.value
        })
    }
}

impl ToJS for UntypedPattern {
    fn to_js(&self) -> JsValue {
        match self {
            Pattern::Int { location, value } => js!(IntPattern {
                span: location,
                value: value
            }),
            Pattern::Float { location, value } => js!(FloatPattern {
                span: location,
                value: value
            }),
            Pattern::String { location, value } => js!(StringPattern {
                span: location,
                value: value
            }),
            Pattern::Var { location, name } => js!(VarPattern {
                span: location,
                name: name
            }),
            Pattern::VarUsage {
                location,
                name,
                type_: _,
            } => js!(VarUsagePattern {
                span: location,
                name: name
            }),
            Pattern::Assign {
                location,
                name,
                pattern,
            } => js!(AssignPattern {
                span: location,
                name: name,
                pattern: pattern,
            }),
            Pattern::Discard { location, name } => js!(DiscardPattern {
                span: location,
                name: name,
            }),
            Pattern::List {
                location,
                elements,
                tail,
            } => js!(ListPattern {
                span: location,
                elements: elements,
                tail: tail,
            }),
            Pattern::Constructor {
                location,
                name,
                arguments,
                module: _,
                constructor,
                with_spread,
                type_: _,
            } => js!(ConstructorPattern {
                span: location,
                name: name,
                arguments: arguments,
                constructor: constructor,
                with_spread: with_spread,
            }),
            Pattern::Tuple { location, elems } => js!(TuplePattern {
                span: location,
                elems: elems,
            }),
            Pattern::BitString { location, segments } => js!(BitStringPattern {
                span: location,
                segments: segments,
            }),
        }
    }
}

impl ToJS for AssignmentKind {
    fn to_js(&self) -> JsValue {
        match self {
            AssignmentKind::Let => "let".to_js(),
            AssignmentKind::Assert => "assert".to_js(),
        }
    }
}

impl ToJS for UntypedConstant {
    fn to_js(&self) -> JsValue {
        match self {
            Constant::Int { location, value } => js!(IntContant {
                span: location,
                value: value
            }),
            Constant::Float { location, value } => js!(FloatConstant {
                span: location,
                value: value
            }),
            Constant::String { location, value } => js!(StringConstant {
                span: location,
                value: value
            }),
            Constant::Tuple { location, elements } => js!(TupleConstant {
                elems: elements,
                span: location,
            }),
            Constant::List {
                location,
                elements,
                typ: _,
            } => js!(ListConstant {
                elements: elements,
                tail: None::<Box<UntypedExpr>>,
                span: location,
            }),
            Constant::Record {
                location,
                name,
                args,
                // FIXME: How should we surface this info?
                tag: _,
                field_map: _,
                module: _,
                typ: _,
            } => js!(RecordConstant {
                name: name,
                args: args,
                span: location,
            }),
            Constant::BitString {
                location,
                // FIXME: How should we surface this info?
                segments: _,
            } => js!(BitStringConstant { span: location }),
        }
    }
}

impl ToJS for UntypedClauseGuard {
    fn to_js(&self) -> JsValue {
        match self {
            ClauseGuard::Equals {
                location,
                left,
                right,
            } => js!(ClauseGuardEquals {
                span: location,
                left: left,
                right: right,
            }),
            ClauseGuard::NotEquals {
                location,
                left,
                right,
            } => js!(ClauseGuardNotEquals {
                span: location,
                left: left,
                right: right,
            }),
            ClauseGuard::GtInt {
                location,
                left,
                right,
            } => js!(ClauseGuardGtInt {
                span: location,
                left: left,
                right: right,
            }),
            ClauseGuard::GtEqInt {
                location,
                left,
                right,
            } => js!(ClauseGuardGtEqInt {
                span: location,
                left: left,
                right: right,
            }),
            ClauseGuard::LtInt {
                location,
                left,
                right,
            } => js!(ClauseGuardLtInt {
                span: location,
                left: left,
                right: right,
            }),
            ClauseGuard::LtEqInt {
                location,
                left,
                right,
            } => js!(ClauseGuardLtEqInt {
                span: location,
                left: left,
                right: right,
            }),
            ClauseGuard::GtFloat {
                location,
                left,
                right,
            } => js!(ClauseGuardGtFloat {
                span: location,
                left: left,
                right: right,
            }),
            ClauseGuard::GtEqFloat {
                location,
                left,
                right,
            } => js!(ClauseGuardGtEqFloat {
                span: location,
                left: left,
                right: right,
            }),
            ClauseGuard::LtFloat {
                location,
                left,
                right,
            } => js!(ClauseGuardLtFloat {
                span: location,
                left: left,
                right: right,
            }),
            ClauseGuard::LtEqFloat {
                location,
                left,
                right,
            } => js!(ClauseGuardLtEqFloat {
                span: location,
                left: left,
                right: right,
            }),
            ClauseGuard::Or {
                location,
                left,
                right,
            } => js!(ClauseGuardOr {
                span: location,
                left: left,
                right: right
            }),
            ClauseGuard::And {
                location,
                left,
                right,
            } => js!(ClauseGuardAnd {
                span: location,
                left: left,
                right: right
            }),
            ClauseGuard::Var {
                location,
                name,
                type_: _,
            } => js!(ClauseGuardVar {
                span: location,
                name: name,
            }),
            ClauseGuard::TupleIndex {
                location,
                index,
                tuple,
                type_: _,
            } => js!(ClauseGuardTupleIndex {
                span: location,
                index: index,
                tuple: tuple,
            }),
            ClauseGuard::Constant(constant) => constant.to_js(),
        }
    }
}

impl ToJS for UntypedClause {
    fn to_js(&self) -> JsValue {
        js!(Clause {
            pattern: self.pattern,
            alternative_patterns: self.alternative_patterns,
            guard: self.guard,
            then: self.then,
            span: self.location,
        })
    }
}

impl<T: ToJS> ToJS for BitStringSegmentOption<T> {
    fn to_js(&self) -> JsValue {
        js!(BitStringSegmentOption {
            span: self.location(),
            label: self.label(),
        })
    }
}

impl<T: ToJS> ToJS for BitStringSegment<T, ()> {
    fn to_js(&self) -> JsValue {
        js!(BitStringSegment {
            span: self.location,
            value: self.value,
            options: self.options,
        })
    }
}

impl ToJS for RecordUpdateSpread {
    fn to_js(&self) -> JsValue {
        js!(RecordUpdateSpread {
            base: self.base,
            span: self.location,
        })
    }
}

impl ToJS for UntypedRecordUpdateArg {
    fn to_js(&self) -> JsValue {
        js!(RecordUpdateArg {
            label: self.label,
            span: self.location,
            value: self.value,
        })
    }
}

impl ToJS for UntypedExpr {
    fn to_js(&self) -> JsValue {
        match self {
            UntypedExpr::Int { location, value } => js!(IntLiteral {
                span: location,
                value: value
            }),
            UntypedExpr::Float { location, value } => js!(FloatLiteral {
                span: location,
                value: value
            }),
            UntypedExpr::String { location, value } => js!(StringLiteral {
                span: location,
                value: value
            }),
            UntypedExpr::Sequence {
                location,
                expressions,
            } => js!(SequenceExpression {
                span: location,
                expressions: expressions
            }),
            UntypedExpr::Var { location, name, .. } => js!(VarExpression {
                span: location,
                name: name
            }),
            UntypedExpr::Fn {
                location,
                is_capture,
                arguments,
                body,
                return_annotation,
            } => js!(FnExpression {
                arguments: arguments,
                body: body,
                is_capture: is_capture,
                return_annotation: return_annotation,
                span: location,
            }),
            UntypedExpr::List {
                location,
                elements,
                tail,
            } => js!(ListExpression {
                elements: elements,
                tail: tail,
                span: location,
            }),
            UntypedExpr::Call {
                location,
                fun,
                arguments,
            } => js!(CallExpression {
                fun: fun,
                arguments: arguments,
                span: location,
            }),
            UntypedExpr::BinOp {
                location,
                name,
                left,
                right,
            } => js!(BinaryExpression {
                name: name.name(),
                left: left,
                right: right,
                span: location,
            }),
            UntypedExpr::PipeLine { expressions } => js!(PipeLineExpression {
                expressions: expressions,
                span: SrcSpan {
                    start: expressions.first().location().start,
                    end: expressions.last().location().end,
                },
            }),
            UntypedExpr::Assignment {
                location,
                value,
                pattern,
                kind,
                annotation,
            } => js!(AssignmentExpression {
                annotation: annotation,
                kind: kind,
                pattern: pattern,
                span: location,
                value: value,
            }),
            UntypedExpr::Try {
                location,
                value,
                pattern,
                then,
                annotation,
                ..
            } => js!(TryExpression {
                annotation: annotation,
                pattern: pattern,
                span: location,
                then: then,
                value: value,
            }),
            UntypedExpr::Case {
                location,
                subjects,
                clauses,
            } => js!(CaseExpression {
                clauses: clauses,
                subjects: subjects,
                span: location,
            }),
            UntypedExpr::FieldAccess {
                location,
                label,
                container,
            } => js!(FieldAccessExpression {
                label: label,
                container: container,
                span: location,
            }),
            UntypedExpr::Tuple { elems, location } => js!(TupleExpression {
                elems: elems,
                span: location,
            }),
            UntypedExpr::TupleIndex {
                index,
                tuple,
                location,
            } => js!(TupleIndexExpression {
                index: index,
                tuple: tuple,
                span: location,
            }),
            UntypedExpr::Todo { label, location } => js!(TodoExpression {
                label: label,
                span: location,
            }),
            UntypedExpr::BitString { location, segments } => js!(BitStringExpression {
                segments: segments,
                span: location,
            }),
            UntypedExpr::RecordUpdate {
                location,
                constructor,
                spread,
                arguments,
            } => js!(RecordUpdateExpression {
                constructor: constructor,
                spread: spread,
                arguments: arguments,
                span: location,
            }),
            UntypedExpr::Negate { location, value } => js!(NegateExpression {
                span: location,
                value: value,
            }),
        }
    }
}

impl ToJS for TypeAst {
    fn to_js(&self) -> JsValue {
        match self {
            TypeAst::Constructor {
                name,
                location,
                arguments,
                ..
            } => js!(ConstructorType {
                arguments: arguments,
                name: name,
                span: location,
            }),
            TypeAst::Fn {
                location,
                arguments,
                return_,
                ..
            } => js!(FnType {
                arguments: arguments,
                span: location,
                return: return_
            }),
            TypeAst::Var { location, name, .. } => js!(VarType {
                name: name,
                span: location,
            }),
            TypeAst::Tuple {
                location, elems, ..
            } => js!(TupleType {
                elems: elems,
                span: location,
            }),
            TypeAst::Hole { location, name, .. } => js!(HoleType {
                name: name,
                span: location,
            }),
        }
    }
}

impl ToJS for Arg<()> {
    fn to_js(&self) -> JsValue {
        js!(Arg {
            annotation: self.annotation,
            name: self.get_variable_name().map(|x| x.to_string()),
            span: self.location,
        })
    }
}

impl ToJS for RecordConstructorArg<()> {
    fn to_js(&self) -> JsValue {
        js!(RecordConstructorArg {
            label: self.label,
            type: self.ast,
            span: self.location,
            doc: self.doc,
        })
    }
}

impl ToJS for RecordConstructor<()> {
    fn to_js(&self) -> JsValue {
        js!(RecordConstructor {
            span: self.location,
            name: self.name,
            arguments: self.arguments,
            doc: self.documentation,
        })
    }
}

impl ToJS for ExternalFnArg<()> {
    fn to_js(&self) -> JsValue {
        js!(ExternalFnArg {
            span: self.location,
            label: self.label,
            type: self.annotation,
        })
    }
}

impl ToJS for UnqualifiedImport {
    fn to_js(&self) -> JsValue {
        js!(UnqualifiedImport {
            span: self.location,
            name: self.name,
            as_name: self.as_name,
        })
    }
}

impl ToJS for UntypedStatement {
    fn to_js(&self) -> JsValue {
        match self {
            Statement::Fn {
                arguments,
                body,
                doc,
                location,
                name,
                public,
                return_annotation,
                return_type: _,
                end_position: _,
            } => js!(FnStatement {
                arguments: arguments,
                body: body,
                doc: doc,
                name: name,
                public: public,
                return_annotation: return_annotation,
                span: location,
            }),
            Statement::TypeAlias {
                alias,
                doc,
                location,
                parameters,
                public,
                type_: _,
                type_ast,
            } => js!(TypeAliasStatement {
                alias: alias,
                doc: doc,
                span: location,
                parameters: parameters,
                public: public,
                type: type_ast,
            }),
            Statement::CustomType {
                constructors,
                doc,
                location,
                name,
                opaque,
                parameters,
                public,
                typed_parameters: _,
            } => js!(CustomTypeStatement {
                constructors: constructors,
                doc: doc,
                name: name,
                opaque: opaque,
                parameters: parameters,
                public: public,
                span: location,
            }),
            Statement::ExternalFn {
                arguments,
                doc,
                fun,
                location,
                module,
                name,
                public,
                return_,
                return_type: _,
            } => js!(ExternalFnStatement {
                arguments: arguments,
                doc: doc,
                fun: fun,
                span: location,
                module: module,
                name: name,
                public: public,
                return: return_,
            }),
            Statement::ExternalType {
                arguments,
                doc,
                location,
                name,
                public,
            } => js!(ExternalTypeStatement {
                arguments: arguments,
                doc: doc,
                span: location,
                name: name,
                public: public,
            }),
            Statement::Import {
                location,
                module,
                as_name,
                unqualified,
                package,
            } => js!(ImportStatement {
                span: location,
                module: module,
                as_name: as_name,
                unqualified: unqualified,
                package: package,
            }),
            Statement::ModuleConstant {
                doc,
                location,
                public,
                name,
                annotation,
                value,
                type_: _,
            } => js!(ModuleConstantStatement {
                span: location,
                doc: doc,
                public: public,
                name: name,
                annotation: annotation,
                value: value,
            }),
        }
    }
}

impl ToJS for UntypedModule {
    fn to_js(&self) -> JsValue {
        let obj = new_object_with_type("Module");
        let mut i = 0;
        for group in self.statements.clone() {
            for item in group.statements().clone() {
                obj.set_i(i, item.to_js());
                i += 1;
            }
        }
        obj.set("length", i.to_js());
        JsValue::from(obj)
    }
}

impl ToJS for parse::error::ParseError {
    fn to_js(&self) -> JsValue {
        let err = SyntaxError::new(self.details().0);
        err.set_line_number(self.location.start as u32);
        err.into()
    }
}

#[wasm_bindgen(js_name = "parseFile")]
pub fn parse_file(gleam_str: &str) -> Result<JsValue, JsValue> {
    match parse::parse_module(gleam_str) {
        Ok((ast, _)) => Ok(ast.to_js()),
        Err(err) => Err(err.to_js()),
    }
}
