use std::collections::HashMap;
use std::fmt::Debug;

use serde::Serialize;

use crate::{
    parser::{
        ASTTypeKind, ASTTypeNode, Block, BlockItem, Expression, Function, FunctionId,
        FunctionParam, IntLiteral, QualifiedName, VariableAccess, VariableId,
    },
    tokenizer::{Locatable, Token},
};

#[derive(Debug, Clone)]
pub struct ConstructProgramAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub functions: &'a [Function<Annotation>],
    pub function_name_mapping: &'a HashMap<FunctionId, String>,
}

#[derive(Debug, Clone)]
pub struct ConstructFunctionParamAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub identifier_token: &'a Locatable<Token>,
    pub colon_token: &'a Locatable<Token>,
    pub type_node: &'a ASTTypeNode<Annotation>,
    pub variable_index: &'a VariableId,
}

#[derive(Debug, Clone)]
pub struct ConstructFunctionAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub fn_token: &'a Locatable<Token>,
    pub name_token: &'a Locatable<Token>,
    pub left_paren_token: &'a Locatable<Token>,
    pub params: &'a [FunctionParam<Annotation>],
    pub comma_tokens: &'a [Locatable<Token>],
    pub right_paren_token: &'a Locatable<Token>,
    pub body: &'a Block<Annotation>,
    pub function_id: &'a FunctionId,
}

#[derive(Debug, Clone)]
pub struct ConstructBlockAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub left_brace_token: &'a Locatable<Token>,
    pub statements: &'a [BlockItem<Annotation>],
    pub right_brace_token: &'a Locatable<Token>,
}

#[derive(Debug, Clone)]
pub struct ConstructExpressionStatementAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub expr: &'a Expression<Annotation>,
    pub semicolon_token: &'a Locatable<Token>,
}

#[derive(Debug, Clone)]
pub struct ConstructAssignmentAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub identifier_token: &'a Locatable<Token>,
    pub equals_token: &'a Locatable<Token>,
    pub value: &'a Expression<Annotation>,
    pub semicolon_token: &'a Locatable<Token>,
}

#[derive(Debug, Clone)]
pub struct ConstructVarDeclAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub let_token: &'a Locatable<Token>,
    pub identifier_token: &'a Locatable<Token>,
    pub colon_token: &'a Locatable<Token>,
    pub type_node: &'a ASTTypeNode<Annotation>,
    pub equals_token: &'a Locatable<Token>,
    pub value: &'a Expression<Annotation>,
    pub semicolon_token: &'a Locatable<Token>,
    pub variable_index: &'a VariableId,
}

#[derive(Debug, Clone)]
pub struct ConstructStringLiteralAnnotationParams<'a> {
    pub string_token: &'a Locatable<Token>,
    pub value: &'a str,
}

#[derive(Debug, Clone)]
pub struct ConstructCharLiteralAnnotationParams<'a> {
    pub char_token: &'a Locatable<Token>,
    pub value: &'a char,
    pub parsed_value: &'a IntLiteral,
}

#[derive(Debug, Clone)]
pub struct ConstructIntLiteralAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub int_token: &'a Locatable<Token>,
    pub int_value: &'a str,
    pub type_node: &'a ASTTypeNode<Annotation>,
    pub parsed_value: &'a IntLiteral,
}

#[derive(Debug, Clone)]
pub struct ConstructArrayAccessAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub array: &'a VariableAccess<Annotation>,
    pub left_bracket_token: &'a Locatable<Token>,
    pub index_expr: &'a Expression<Annotation>,
    pub right_bracket_token: &'a Locatable<Token>,
}

#[derive(Debug, Clone)]
pub struct ConstructFnCallAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub qualified_name: &'a QualifiedName<Annotation>,
    pub left_paren_token: &'a Locatable<Token>,
    pub arguments: &'a [Expression<Annotation>],
    pub comma_tokens: &'a [Locatable<Token>],
    pub right_paren_token: &'a Locatable<Token>,
}

#[derive(Debug, Clone)]
pub struct ConstructVariableAccessExpressionAnnotationParams<'a, Annotation: ASTAnnotation> {
    pub variable_access: &'a VariableAccess<Annotation>,
}

#[derive(Debug, Clone)]
pub struct ConstructVariableAccessAnnotationParams<'a> {
    pub name_token: &'a Locatable<Token>,
    pub name: &'a str,
    pub variable_id: &'a VariableId,
}

#[derive(Debug, Clone)]
pub struct ConstructQualifiedIdentifierAnnotationParams<'a> {
    pub identifier_tokens: &'a [Locatable<Token>],
    pub double_colon_tokens: &'a [Locatable<Token>],
    pub parts: &'a [String],
    pub name: &'a str,
}

#[derive(Debug, Clone)]
pub struct ConstructSimpleTypeNodeAnnotationParams<'a> {
    pub type_token: &'a Locatable<Token>,
    pub kind: &'a ASTTypeKind,
}

#[derive(Debug, Clone)]
pub struct ConstructStrTypeNodeAnnotationParams<'a> {
    pub type_token: &'a Locatable<Token>,
    pub kind: &'a ASTTypeKind,
    pub left_angle_token: &'a Locatable<Token>,
    pub size_token: &'a Locatable<Token>,
    pub right_angle_token: &'a Locatable<Token>,
}

pub trait ASTAnnotation: Sized + Clone + Debug {
    type ProgramAnnotation: Debug + Clone + Serialize;
    fn construct_program_annotation(
        &mut self,
        params: ConstructProgramAnnotationParams<'_, Self>,
    ) -> Self::ProgramAnnotation;
    type FunctionParamAnnotation: Debug + Clone + Serialize;
    fn construct_function_param_annotation(
        &mut self,
        params: ConstructFunctionParamAnnotationParams<'_, Self>,
    ) -> Self::FunctionParamAnnotation;
    type FunctionAnnotation: Debug + Clone + Serialize;
    fn construct_function_annotation(
        &mut self,
        params: ConstructFunctionAnnotationParams<'_, Self>,
    ) -> Self::FunctionAnnotation;
    type BlockAnnotation: Debug + Clone + Serialize;
    fn construct_block_annotation(
        &mut self,
        params: ConstructBlockAnnotationParams<'_, Self>,
    ) -> Self::BlockAnnotation;
    type StatementAnnotation: Debug + Clone + Serialize;
    fn construct_expression_statement_annotation(
        &mut self,
        params: ConstructExpressionStatementAnnotationParams<'_, Self>,
    ) -> Self::StatementAnnotation;
    fn construct_assignment_annotation(
        &mut self,
        params: ConstructAssignmentAnnotationParams<'_, Self>,
    ) -> Self::StatementAnnotation;
    fn construct_var_decl_annotation(
        &mut self,
        params: ConstructVarDeclAnnotationParams<'_, Self>,
    ) -> Self::StatementAnnotation;
    type ExpressionAnnotation: Debug + Clone + Serialize;
    fn construct_string_literal_annotation(
        &mut self,
        params: ConstructStringLiteralAnnotationParams<'_>,
    ) -> Self::ExpressionAnnotation;
    fn construct_char_literal_annotation(
        &mut self,
        params: ConstructCharLiteralAnnotationParams<'_>,
    ) -> Self::ExpressionAnnotation;
    fn construct_int_literal_annotation(
        &mut self,
        params: ConstructIntLiteralAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation;
    fn construct_array_access_annotation(
        &mut self,
        params: ConstructArrayAccessAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation;
    fn construct_fn_call_annotation(
        &mut self,
        params: ConstructFnCallAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation;
    fn construct_variable_access_expression_annotation(
        &mut self,
        params: ConstructVariableAccessExpressionAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation;
    type VariableAccessAnnotation: Debug + Clone + Serialize;
    fn construct_variable_access_annotation(
        &mut self,
        params: ConstructVariableAccessAnnotationParams<'_>,
    ) -> Self::VariableAccessAnnotation;
    type QualifiedIdentifierAnnotation: Debug + Clone + Serialize;
    fn construct_qualified_identifier_annotation(
        &mut self,
        params: ConstructQualifiedIdentifierAnnotationParams<'_>,
    ) -> Self::QualifiedIdentifierAnnotation;
    type ASTTypeNodeAnnotation: Debug + Clone + Serialize;
    fn construct_simple_type_node_annotation(
        &mut self,
        params: ConstructSimpleTypeNodeAnnotationParams<'_>,
    ) -> Self::ASTTypeNodeAnnotation;
    fn construct_str_type_node_annotation(
        &mut self,
        params: ConstructStrTypeNodeAnnotationParams<'_>,
    ) -> Self::ASTTypeNodeAnnotation;
}

impl ASTAnnotation for () {
    type ProgramAnnotation = ();

    fn construct_program_annotation(
        &mut self,
        _params: ConstructProgramAnnotationParams<'_, Self>,
    ) -> Self::ProgramAnnotation {
    }

    type FunctionParamAnnotation = ();

    fn construct_function_param_annotation(
        &mut self,
        _params: ConstructFunctionParamAnnotationParams<'_, Self>,
    ) -> Self::FunctionParamAnnotation {
    }

    type FunctionAnnotation = ();

    fn construct_function_annotation(
        &mut self,
        _params: ConstructFunctionAnnotationParams<'_, Self>,
    ) -> Self::FunctionAnnotation {
    }

    type BlockAnnotation = ();

    fn construct_block_annotation(
        &mut self,
        _params: ConstructBlockAnnotationParams<'_, Self>,
    ) -> Self::BlockAnnotation {
    }

    type StatementAnnotation = ();

    fn construct_expression_statement_annotation(
        &mut self,
        _params: ConstructExpressionStatementAnnotationParams<'_, Self>,
    ) -> Self::StatementAnnotation {
    }

    fn construct_assignment_annotation(
        &mut self,
        _params: ConstructAssignmentAnnotationParams<'_, Self>,
    ) -> Self::StatementAnnotation {
    }

    fn construct_var_decl_annotation(
        &mut self,
        _params: ConstructVarDeclAnnotationParams<'_, Self>,
    ) -> Self::StatementAnnotation {
    }

    type ExpressionAnnotation = ();

    fn construct_string_literal_annotation(
        &mut self,
        _params: ConstructStringLiteralAnnotationParams<'_>,
    ) -> Self::ExpressionAnnotation {
    }

    fn construct_char_literal_annotation(
        &mut self,
        _params: ConstructCharLiteralAnnotationParams<'_>,
    ) -> Self::ExpressionAnnotation {
    }

    fn construct_int_literal_annotation(
        &mut self,
        _params: ConstructIntLiteralAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation {
    }

    fn construct_array_access_annotation(
        &mut self,
        _params: ConstructArrayAccessAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation {
    }

    fn construct_fn_call_annotation(
        &mut self,
        _params: ConstructFnCallAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation {
    }

    type VariableAccessAnnotation = ();

    fn construct_variable_access_annotation(
        &mut self,
        _params: ConstructVariableAccessAnnotationParams<'_>,
    ) -> Self::VariableAccessAnnotation {
    }

    type QualifiedIdentifierAnnotation = ();

    fn construct_qualified_identifier_annotation(
        &mut self,
        _params: ConstructQualifiedIdentifierAnnotationParams<'_>,
    ) -> Self::QualifiedIdentifierAnnotation {
    }

    fn construct_variable_access_expression_annotation(
        &mut self,
        _params: ConstructVariableAccessExpressionAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation {
    }

    type ASTTypeNodeAnnotation = ();

    fn construct_simple_type_node_annotation(
        &mut self,
        _params: ConstructSimpleTypeNodeAnnotationParams<'_>,
    ) -> Self::ASTTypeNodeAnnotation {
    }

    fn construct_str_type_node_annotation(
        &mut self,
        _params: ConstructStrTypeNodeAnnotationParams<'_>,
    ) -> Self::ASTTypeNodeAnnotation {
    }
}
