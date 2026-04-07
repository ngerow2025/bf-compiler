use serde::Serialize;

use crate::{
    ast_annotation::{
        ASTAnnotation, ConstructArrayAccessAnnotationParams, ConstructAssignmentAnnotationParams,
        ConstructBlockAnnotationParams, ConstructCharLiteralAnnotationParams,
        ConstructExpressionStatementAnnotationParams, ConstructFnCallAnnotationParams,
        ConstructFunctionAnnotationParams, ConstructFunctionParamAnnotationParams,
        ConstructIntLiteralAnnotationParams, ConstructProgramAnnotationParams,
        ConstructQualifiedIdentifierAnnotationParams, ConstructSimpleTypeNodeAnnotationParams,
        ConstructStrTypeNodeAnnotationParams, ConstructStringLiteralAnnotationParams,
        ConstructVarDeclAnnotationParams, ConstructVariableAccessAnnotationParams,
        ConstructVariableAccessExpressionAnnotationParams,
    },
    parser::{BlockItem, Expression},
    sources::{SourceCodeOrigin, SourceLocation},
};

#[derive(Debug, Clone, Serialize)]
pub struct SourceAnnotation {
    #[serde(skip)]
    pub source_code: SourceCodeOrigin,
}

impl SourceAnnotation {
    pub fn new(source_code: SourceCodeOrigin) -> Self {
        SourceAnnotation { source_code }
    }
}

impl ASTAnnotation for SourceAnnotation {
    type ProgramAnnotation = ();

    fn construct_program_annotation(
        &mut self,
        _params: ConstructProgramAnnotationParams<'_, Self>,
    ) -> Self::ProgramAnnotation {
    }

    type FunctionParamAnnotation = SourceLocation;

    fn construct_function_param_annotation(
        &mut self,
        params: ConstructFunctionParamAnnotationParams<'_, Self>,
    ) -> Self::FunctionParamAnnotation {
        SourceLocation::superset([
            &params.identifier_token.loc,
            &params.colon_token.loc,
            &params.type_node.annotation,
        ])
    }

    type FunctionAnnotation = SourceLocation;

    fn construct_function_annotation(
        &mut self,
        params: ConstructFunctionAnnotationParams<'_, Self>,
    ) -> Self::FunctionAnnotation {
        SourceLocation::superset([
            &params.fn_token.loc,
            &params.name_token.loc,
            &params.left_paren_token.loc,
            &params.right_paren_token.loc,
            &params.body.annotation,
        ])
    }

    type BlockAnnotation = SourceLocation;

    fn construct_block_annotation(
        &mut self,
        params: ConstructBlockAnnotationParams<'_, Self>,
    ) -> Self::BlockAnnotation {
        SourceLocation::superset(
            params
                .statements
                .iter()
                .map(|stmt| match stmt {
                    BlockItem::Statement(statement) => match statement {
                        crate::parser::Statement::Expression { annotation, .. } => annotation,
                        crate::parser::Statement::Assignment { annotation, .. } => annotation,
                        crate::parser::Statement::VarDecl { annotation, .. } => annotation,
                    },
                    BlockItem::Block(block) => &block.annotation,
                })
                .chain([&params.left_brace_token.loc, &params.right_brace_token.loc]),
        )
    }

    type StatementAnnotation = SourceLocation;

    fn construct_expression_statement_annotation(
        &mut self,
        params: ConstructExpressionStatementAnnotationParams<'_, Self>,
    ) -> Self::StatementAnnotation {
        SourceLocation::superset([params.expr.annotation(), &params.semicolon_token.loc])
    }

    fn construct_assignment_annotation(
        &mut self,
        params: ConstructAssignmentAnnotationParams<'_, Self>,
    ) -> Self::StatementAnnotation {
        SourceLocation::superset([
            &params.identifier_token.loc,
            &params.equals_token.loc,
            params.value.annotation(),
            &params.semicolon_token.loc,
        ])
    }

    fn construct_var_decl_annotation(
        &mut self,
        params: ConstructVarDeclAnnotationParams<'_, Self>,
    ) -> Self::StatementAnnotation {
        SourceLocation::superset([
            &params.let_token.loc,
            &params.identifier_token.loc,
            &params.colon_token.loc,
            &params.type_node.annotation,
            &params.equals_token.loc,
            params.value.annotation(),
            &params.semicolon_token.loc,
        ])
    }

    type ExpressionAnnotation = SourceLocation;

    fn construct_string_literal_annotation(
        &mut self,
        params: ConstructStringLiteralAnnotationParams<'_>,
    ) -> Self::ExpressionAnnotation {
        params.string_token.loc.clone()
    }

    fn construct_char_literal_annotation(
        &mut self,
        params: ConstructCharLiteralAnnotationParams<'_>,
    ) -> Self::ExpressionAnnotation {
        params.char_token.loc.clone()
    }

    fn construct_int_literal_annotation(
        &mut self,
        params: ConstructIntLiteralAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation {
        SourceLocation::superset([&params.int_token.loc, &params.type_node.annotation])
    }

    fn construct_array_access_annotation(
        &mut self,
        params: ConstructArrayAccessAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation {
        SourceLocation::superset([
            &params.array.annotation,
            &params.left_bracket_token.loc,
            params.index_expr.annotation(),
            &params.right_bracket_token.loc,
        ])
    }

    fn construct_fn_call_annotation(
        &mut self,
        params: ConstructFnCallAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation {
        SourceLocation::superset(
            std::iter::once(&params.qualified_name.annotation)
                .chain(std::iter::once(&params.left_paren_token.loc))
                .chain(params.arguments.iter().map(|arg| match arg {
                    Expression::IntLiteral { annotation, .. } => annotation,
                    Expression::VariableAccess { annotation, .. } => annotation,
                    Expression::FnCall { annotation, .. } => annotation,
                    Expression::StringLiteral { annotation, .. } => annotation,
                    Expression::ArrayAccess { annotation, .. } => annotation,
                    Expression::CharLiteral { annotation, .. } => annotation,
                }))
                .chain(params.comma_tokens.iter().map(|comma| &comma.loc))
                .chain(std::iter::once(&params.right_paren_token.loc)),
        )
    }

    type VariableAccessAnnotation = SourceLocation;

    fn construct_variable_access_annotation(
        &mut self,
        params: ConstructVariableAccessAnnotationParams<'_>,
    ) -> Self::VariableAccessAnnotation {
        params.name_token.loc.clone()
    }

    type QualifiedIdentifierAnnotation = SourceLocation;

    fn construct_qualified_identifier_annotation(
        &mut self,
        params: ConstructQualifiedIdentifierAnnotationParams<'_>,
    ) -> Self::QualifiedIdentifierAnnotation {
        SourceLocation::superset(
            params
                .identifier_tokens
                .iter()
                .map(|t| &t.loc)
                .chain(params.double_colon_tokens.iter().map(|t| &t.loc)),
        )
    }

    type ASTTypeNodeAnnotation = SourceLocation;

    fn construct_simple_type_node_annotation(
        &mut self,
        params: ConstructSimpleTypeNodeAnnotationParams<'_>,
    ) -> Self::ASTTypeNodeAnnotation {
        params.type_token.loc.clone()
    }

    fn construct_str_type_node_annotation(
        &mut self,
        params: ConstructStrTypeNodeAnnotationParams<'_>,
    ) -> Self::ASTTypeNodeAnnotation {
        SourceLocation::superset([
            &params.type_token.loc,
            &params.left_angle_token.loc,
            &params.size_token.loc,
            &params.right_angle_token.loc,
        ])
    }

    fn construct_variable_access_expression_annotation(
        &mut self,
        params: ConstructVariableAccessExpressionAnnotationParams<'_, Self>,
    ) -> Self::ExpressionAnnotation {
        params.variable_access.annotation.clone()
    }
}
