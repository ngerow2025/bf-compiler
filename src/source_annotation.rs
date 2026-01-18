use crate::{
    parser::{
        ASTAnnotation, ASTTypeKind, ASTTypeNode, Block, BlockItem, Expression, Function,
        FunctionId, FunctionParam, IntLiteral, QualifiedName, VariableAccess, VariableId,
    },
    sources::{SourceCodeOrigin, SourceLocation},
    tokenizer::{Locatable, Token},
};

#[derive(Debug, Clone)]
pub struct SourceAnnotation {
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
        functions: &[Function<Self>],
        function_name_mapping: &std::collections::HashMap<FunctionId, String>,
    ) -> Self::ProgramAnnotation {
        ()
    }

    type FunctionParamAnnotation = SourceLocation;

    fn construct_function_param_annotation(
        &mut self,
        identifier_token: &Locatable<Token>,
        colon_token: &Locatable<Token>,
        type_node: &ASTTypeNode<Self>,
        variable_index: &VariableId,
    ) -> Self::FunctionParamAnnotation {
        SourceLocation::superset([
            &identifier_token.loc,
            &colon_token.loc,
            &type_node.annotation,
        ])
    }

    type FunctionAnnotation = SourceLocation;

    fn construct_function_annotation(
        &mut self,
        fn_token: &Locatable<Token>,
        name_token: &Locatable<Token>,
        left_paren_token: &Locatable<Token>,
        params: &[FunctionParam<Self>],
        comma_tokens: &[Locatable<Token>],
        right_paren_token: &Locatable<Token>,
        body: &Block<Self>,
        function_id: &FunctionId,
    ) -> Self::FunctionAnnotation {
        SourceLocation::superset([
            &fn_token.loc,
            &name_token.loc,
            &left_paren_token.loc,
            &right_paren_token.loc,
            &body.annotation,
        ])
    }

    type BlockAnnotation = SourceLocation;

    fn construct_block_annotation(
        &mut self,
        left_brace_token: &Locatable<Token>,
        statements: &[BlockItem<Self>],
        right_brace_token: &Locatable<Token>,
    ) -> Self::BlockAnnotation {
        SourceLocation::superset(
            statements
                .iter()
                .map(|stmt| match stmt {
                    BlockItem::Statement(statement) => match statement {
                        crate::parser::Statement::Expression { annotation, .. } => annotation,
                        crate::parser::Statement::Assignment { annotation, .. } => annotation,
                        crate::parser::Statement::VarDecl { annotation, .. } => annotation,
                    },
                    BlockItem::Block(block) => &block.annotation,
                })
                .chain([&left_brace_token.loc, &right_brace_token.loc]),
        )
    }

    type StatementAnnotation = SourceLocation;

    fn construct_expression_statement_annotation(
        &mut self,
        expr: &Expression<Self>,
        semicolon_token: &Locatable<Token>,
    ) -> Self::StatementAnnotation {
        SourceLocation::superset([expr.annotation(), &semicolon_token.loc])
    }

    fn construct_assignment_annotation(
        &mut self,
        identifier_token: &Locatable<Token>,
        equals_token: &Locatable<Token>,
        value: &Expression<Self>,
        semicolon_token: &Locatable<Token>,
    ) -> Self::StatementAnnotation {
        SourceLocation::superset([
            &identifier_token.loc,
            &equals_token.loc,
            &value.annotation(),
            &semicolon_token.loc,
        ])
    }

    fn construct_var_decl_annotation(
        &mut self,
        let_token: &Locatable<Token>,
        identifier_token: &Locatable<Token>,
        colon_token: &Locatable<Token>,
        type_node: &ASTTypeNode<Self>,
        equals_token: &Locatable<Token>,
        value: &Expression<Self>,
        semicolon_token: &Locatable<Token>,
        variable_index: &VariableId,
    ) -> Self::StatementAnnotation {
        SourceLocation::superset([
            &let_token.loc,
            &identifier_token.loc,
            &colon_token.loc,
            &type_node.annotation,
            &equals_token.loc,
            &value.annotation(),
            &semicolon_token.loc,
        ])
    }

    type ExpressionAnnotation = SourceLocation;

    fn construct_string_literal_annotation(
        &mut self,
        string_token: &Locatable<Token>,
        value: &str,
    ) -> Self::ExpressionAnnotation {
        string_token.loc.clone()
    }

    fn construct_int_literal_annotation(
        &mut self,
        int_token: &Locatable<Token>,
        int_value: &str,
        type_node: &ASTTypeNode<Self>,
        parsed_value: &IntLiteral,
    ) -> Self::ExpressionAnnotation {
        SourceLocation::superset([&int_token.loc, &type_node.annotation])
    }

    fn construct_array_access_annotation(
        &mut self,
        array: &VariableAccess<Self>,
        left_bracket_token: &Locatable<Token>,
        index_expr: &Expression<Self>,
        right_bracket_token: &Locatable<Token>,
    ) -> Self::ExpressionAnnotation {
        SourceLocation::superset([
            &array.annotation,
            &left_bracket_token.loc,
            &index_expr.annotation(),
            &right_bracket_token.loc,
        ])
    }

    fn construct_fn_call_annotation(
        &mut self,
        qualified_name: &QualifiedName<Self>,
        left_paren_token: &Locatable<Token>,
        arguments: &[Expression<Self>],
        comma_tokens: &[Locatable<Token>],
        right_paren_token: &Locatable<Token>,
    ) -> Self::ExpressionAnnotation {
        SourceLocation::superset(
            std::iter::once(&qualified_name.annotation)
                .chain(std::iter::once(&left_paren_token.loc))
                .chain(arguments.iter().map(|arg| match arg {
                    Expression::IntLiteral { annotation, .. } => annotation,
                    Expression::VariableAccess { annotation, .. } => annotation,
                    Expression::FnCall { annotation, .. } => annotation,
                    Expression::StringLiteral { annotation, .. } => annotation,
                    Expression::ArrayAccess { annotation, .. } => annotation,
                }))
                .chain(comma_tokens.iter().map(|comma| &comma.loc))
                .chain(std::iter::once(&right_paren_token.loc)),
        )
    }

    type VariableAccessAnnotation = SourceLocation;

    fn construct_variable_access_annotation(
        &mut self,
        name_token: &Locatable<Token>,
        name: &str,
        variable_id: &VariableId,
    ) -> Self::VariableAccessAnnotation {
        name_token.loc.clone()
    }

    type QualifiedIdentifierAnnotation = SourceLocation;

    fn construct_qualified_identifier_annotation(
        &mut self,
        identifier_tokens: &[Locatable<Token>],
        double_colon_tokens: &[Locatable<Token>],
        parts: &[String],
        name: &str,
    ) -> Self::QualifiedIdentifierAnnotation {
        SourceLocation::superset(
            identifier_tokens
                .iter()
                .map(|t| &t.loc)
                .chain(double_colon_tokens.iter().map(|t| &t.loc)),
        )
    }

    type ASTTypeNodeAnnotation = SourceLocation;

    fn construct_simple_type_node_annotation(
        &mut self,
        type_token: &Locatable<Token>,
        kind: &ASTTypeKind,
    ) -> Self::ASTTypeNodeAnnotation {
        type_token.loc.clone()
    }

    fn construct_str_type_node_annotation(
        &mut self,
        type_token: &Locatable<Token>,
        kind: &ASTTypeKind,
        left_angle_token: &Locatable<Token>,
        size_token: &Locatable<Token>,
        right_angle_token: &Locatable<Token>,
    ) -> Self::ASTTypeNodeAnnotation {
        SourceLocation::superset([
            &type_token.loc,
            &left_angle_token.loc,
            &size_token.loc,
            &right_angle_token.loc,
        ])
    }

    fn construct_variable_access_expression_annotation(
        &mut self,
        variable_access: &VariableAccess<Self>,
    ) -> Self::ExpressionAnnotation {
        variable_access.annotation.clone()
    }
}
