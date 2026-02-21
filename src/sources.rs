use std::sync::Arc;

use miette::SourceSpan;
use serde::Serialize;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct SourceLocation {
    pub span: SourceSpan,
    #[serde(skip)]
    pub origin: Option<SourceCodeOrigin>,
}

impl SourceLocation {
    pub fn superset<'a>(elements: impl IntoIterator<Item = &'a SourceLocation>) -> SourceLocation {
        let elements: Vec<_> = elements.into_iter().collect();
        let first = elements.first().expect("No elements provided");
        let last = elements.last().expect("No elements provided");
        SourceLocation {
            span: SourceSpan::new(
                first.span.offset().into(),
                last.span.offset() + last.span.len() - first.span.offset(),
            ),
            origin: first.origin.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SourceCodeOrigin {
    File(Arc<String>), //String contains the file path
    Anon(Arc<String>), //String contains the code itself
}
