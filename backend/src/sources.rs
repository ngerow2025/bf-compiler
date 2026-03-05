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
        let mut begining = elements
            .first()
            .expect("No elements provided")
            .span
            .offset();
        let mut end = elements
            .first()
            .expect("No elements provided")
            .span
            .offset()
            + elements.first().expect("No elements provided").span.len();
        for element in &elements {
            let element_begining = element.span.offset();
            let element_end = element.span.offset() + element.span.len();
            if element_begining < begining {
                begining = element_begining;
            }
            if element_end > end {
                end = element_end;
            }
        }
        SourceLocation {
            span: SourceSpan::new(begining.into(), end - begining),
            origin: elements
                .first()
                .expect("No elements provided")
                .origin
                .clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SourceCodeOrigin {
    File(Arc<String>), //String contains the file path
    Anon(Arc<String>), //String contains the code itself
}
