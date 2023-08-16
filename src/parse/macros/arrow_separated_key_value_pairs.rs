use rustc_ast::ast;
use rustc_ast::ptr::P;
use rustc_ast::token::{Delimiter, TokenKind};
use rustc_ast::tokenstream::TokenStream;
use rustc_parse::parser::Parser;

use crate::rewrite::RewriteContext;

pub(crate) type ArrowSeparatedKeyValuePairs =
    Vec<(P<ast::Expr>, ExprOrArrowSeparatedKeyValuePairs)>;

pub(crate) enum ExprOrArrowSeparatedKeyValuePairs {
    Expr(P<ast::Expr>),
    ArrowSeparatedKeyValuePairs((ArrowSeparatedKeyValuePairs, Delimiter)),
}

pub(crate) fn parse_arrow_separated_key_value_pairs(
    context: &RewriteContext<'_>,
    ts: TokenStream,
) -> Option<ArrowSeparatedKeyValuePairs> {
    _parse_arrow_separated_key_value_pairs(context, &mut super::build_parser(context, ts))
}

fn _parse_arrow_separated_key_value_pairs<'a, 'b: 'a>(
    context: &RewriteContext<'_>,
    parser: &'a mut Parser<'b>,
) -> Option<ArrowSeparatedKeyValuePairs> {
    let mut result = vec![];
    macro_rules! parse_or {
        ($method:ident $(,)* $($arg:expr),* $(,)*) => {
            match parser.$method($($arg,)*) {
                Ok(val) => {
                    if parser.sess.span_diagnostic.has_errors().is_some() {
                        parser.sess.span_diagnostic.reset_err_count();
                        return None;
                    } else {
                        val
                    }
                }
                Err(err) => {
                    err.cancel();
                    parser.sess.span_diagnostic.reset_err_count();
                    return None;
                }
            }
        }
    }

    while parser.token.kind != TokenKind::Eof {
        let key = parse_or!(parse_expr);
        parser.eat(&TokenKind::FatArrow);
        let value = parse_value(context, parser)?;
        parser.eat(&TokenKind::Comma);
        result.push((key, value));
    }

    Some(result)
}

fn parse_value<'a, 'b: 'a>(
    context: &RewriteContext<'_>,
    parser: &'a mut Parser<'b>,
) -> Option<ExprOrArrowSeparatedKeyValuePairs> {
    let mut cloned_parser = (*parser).clone();
    match cloned_parser.parse_expr() {
        Ok(expr) => {
            if cloned_parser.sess.span_diagnostic.has_errors().is_some() {
                cloned_parser.sess.span_diagnostic.reset_err_count();
            } else {
                // Parsing succeeded.
                *parser = cloned_parser;
                return Some(ExprOrArrowSeparatedKeyValuePairs::Expr(expr));
            }
        }
        Err(e) => {
            e.cancel();
            cloned_parser.sess.span_diagnostic.reset_err_count();
        }
    }
    let delimiter = if parser.eat(&TokenKind::OpenDelim(Delimiter::Brace)) {
        Delimiter::Brace
    } else if parser.eat(&TokenKind::OpenDelim(Delimiter::Bracket)) {
        Delimiter::Bracket
    } else {
        return None;
    };
    let value_contents_token_stream = parser.parse_tokens();
    let ret = _parse_arrow_separated_key_value_pairs(
        context,
        &mut super::build_parser(context, value_contents_token_stream),
    )
    // .map(ExprOrArrowSeparatedKeyValuePairs::ArrowSeparatedKeyValuePairs)
    .map(|x| ExprOrArrowSeparatedKeyValuePairs::ArrowSeparatedKeyValuePairs((x, delimiter)));
    let _ = parser.eat(&TokenKind::CloseDelim(Delimiter::Brace))
        || parser.eat(&TokenKind::CloseDelim(Delimiter::Bracket));
    ret
}
