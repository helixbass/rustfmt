use rustc_ast::ast;
use rustc_ast::ptr::P;
use rustc_ast::token::{Delimiter, TokenKind};
use rustc_ast::tokenstream::{TokenStream, TokenTree};
use rustc_parse::parser::Parser;

use crate::rewrite::RewriteContext;

pub(crate) type ArrowSeparatedKeyValuePairs =
    Vec<(P<ast::Expr>, ExprOrArrowSeparatedKeyValuePairs)>;

pub(crate) enum ExprOrArrowSeparatedKeyValuePairs {
    Expr(P<ast::Expr>),
    ArrowSeparatedKeyValuePairs((ArrowSeparatedKeyValuePairs, Delimiter)),
    TokenTree(TokenTree),
}

pub(crate) fn parse_arrow_separated_key_value_pairs(
    context: &RewriteContext<'_>,
    ts: TokenStream,
) -> Option<ArrowSeparatedKeyValuePairs> {
    _parse_arrow_separated_key_value_pairs(context, &mut super::build_parser(context, ts))
}

fn _parse_arrow_separated_key_value_pairs<'a, 'b: 'a>(
    context: &RewriteContext<'b>,
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
        if !parser.eat(&TokenKind::FatArrow) {
            return None;
        }
        let value = parse_value(context, parser)?;
        parser.eat(&TokenKind::Comma);
        result.push((key, value));
    }

    Some(result)
}

fn parse_value<'a, 'b: 'a>(
    context: &RewriteContext<'b>,
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
    let mut cloned_parser = (*parser).clone();
    let delimiter = if cloned_parser.eat(&TokenKind::OpenDelim(Delimiter::Brace)) {
        Delimiter::Brace
    } else if cloned_parser.eat(&TokenKind::OpenDelim(Delimiter::Bracket)) {
        Delimiter::Bracket
    } else {
        return None;
    };
    let value_contents_token_stream = cloned_parser.parse_tokens();
    _parse_arrow_separated_key_value_pairs(
        context,
        &mut super::build_parser(context, value_contents_token_stream),
    )
    // .map(ExprOrArrowSeparatedKeyValuePairs::ArrowSeparatedKeyValuePairs)
    .map(|x| {
        let _ = cloned_parser.eat(&TokenKind::CloseDelim(Delimiter::Brace))
            || cloned_parser.eat(&TokenKind::CloseDelim(Delimiter::Bracket));
        *parser = cloned_parser;
        ExprOrArrowSeparatedKeyValuePairs::ArrowSeparatedKeyValuePairs((x, delimiter))
    })
    .or_else(|| {
        if !matches!(
            parser.token.kind,
            TokenKind::OpenDelim(Delimiter::Brace) | TokenKind::OpenDelim(Delimiter::Bracket)
        ) {
            return None;
        }

        let mut cloned_parser = (*parser).clone();
        let mut all_token_trees = cloned_parser.parse_all_token_trees().ok()?;
        let next_token_tree = (!all_token_trees.is_empty()).then(|| all_token_trees.remove(0))?;
        *parser = super::build_parser(context, TokenStream::new(all_token_trees));
        Some(ExprOrArrowSeparatedKeyValuePairs::TokenTree(
            next_token_tree,
        ))
    })
}
