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
    ArrowSeparatedKeyValuePairs(ArrowSeparatedKeyValuePairs),
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
    println!("parse_value() 1");
    match cloned_parser.parse_expr() {
        Ok(expr) => {
            println!("parse_value() 2");
            if cloned_parser.sess.span_diagnostic.has_errors().is_some() {
                println!("parse_value() 3");
                cloned_parser.sess.span_diagnostic.reset_err_count();
            } else {
                println!("parse_value() 4");
                // Parsing succeeded.
                *parser = cloned_parser;
                return Some(ExprOrArrowSeparatedKeyValuePairs::Expr(expr));
            }
        }
        Err(e) => {
            println!("parse_value() 5");
            e.cancel();
            cloned_parser.sess.span_diagnostic.reset_err_count();
        }
    }
    println!("parse_value() 6");
    if !(parser.eat(&TokenKind::OpenDelim(Delimiter::Brace))
        || parser.eat(&TokenKind::OpenDelim(Delimiter::Bracket)))
    {
        return None;
    }
    let value_contents_token_stream = parser.parse_tokens();
    let ret = _parse_arrow_separated_key_value_pairs(
        context,
        &mut super::build_parser(context, value_contents_token_stream),
    )
    // .map(ExprOrArrowSeparatedKeyValuePairs::ArrowSeparatedKeyValuePairs)
    .map(|x| {
        println!("parse_value() 7");
        ExprOrArrowSeparatedKeyValuePairs::ArrowSeparatedKeyValuePairs(x)
    });
    let _ = parser.eat(&TokenKind::CloseDelim(Delimiter::Brace))
        || parser.eat(&TokenKind::CloseDelim(Delimiter::Bracket));
    ret
}
