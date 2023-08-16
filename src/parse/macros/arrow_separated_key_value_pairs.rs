use rustc_ast::ast;
use rustc_ast::ptr::P;
use rustc_ast::token::TokenKind;
use rustc_ast::tokenstream::TokenStream;

use crate::rewrite::RewriteContext;

pub(crate) fn parse_arrow_separated_key_value_pairs(
    context: &RewriteContext<'_>,
    ts: TokenStream,
) -> Option<Vec<(P<ast::Expr>, P<ast::Expr>)>> {
    let mut result = vec![];
    let mut parser = super::build_parser(context, ts);
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
        let value = parse_or!(parse_expr);
        parser.eat(&TokenKind::Comma);
        result.push((key, value));
    }

    Some(result)
}
