//! The parser for the source code for the syntax highlighting and debug position indication.
//! It is deliberately different from source code parser (/parser/src/parser.rs) since it only tokenizes,
//! does not build a syntax tree.

use mascal::LineInfo;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, alphanumeric1, char, multispace1, none_of, one_of},
    combinator::{opt, recognize},
    multi::{many0, many1},
    sequence::{delimited, pair, terminated},
    Finish, IResult,
};
use ratatui::{
    style::{Color, Style, Stylize},
    text::Span,
};

pub(super) fn style_text<'a>(
    current_pos: Option<&LineInfo>,
    line_num: usize,
    s: &'a str,
) -> Vec<Span<'a>> {
    let is_current = current_pos.is_some_and(|s| s.src_line as usize == line_num);
    let current = if is_current { "*" } else { " " };
    let mut line = vec![Span::from(format!("{current}  {line_num:4} "))];
    if let Ok((_, s)) = text(s) {
        line.extend_from_slice(&s);
    }
    if let Some(current_pos) = current_pos {
        if is_current {
            // Patch a sequence of highlighted text. The style of syntax highlight does not necessarily
            // have the same boundary as the source information, but we try to match them as much as possible
            // but do not actively insert a new span since it's too complicated to do now.
            let mut accum_len = 0;
            // Skip the first element since it is the line number, not a part of the source text.
            for span in line.iter_mut().skip(1) {
                let span_len = span.content.len();
                // Subtract 1 since nom_locate's LocatedSpan returns a column index starting with 1.
                let cur_col = current_pos.src_column.saturating_sub(1) as usize;
                let cur_len = current_pos.src_len as usize;
                if accum_len <= cur_col + cur_len && cur_col < accum_len + span_len {
                    *span = span.clone().style(Style::new().bg(Color::LightYellow));
                }
                accum_len += cur_len;
            }
        }
    }
    line
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn keyword(input: &str) -> IResult<&str, Span> {
    let (r, id) = identifier(input)?;
    Ok(
        if matches!(
            id,
            "fn" | "if" | "else" | "for" | "in" | "var" | "while" | "loop" | "break"
        ) {
            (r, id.blue())
        } else {
            (r, id.light_cyan())
        },
    )
}

fn decimal(input: &str) -> IResult<&str, &str> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

fn decimal_value(i: &str) -> IResult<&str, Span> {
    recognize(pair(opt(one_of("+-")), decimal))(i).map(|(r, s)| (r, s.light_green()))
}

fn comment(r: &str) -> IResult<&str, Span> {
    recognize(delimited(tag("/*"), take_until("*/"), tag("*/")))(r).map(|(r, s)| (r, s.green()))
}

fn non_ident(mut input: &str) -> IResult<&str, Span> {
    let start = input;
    let mut last = None;
    loop {
        let mut iter = input.chars();
        let Some(next) = iter.next() else { break };
        if next.is_alphanumeric() || next == '_' {
            break;
        }
        if next == '*' && last == Some('/') {
            break;
        }
        last = Some(next);
        input = iter.as_str();
    }
    if start == input {
        return Err(nom::Err::Error(nom::error::Error::new(
            "",
            nom::error::ErrorKind::Alpha,
        )));
    }
    Ok((
        input,
        start[..input.as_ptr() as usize - start.as_ptr() as usize].into(),
    ))
}

fn punctuation(i: &str) -> IResult<&str, Span> {
    alt((recognize(one_of("(){}[],:;*+-/=<>")), tag("->")))(i).map(|(r, s)| (r, s.white()))
}

fn str_literal(i: &str) -> IResult<&str, Span> {
    recognize(delimited(char('\"'), many0(none_of("\"")), char('"')))(i)
        .map(|(r, s)| (r, s.light_magenta()))
}

fn whitespace(i: &str) -> IResult<&str, Span> {
    multispace1(i).map(|(r, s)| (r, s.into()))
}

fn text(input: &str) -> Result<(&str, Vec<Span>), nom::error::Error<&str>> {
    many0(alt((
        comment,
        keyword,
        whitespace,
        punctuation,
        decimal_value,
        str_literal,
        non_ident,
    )))(input)
    .finish()
}

#[test]
fn test_non_ident() {
    let s = "!!! hello";
    assert_eq!(non_ident(s), Ok(("hello", "!!! ".into())));
}

#[test]
fn test_text() {
    let s = "fn hello() 1";
    assert_eq!(
        text(s),
        Ok((
            "",
            vec![
                "fn".blue(),
                " ".into(),
                "hello".light_cyan(),
                "(".light_yellow(),
                ")".light_yellow(),
                " ".into(),
                "1".light_green()
            ]
        ))
    );
}
