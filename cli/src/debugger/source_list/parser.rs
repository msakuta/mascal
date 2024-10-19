use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, alphanumeric1, char, one_of},
    combinator::{opt, recognize},
    multi::{many0, many1},
    sequence::{delimited, pair, terminated},
    Finish, IResult,
};
use ratatui::{style::Stylize, text::Span};

pub(super) fn style_text(is_current: bool, line_num: usize, s: &str) -> Vec<Span> {
    let current = if is_current { "*" } else { " " };
    let mut line = vec![Span::from(format!("{current}  {line_num:4} "))];
    if let Ok((_, s)) = text(s) {
        line.extend_from_slice(&s);
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

fn text(input: &str) -> Result<(&str, Vec<Span>), nom::error::Error<&str>> {
    many0(alt((comment, keyword, decimal_value, non_ident)))(input).finish()
}

#[test]
fn test_non_ident() {
    let s = "!!! hello";
    assert_eq!(non_ident(s), Ok(("hello", "!!! ".into())));
}

#[test]
fn test_text() {
    let s = "fn hello 1";
    assert_eq!(
        text(s),
        Ok((
            "",
            vec![
                "fn".blue(),
                " ".into(),
                "hello".light_cyan(),
                " ".into(),
                "1".light_green()
            ]
        ))
    );
}
