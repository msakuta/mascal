//! The parser for the source code for the syntax highlighting and debug position indication.
//! It is deliberately different from source code parser (/parser/src/parser.rs) since it only tokenizes,
//! does not build a syntax tree.

use std::collections::HashSet;

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
    lexer: &mut Lexer,
    current_pos: Option<&LineInfo>,
    breakpoints: &HashSet<usize>,
    line_num: usize,
    s: &'a str,
) -> Vec<Span<'a>> {
    let is_current = current_pos.is_some_and(|s| s.src_line as usize == line_num);
    let current = if is_current { "*" } else { " " };
    let has_breakpoint = breakpoints.contains(&line_num);
    let breakpoint = if has_breakpoint {
        "o".red()
    } else {
        " ".into()
    };
    let mut all_line = vec![
        Span::from(current),
        Span::from(breakpoint),
        Span::from(format!(" {line_num:4} ")),
    ];
    let mut line = lexer
        .eat(s)
        .unwrap_or_else(|e| vec![format!("line: {s} {e}").red()]);
    if let Some(current_pos) = current_pos {
        if is_current {
            // Patch a sequence of highlighted text. The style of syntax highlight does not necessarily
            // have the same boundary as the source information, but we try to match them as much as possible
            // but do not actively insert a new span since it's too complicated to do now.
            let mut accum_len = 0;
            for span in line.iter_mut() {
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
    all_line.extend_from_slice(&line);
    all_line
}

#[derive(Default, Clone, Copy, Debug)]
/// A lexer that can retain state among lines.
pub(super) struct Lexer {
    cur: LexerState,
}

#[derive(Default, Clone, Copy, Debug)]
enum LexerState {
    #[default]
    Normal,
    Comment,
    Str,
}

impl Lexer {
    pub fn new() -> Self {
        Self::default()
    }

    fn eat<'a>(&mut self, line: &'a str) -> Result<Vec<Span<'a>>, nom::error::Error<&'a str>> {
        let mut input = line;
        let mut res = vec![];
        'global_loop: loop {
            match self.cur {
                LexerState::Normal => {
                    if input.starts_with("/*") {
                        self.cur = LexerState::Comment;
                        res.push("/*".green());
                        input = &input[2..];
                        continue;
                    }

                    if input.starts_with("\"") {
                        self.cur = LexerState::Str;
                        res.push(input[..1].light_magenta());
                        input = &input[1..];
                        continue;
                    }

                    if let Ok((r, s)) = whitespace(input) {
                        res.push(s);
                        input = r;
                        continue;
                    }
                    if let Ok((r, s)) = keyword(input) {
                        res.push(s);
                        input = r;
                        continue;
                    }
                    if let Ok((r, s)) = punctuation(input) {
                        res.push(s);
                        input = r;
                        continue;
                    }
                    if let Ok((r, s)) = decimal_value(input) {
                        res.push(s);
                        input = r;
                        continue;
                    }
                }
                LexerState::Comment => {
                    let start = input;
                    loop {
                        if 2 <= input.len() && &input[..2] == "*/" {
                            self.cur = LexerState::Normal;
                            input = &input[2..];
                            break;
                        }
                        let mut chars = input.chars();
                        if chars.next().is_none() {
                            res.push(
                                start[..input.as_ptr() as usize - start.as_ptr() as usize].green(),
                            );
                            break 'global_loop;
                        }
                        input = chars.as_str();
                    }

                    res.push(start[..input.as_ptr() as usize - start.as_ptr() as usize].green());
                    continue;
                }
                LexerState::Str => {
                    let start = input;
                    loop {
                        if input.starts_with("\"") {
                            self.cur = LexerState::Normal;
                            input = &input[1..];
                            break;
                        }
                        let mut chars = input.chars();
                        if chars.next().is_none() {
                            res.push(
                                start[..input.as_ptr() as usize - start.as_ptr() as usize]
                                    .magenta(),
                            );
                            break 'global_loop;
                        }
                        input = chars.as_str();
                    }

                    res.push(start[..input.as_ptr() as usize - start.as_ptr() as usize].magenta());
                    continue;
                }
            }

            break;
        }
        Ok(res)
    }
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

fn _comment(r: &str) -> IResult<&str, Span> {
    recognize(delimited(tag("/*"), take_until("*/"), tag("*/")))(r).map(|(r, s)| (r, s.green()))
}

fn _non_ident(mut input: &str) -> IResult<&str, Span> {
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

fn _str_literal(i: &str) -> IResult<&str, Span> {
    recognize(delimited(char('\"'), many0(none_of("\"")), char('"')))(i)
        .map(|(r, s)| (r, s.light_magenta()))
}

fn whitespace(i: &str) -> IResult<&str, Span> {
    multispace1(i).map(|(r, s)| (r, s.into()))
}

fn _text(input: &str) -> Result<(&str, Vec<Span>), nom::error::Error<&str>> {
    many0(alt((
        _comment,
        keyword,
        whitespace,
        punctuation,
        decimal_value,
        _str_literal,
        _non_ident,
    )))(input)
    .finish()
}

#[test]
fn test_non_ident() {
    let s = "!!! hello";
    assert_eq!(_non_ident(s), Ok(("hello", "!!! ".into())));
}

#[test]
fn test_text() {
    let s = "fn hello() 1";
    assert_eq!(
        _text(s),
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
