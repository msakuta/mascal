mod parser;

use std::collections::HashSet;

use mascal::{Bytecode, LineInfo};
use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Color, Style, Stylize},
    symbols::{border, scrollbar},
    text::{Line, Span, Text},
    widgets::{
        block::Title, Block, Paragraph, Scrollbar, ScrollbarOrientation, ScrollbarState,
        StatefulWidget, Widget,
    },
};

use self::parser::{style_text, Lexer};

pub(super) struct SourceListWidget {
    text: Vec<(String, Lexer)>,
    line: Option<LineInfo>,
    scroll: usize,
    scroll_state: ScrollbarState,
    cursor: usize,
    /// Cached height of rendered text area. Used for calculating scroll position.
    render_height: u16,
    pub(super) focus: bool,
    pub(super) breakpoints: HashSet<usize>,
}

impl SourceListWidget {
    pub(super) fn new(bytecode: &Bytecode) -> (Self, Option<Box<dyn std::error::Error>>) {
        let mut temp = String::new();
        let mut error = None;
        if let Some(source_file) = bytecode.debug_info().map(|d| d.file_name()) {
            // Even if we fail to read the source file, we still create widget, just for the sake of laying out,
            // with an empty content. However, we want to communicate the error up to the user.
            match std::fs::read_to_string(source_file) {
                Ok(source) => temp = source,
                Err(e) => error = Some(e.into()),
            }
        }

        let mut lexer = Lexer::new();
        let text: Vec<_> = temp
            .split("\n")
            .enumerate()
            .map(|(i, s)| {
                let start_state = lexer;
                style_text(&mut lexer, None, &HashSet::new(), i, s);
                (s.trim_end().to_string(), start_state)
            })
            .collect();

        let length = text.len();
        (
            Self {
                text,
                line: None,
                scroll: 0,
                scroll_state: ScrollbarState::new(length),
                cursor: 0,
                render_height: 10,
                focus: true,
                breakpoints: HashSet::new(),
            },
            error,
        )
    }

    pub(super) fn update(
        &mut self,
        ip: usize,
        debug: Option<&[LineInfo]>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        if let Some(debug) = debug {
            let ip32 = ip as u32;
            let line_info = debug
                .binary_search_by_key(&ip32, |li| li.instruction)
                .map_or_else(|res| res, |res| res);
            if let Some(line_info) = debug.get(line_info) {
                self.line = Some(*line_info);
                let line = line_info.src_line as usize;
                // When the instruction pointer moves by stepping the program, we want to follow its position.
                self.scroll = self.scroll.clamp(
                    (line + 3).saturating_sub(self.render_height as usize),
                    line.saturating_sub(3),
                ); // Leave 3 lines before
                self.scroll_state = ScrollbarState::new(self.text.len()).position(self.scroll);
            }
        }
        Ok(())
    }

    pub(super) fn update_scroll(&mut self, delta: i32) {
        if delta < 0 {
            // The line number starts with 1
            self.cursor = self.cursor.saturating_sub(delta.abs() as usize).max(1);
        } else {
            self.cursor = self
                .cursor
                .saturating_add(delta as usize)
                .min(self.text.len());
        }
        let ofs = self.cursor.saturating_sub(1);
        self.scroll = self.scroll.clamp(
            (ofs + 3).saturating_sub(self.render_height as usize),
            ofs.saturating_sub(3),
        );
        self.scroll_state = self.scroll_state.position(self.scroll);
    }

    /// Toggle a breakpoint at the cursor line.
    pub(super) fn toggle_breakpoint(&mut self) {
        if self.breakpoints.contains(&self.cursor) {
            self.breakpoints.remove(&self.cursor);
        } else {
            self.breakpoints.insert(self.cursor);
        }
    }

    /// Check if we should stop execution at `ip` with a breakpoint. If we hit another breakpoint in the same line,
    /// we want to skip it, so we use `skip_line` to indicate the starting line.
    pub(super) fn check_breakpoint(
        &self,
        ip: usize,
        debug: Option<&[LineInfo]>,
        skip_line: Option<u32>,
    ) -> bool {
        if let Some(debug) = debug {
            let ip32 = ip as u32;
            let line_info = debug
                .binary_search_by_key(&ip32, |li| li.instruction)
                .map_or_else(|res| res, |res| res);
            if let Some(line_info) = debug.get(line_info) {
                return skip_line != Some(line_info.src_line)
                    && self.breakpoints.contains(&(line_info.src_line as usize));
            }
        }
        false
    }
}

impl Widget for &mut SourceListWidget {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let title =
            Title::from(format!(" Source listing {}/{} ", self.scroll, self.text.len()).bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .border_style(Style::new().white())
            .border_set(if self.focus {
                border::THICK
            } else {
                border::PLAIN
            });

        let inner = block.inner(area);
        let height = inner.height as usize;
        let lines_end = (self.scroll + height).min(self.text.len().saturating_sub(1));

        let mut lines = vec![];
        if self.scroll < self.text.len() && lines_end <= self.text.len() {
            lines.extend(self.text[self.scroll..lines_end].iter().enumerate().map(
                |(i, (s, lexer))| {
                    let mut lexer = *lexer;
                    let line_num = i + self.scroll + 1;
                    let mut v = style_text(
                        &mut lexer,
                        self.line.as_ref(),
                        &self.breakpoints,
                        line_num,
                        s,
                    );
                    if line_num == self.cursor {
                        for span in v.iter_mut() {
                            *span = span.clone().bg(Color::DarkGray);
                        }
                        v.push(Span::from(" ".repeat(area.width as usize)).bg(Color::DarkGray));
                    }
                    Line::from(v)
                },
            ));
        }

        let sbar = Scrollbar::new(ScrollbarOrientation::VerticalRight)
            .symbols(scrollbar::VERTICAL)
            .begin_symbol(None)
            .track_symbol(None)
            .end_symbol(None);

        Paragraph::new(Text::from(lines))
            .block(block)
            .render(area, buf);

        sbar.render(inner, buf, &mut self.scroll_state);

        self.render_height = inner.height;
    }
}
