mod parser;

use mascal::{Bytecode, LineInfo};
use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Style, Stylize},
    symbols::{border, scrollbar},
    text::{Line, Text},
    widgets::{
        block::Title, Block, Paragraph, Scrollbar, ScrollbarOrientation, ScrollbarState,
        StatefulWidget, Widget,
    },
};

use self::parser::style_text;

pub(super) struct SourceListWidget {
    pub(super) visible: bool,
    text: Vec<String>,
    line: Option<LineInfo>,
    scroll: usize,
    scroll_state: ScrollbarState,
    /// Cached height of rendered text area. Used for calculating scroll position.
    render_height: u16,
    pub(super) focus: bool,
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
        let text: Vec<String> = temp.split("\n").map(|s| s.trim_end().to_string()).collect();
        let length = text.len();
        (
            Self {
                visible: true,
                text,
                line: None,
                scroll: 0,
                scroll_state: ScrollbarState::new(length),
                render_height: 10,
                focus: true,
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
            self.scroll = self.scroll.saturating_sub(delta.abs() as usize);
        } else {
            self.scroll = self.scroll.saturating_add(delta as usize);
        }
        self.scroll_state = self.scroll_state.position(self.scroll);
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

        let mut lines = vec![];
        if self.scroll < self.text.len() {
            lines.extend(self.text[self.scroll..].iter().enumerate().map(|(i, v)| {
                let line_num = i + self.scroll + 1;
                let v = style_text(self.line.as_ref(), line_num, v);
                Line::from(v)
            }));
        }

        let sbar = Scrollbar::new(ScrollbarOrientation::VerticalRight)
            .symbols(scrollbar::VERTICAL)
            .begin_symbol(None)
            .track_symbol(None)
            .end_symbol(None);
        let inner = block.inner(area);

        Paragraph::new(Text::from(lines))
            .block(block)
            .render(area, buf);

        sbar.render(inner, buf, &mut self.scroll_state);

        self.render_height = inner.height;
    }
}
