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
    pub(super) text: Vec<String>,
    line: Option<usize>,
    pub(super) scroll: usize,
    scroll_state: ScrollbarState,
    /// Cached height of rendered text area. Used for calculating scroll position.
    render_height: u16,
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
                .iter()
                .find(|li| li.byte_start <= ip32 && ip32 <= li.byte_end);
            if let Some(line_info) = line_info {
                let line = line_info.src_start as usize;
                self.line = Some(line);
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
            .border_set(border::THICK);

        let mut lines = vec![];
        if self.scroll < self.text.len() {
            lines.extend(self.text[self.scroll..].iter().enumerate().map(|(i, v)| {
                let line_num = i + self.scroll + 1;
                let v = style_text(Some(i + self.scroll + 1) == self.line, line_num, v);
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
