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

pub(super) struct OutputWidget {
    text: Vec<String>,
    scroll: usize,
    scroll_state: ScrollbarState,
    /// Cached height of rendered text area. Used for calculating scroll position.
    render_height: u16,
    pub(super) focus: bool,
}

impl OutputWidget {
    pub(super) fn new() -> Self {
        Self {
            text: vec![],
            scroll: 0,
            scroll_state: ScrollbarState::new(1),
            render_height: 10,
            focus: false,
        }
    }

    pub(super) fn update(
        &mut self,
        output_buffer: &[u8],
    ) -> Result<(), Box<dyn std::error::Error>> {
        self.text = String::from_utf8(output_buffer.to_vec())
            .unwrap_or_else(|_| "Failed to decode output as utf-8".to_string())
            .split("\n")
            .map(|s| s.to_string())
            .collect();

        // Show the latest line at the bottom
        self.scroll = self.scroll.clamp(
            self.text
                .len()
                .saturating_sub(self.render_height as usize + 1),
            self.text.len(),
        );

        self.scroll_state = ScrollbarState::new(self.text.len()).position(self.scroll);
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

impl Widget for &mut OutputWidget {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let text_lines = &self.text;
        let title = Title::from(format!(" Output {}/{} ", self.scroll, text_lines.len()).bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .border_style(Style::new().magenta())
            .border_set(if self.focus {
                border::THICK
            } else {
                border::PLAIN
            });

        let sbar = Scrollbar::new(ScrollbarOrientation::VerticalRight)
            .symbols(scrollbar::VERTICAL)
            .begin_symbol(None)
            .track_symbol(None)
            .end_symbol(None);
        let inner = block.inner(area);

        let mut lines = vec![];
        if self.scroll < text_lines.len() {
            lines.extend(
                text_lines[self.scroll..]
                    .iter()
                    .map(|v| Line::from(v.as_str())),
            );
        }

        Paragraph::new(Text::from(lines))
            .block(block)
            .render(area, buf);

        sbar.render(inner, buf, &mut self.scroll_state);

        self.render_height = inner.height;
    }
}
