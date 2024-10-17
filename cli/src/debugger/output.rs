use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Style, Stylize},
    symbols::border,
    text::{Line, Text},
    widgets::{block::Title, Block, Paragraph, Widget},
};

pub(super) struct OutputWidget {
    text: String,
    scroll: usize,
    visible: bool,
}

impl OutputWidget {
    pub(super) fn new() -> Self {
        Self {
            text: String::new(),
            scroll: 0,
            visible: true,
        }
    }

    pub(super) fn update(
        &mut self,
        output_buffer: &[u8],
    ) -> Result<(), Box<dyn std::error::Error>> {
        self.text = String::from_utf8(output_buffer.to_vec())
            .unwrap_or_else(|_| "Failed to decode output as utf-8".to_string());
        Ok(())
    }

    pub(super) fn toggle_visible(&mut self) {
        self.visible = !self.visible;
    }

    pub(super) fn visible(&self) -> bool {
        self.visible
    }
}

impl Widget for &OutputWidget {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let text_lines: Vec<_> = self.text.split('\n').collect();
        let title = Title::from(format!(" Output {}/{} ", self.scroll, text_lines.len()).bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .border_style(Style::new().magenta())
            .border_set(border::THICK);

        let mut lines = vec![];
        if self.scroll < text_lines.len() {
            lines.extend(text_lines[self.scroll..].iter().map(|v| Line::from(*v)));
        }

        Paragraph::new(Text::from(lines))
            .block(block)
            .render(area, buf);
    }
}
