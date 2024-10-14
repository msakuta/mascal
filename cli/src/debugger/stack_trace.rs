use mascal::Vm;
use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Style, Stylize},
    symbols::border,
    text::{Line, Text},
    widgets::{block::Title, Block, Paragraph, Widget},
};

pub(super) struct StackTraceWidget {
    text: String,
    scroll: usize,
}

impl StackTraceWidget {
    pub(super) fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            text: String::new(),
            scroll: 0,
        })
    }

    pub(super) fn update(&mut self, vm: &Vm) -> Result<(), Box<dyn std::error::Error>> {
        let mut buf = vec![];
        vm.stack_trace(&mut buf)?;
        self.text = String::from_utf8(buf)?;
        Ok(())
    }
}

impl Widget for &StackTraceWidget {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let text_lines: Vec<_> = self.text.split('\n').collect();
        let title =
            Title::from(format!(" Stack trace {}/{} ", self.scroll, text_lines.len()).bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .border_style(Style::new().blue())
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
