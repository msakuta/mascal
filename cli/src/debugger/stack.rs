use mascal::Vm;
use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Style, Stylize},
    symbols::border,
    text::{Line, Text},
    widgets::{block::Title, Block, Paragraph, ScrollbarState, Widget},
};

pub(super) struct StackWidget {
    text: String,
    scroll: usize,
    scroll_state: ScrollbarState,
    pub(super) focus: bool,
}

impl StackWidget {
    pub(super) fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            text: String::new(),
            scroll: 0,
            scroll_state: ScrollbarState::new(1),
            focus: false,
        })
    }

    pub(super) fn update(
        &mut self,
        vm: &Vm,
        level: usize,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut buf = vec![];
        vm.print_stack(&mut buf, level)?;
        self.text = String::from_utf8(buf)?;
        self.scroll_state =
            ScrollbarState::new(self.text.split('\n').count()).position(self.scroll);
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

impl Widget for &StackWidget {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let text_lines: Vec<_> = self.text.split('\n').collect();
        let title =
            Title::from(format!(" Stack values {}/{} ", self.scroll, text_lines.len()).bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .border_style(Style::new().cyan())
            .border_set(if self.focus {
                border::THICK
            } else {
                border::PLAIN
            });

        let mut lines = vec![];
        if self.scroll < text_lines.len() {
            lines.extend(text_lines[self.scroll..].iter().map(|v| Line::from(*v)));
        }

        Paragraph::new(Text::from(lines))
            .block(block)
            .render(area, buf);
    }
}
