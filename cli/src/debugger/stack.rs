use mascal::{FunctionInfo, Vm};
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

pub(super) struct StackWidget {
    text: Vec<String>,
    scroll: usize,
    scroll_state: ScrollbarState,
    /// Cached height of rendered text area. Used for calculating scroll position.
    render_height: u16,
    pub(super) focus: bool,
    named_only: bool,
}

impl StackWidget {
    pub(super) fn new(named_only: bool) -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            text: vec![],
            scroll: 0,
            scroll_state: ScrollbarState::new(1),
            render_height: 10,
            focus: false,
            named_only,
        })
    }

    pub(super) fn update(
        &mut self,
        vm: &Vm,
        level: usize,
        debug: Option<&FunctionInfo>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut buf = vec![];
        if let Some(iter) = vm.iter_stack(level) {
            for (i, value) in iter.enumerate() {
                let var_name = debug
                    .and_then(|debug| debug.vars.iter().find(|(_, idx)| **idx == i))
                    .map(|(name, _)| name);
                if self.named_only && var_name.is_none() {
                    continue;
                }
                buf.push(if let Some(var_name) = var_name {
                    format!("  [{i}] {value}    (Local: {var_name})")
                } else {
                    format!("  [{i}] {value}")
                });
            }
        }

        self.text = buf;
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

    pub(super) fn toggle_named_only(&mut self) {
        self.named_only = !self.named_only;
    }
}

impl Widget for &mut StackWidget {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let title =
            Title::from(format!(" Stack values {}/{} ", self.scroll, self.text.len()).bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .border_style(Style::new().cyan())
            .border_set(if self.focus {
                border::THICK
            } else {
                border::PLAIN
            });

        let mut lines = vec![];
        if self.scroll < self.text.len() {
            lines.extend(
                self.text[self.scroll..]
                    .iter()
                    .map(|v| Line::from(v.as_str())),
            );
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
