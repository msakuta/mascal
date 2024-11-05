use mascal::{FunctionInfo, Vm};
use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Color, Style, Stylize},
    symbols::{border, scrollbar},
    text::{Line, Text},
    widgets::{
        block::Title, Block, Paragraph, Scrollbar, ScrollbarOrientation, ScrollbarState,
        StatefulWidget, Widget,
    },
};

use super::view_settings::ViewSettings;

pub(super) struct StackWidget {
    text: Vec<VariableInfo>,
    scroll: usize,
    scroll_state: ScrollbarState,
    /// Cached height of rendered text area. Used for calculating scroll position.
    render_height: u16,
    pub(super) focus: bool,
}

struct VariableInfo {
    idx: usize,
    name: String,
    value: String,
}

impl StackWidget {
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
        vm: &Vm,
        level: usize,
        debug: Option<&FunctionInfo>,
        vs: &ViewSettings,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut buf = vec![];
        if let Some(iter) = vm.iter_stack(level) {
            for (i, value) in iter.enumerate() {
                let var_name = debug
                    .and_then(|debug| debug.vars.iter().find(|(_, idx)| **idx == i))
                    .map(|(name, _)| name);
                if vs.show_named_locals && var_name.is_none() {
                    continue;
                }
                buf.push(VariableInfo {
                    idx: i,
                    name: var_name.cloned().unwrap_or_default(),
                    value: value.to_string(),
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
        let inner = block.inner(area);

        let max_width = self.text.iter().fold(4, |acc, cur| acc.max(cur.name.len()));

        let header = vec![format!(
            "  Idx   {:1$}   Value{2:3$}",
            "Name", max_width, " ", inner.width as usize
        )
        .black()
        .bg(Color::Cyan)];

        let mut lines = vec![header.into()];
        if self.scroll < self.text.len() {
            lines.extend(self.text[self.scroll..].iter().map(|v| {
                Line::from(format!(
                    "  {i:3}   {name:name_len$}   {value}",
                    i = v.idx,
                    name = v.name,
                    name_len = max_width,
                    value = v.value
                ))
            }));
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
