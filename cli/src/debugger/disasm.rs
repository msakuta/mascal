use mascal::{Bytecode, FnBytecode, LineInfo};
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

pub(super) struct DisasmWidget {
    text: String,
    scroll: usize,
    scroll_state: ScrollbarState,
    /// Cached height of rendered text area. Used for calculating scroll position.
    render_height: u16,
    pub(super) focus: bool,
}

impl DisasmWidget {
    pub(super) fn new(bytecode: &Bytecode) -> Result<Self, Box<dyn std::error::Error>> {
        let mut temp = vec![];
        bytecode.disasm(&mut temp)?;
        let text = String::from_utf8(temp)?;
        let length = text.chars().filter(|c| *c == '\n').count();
        Ok(Self {
            text,
            scroll: 0,
            scroll_state: ScrollbarState::new(length),
            render_height: 10,
            focus: false,
        })
    }

    pub(super) fn update(
        &mut self,
        bytecode: &FnBytecode,
        ip: usize,
        debug: Option<&[LineInfo]>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut temp = String::new();
        let mut lines = 0;
        for (i, inst) in bytecode.iter_instructions().enumerate() {
            let current = if i == ip { "*" } else { " " };
            let line_num = debug.map_or_else(
                || "   ".to_string(),
                |debug| {
                    debug
                        .iter()
                        .find(|line_info: &&LineInfo| line_info.instruction == (i as u32))
                        .map_or_else(|| "    ".to_string(), |li| format!("{:04}", li.src_line))
                },
            );
            temp += &format!("{current}  {} [{}] {}\n", line_num, i, inst);
            lines += 1;
        }
        self.text = temp;
        // When the instruction pointer moves by stepping the program, we want to follow its position.
        self.scroll = self.scroll.clamp(
            (ip + 3).saturating_sub(self.render_height as usize),
            ip.saturating_sub(3),
        ); // Leave 3 lines before
        self.scroll_state = ScrollbarState::new(lines).position(self.scroll);
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

impl Widget for &mut DisasmWidget {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let text_lines: Vec<_> = self.text.split('\n').collect();
        let title =
            Title::from(format!(" Disassembly {}/{} ", self.scroll, text_lines.len()).bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .border_style(Style::new().yellow())
            .border_set(if self.focus {
                border::THICK
            } else {
                border::PLAIN
            });

        let mut lines = vec![];
        if self.scroll < text_lines.len() {
            lines.extend(text_lines[self.scroll..].iter().map(|v| Line::from(*v)));
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
