use mascal::{Bytecode, FnBytecode, LineInfo};
use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Color, Style, Stylize},
    symbols::{border, scrollbar},
    text::Text,
    widgets::{
        block::Title, Block, Paragraph, Scrollbar, ScrollbarOrientation, ScrollbarState,
        StatefulWidget, Widget,
    },
};

pub(super) struct DisasmWidget {
    text: DisasmText,
    scroll: usize,
    scroll_state: ScrollbarState,
    /// Cached height of rendered text area. Used for calculating scroll position.
    render_height: u16,
    pub(super) focus: bool,
}

enum DisasmText {
    Source(String),
    Function(Vec<String>),
}

impl DisasmWidget {
    pub(super) fn new(bytecode: &Bytecode) -> Result<Self, Box<dyn std::error::Error>> {
        let mut temp = vec![];
        bytecode.disasm(&mut temp)?;
        let source = String::from_utf8(temp)?;
        let length = source.chars().filter(|c| *c == '\n').count();
        let text = DisasmText::Source(source);
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
        let mut text = vec![];
        let mut lines = 0;
        for (i, inst) in bytecode.iter_instructions().enumerate() {
            let current = if i == ip { "*" } else { " " };
            let line_num = debug
                .and_then(|debug| {
                    debug
                        .iter()
                        .find(|line_info: &&LineInfo| line_info.instruction == (i as u32))
                })
                .map_or_else(|| "    ".to_string(), |li| format!("{:4}", li.src_line));
            text.push(format!(
                "{current}  {line_num}   {i:4}   {op:12}   {arg0:3}   {arg1:3}\n",
                op = inst.op().to_string(),
                arg0 = inst.arg0(),
                arg1 = inst.arg1()
            ));
            lines += 1;
        }
        self.text = DisasmText::Function(text);
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
            self.scroll = self.scroll.saturating_sub(delta.unsigned_abs() as usize);
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
        let line_count = match self.text {
            DisasmText::Source(ref text) => text.split('\n').count(),
            DisasmText::Function(ref text) => text.len(),
        };
        let title = Title::from(format!(" Disassembly {}/{} ", self.scroll, line_count).bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .border_style(Style::new().yellow())
            .border_set(if self.focus {
                border::THICK
            } else {
                border::PLAIN
            });
        let inner = block.inner(area);

        let mut lines;
        let text_lines;
        match self.text {
            DisasmText::Source(ref text) => {
                text_lines = text.split('\n').map(|s| s.to_string()).collect();
                lines = vec![];
            }
            DisasmText::Function(ref text) => {
                let header = vec![format!(
                    "  Line#    Idx   OpCode        Arg0  Arg1 {:1$}",
                    " ", inner.width as usize
                )
                .black()
                .bg(Color::Gray)];

                text_lines = text.clone();
                lines = vec![header.into()];
            }
        }

        if self.scroll < line_count {
            let end = self.scroll + inner.height.saturating_sub(1) as usize;

            lines.extend(
                text_lines[self.scroll..end.min(text_lines.len())]
                    .iter()
                    .map(|v| v.as_str().into()),
            );
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
