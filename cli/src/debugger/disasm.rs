use mascal::{Bytecode, FnBytecode};
use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Style, Stylize},
    symbols::border,
    text::{Line, Text},
    widgets::{block::Title, Block, Paragraph, Widget},
};

pub(super) struct DisasmWidget {
    pub(super) text: String,
    pub(super) scroll: usize,
}

impl DisasmWidget {
    pub(super) fn new(bytecode: &Bytecode) -> Result<Self, Box<dyn std::error::Error>> {
        let mut temp = vec![];
        bytecode.disasm(&mut temp)?;
        Ok(Self {
            text: String::from_utf8(temp)?,
            scroll: 0,
        })
    }

    pub(super) fn update(
        &mut self,
        bytecode: &FnBytecode,
        ip: usize,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut temp = String::new();
        for (i, inst) in bytecode.iter_instructions().enumerate() {
            let current = if i == ip { "*" } else { " " };
            temp += &format!("{current}  [{}] {}\n", i, inst);
        }
        self.text = temp;
        self.scroll = ip.saturating_sub(3); // Leave 3 lines before
        Ok(())
    }
}

impl Widget for &DisasmWidget {
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
