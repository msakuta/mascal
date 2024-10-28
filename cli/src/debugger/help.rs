use ratatui::{
    buffer::Buffer,
    layout::{Alignment, Rect},
    style::{Color, Style, Stylize},
    symbols::border,
    text::{Line, Text},
    widgets::{block::Title, Block, Clear, Paragraph, Widget},
};

pub(super) struct HelpWidget {}

impl HelpWidget {
    pub(super) fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {})
    }
}

impl Widget for &HelpWidget {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let text_lines = Text::from(vec![
            Line::from(vec![
                "  Toggle help (this window): ".into(),
                "h".blue().bold(),
            ]),
            Line::from(vec!["  Toggle source list: ".into(), "l".blue().bold()]),
            Line::from(vec!["  Toggle disassembly: ".into(), "D".blue().bold()]),
            Line::from(vec!["  Toggle stack trace: ".into(), "t".blue().bold()]),
            Line::from(vec![
                "  Toggle local stack values: ".into(),
                "k".blue().bold(),
            ]),
            Line::from(vec!["  Toggle output widget: ".into(), "o".blue().bold()]),
            Line::from(vec![
                "  Change widget focus to scroll: ".into(),
                "Tab".blue().bold(),
            ]),
            Line::from(vec![
                "  Scroll up current widget: ".into(),
                "up".blue().bold(),
            ]),
            Line::from(vec![
                "  Scroll down current widget: ".into(),
                "down".blue().bold(),
            ]),
            Line::from(vec!["  run current code: ".into(), "r".blue().bold()]),
            Line::from(vec![
                "  Continue until a breakpoint or the end of the program: ".into(),
                "c".blue().bold(),
            ]),
            Line::from(vec!["  Step execution mode: ".into(), "s".blue().bold()]),
            Line::from(vec!["  Move up stack frame: ".into(), "u".blue().bold()]),
            Line::from(vec!["  Move down stack frame: ".into(), "d".blue().bold()]),
            Line::from(vec![
                "  Previous state in time travel debugger: ".into(),
                "p".blue().bold(),
            ]),
            Line::from(vec![
                "  Next state in time travel debugger: ".into(),
                "n".blue().bold(),
            ]),
            Line::from(vec![
                "  quit or stop current debugging session: ".into(),
                "q, esc ".blue().bold(),
            ]),
        ]);
        let title = Title::from(" Help ".bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            // .bg(Color::Blue)
            .style(Style::default().bg(Color::DarkGray))
            .border_style(Style::new().white())
            .border_set(border::THICK);

        Clear.render(area, buf);

        Paragraph::new(text_lines).block(block).render(area, buf);
    }
}
