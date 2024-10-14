//! Implementation of the interactive debugger.

use ratatui::{
    buffer::Buffer,
    crossterm::event::{self, KeyCode, KeyEventKind},
    layout::{Alignment, Rect},
    style::Stylize,
    symbols::border,
    text::{Line, Text},
    widgets::{
        block::{Position, Title},
        Block, Paragraph, Widget,
    },
    DefaultTerminal, Frame,
};

use ::mascal::Bytecode;

pub(crate) fn run_debugger(bytecode: &Bytecode) -> Result<(), Box<dyn std::error::Error>> {
    let mut terminal = ratatui::init();
    terminal.clear()?;
    let app_result = App::new(bytecode).run(terminal);
    ratatui::restore();
    // Error out after restore
    app_result
}

struct App<'a> {
    bytecode: &'a Bytecode,
    mode: AppMode,
    exit: bool,
}

impl<'a> App<'a> {
    fn new(bytecode: &'a Bytecode) -> Self {
        Self {
            bytecode,
            mode: Default::default(),
            exit: false,
        }
    }

    fn run(&mut self, mut terminal: DefaultTerminal) -> Result<(), Box<dyn std::error::Error>> {
        while !self.exit {
            terminal.draw(|frame| self.draw(frame))?;
            self.handle_events()?;
        }
        Ok(())
    }

    fn draw(&self, frame: &mut Frame) {
        frame.render_widget(self, frame.area());
    }

    fn handle_events(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        if let event::Event::Key(key) = event::read()? {
            if key.kind == KeyEventKind::Press && key.code == KeyCode::Char('d') {
                let mut temp = vec![];
                self.bytecode.disasm(&mut temp)?;
                self.mode = AppMode::Disasm {
                    text: String::from_utf8(temp)?,
                    scroll: 0,
                };
            }
            if key.kind == KeyEventKind::Press && matches!(key.code, KeyCode::Char('q' | 'Q')) {
                if !matches!(self.mode, AppMode::None) {
                    self.mode = AppMode::None;
                } else {
                    self.exit = true;
                }
            }
            if key.kind == KeyEventKind::Press {
                match key.code {
                    KeyCode::Up => {
                        if let AppMode::Disasm { ref mut scroll, .. } = self.mode {
                            *scroll = scroll.saturating_sub(1)
                        }
                    }
                    KeyCode::Down => {
                        if let AppMode::Disasm { ref mut scroll, .. } = self.mode {
                            *scroll += 1
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }
}

impl<'a> Widget for &App<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" Interactive debugger ".bold());
        let instructions = Title::from(Line::from(vec![
            " disassemble current code: ".into(),
            "d".blue().bold(),
            "  run current code: ".into(),
            "r".blue().bold(),
            "  quit: ".into(),
            "q".blue().bold(),
        ]));
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .title(
                instructions
                    .alignment(Alignment::Center)
                    .position(Position::Bottom),
            )
            .border_set(border::THICK);

        let disasm_msg = "Showing disassembly (q to quit)";

        let inner_text = match &self.mode {
            AppMode::None => Text::from(vec![Line::from("Press Q to exit")]),
            AppMode::Disasm { text, scroll } => {
                let text_lines: Vec<_> = text.split('\n').collect();
                let mut lines = vec![format!("{disasm_msg}: {scroll}/{}", text_lines.len()).into()];
                if *scroll < text_lines.len() {
                    lines.extend(text_lines[*scroll..].iter().map(|v| Line::from(*v)));
                }
                Text::from(lines)
            }
        };

        Paragraph::new(inner_text).block(block).render(area, buf);
    }
}

#[derive(Default)]
enum AppMode {
    #[default]
    None,
    Disasm {
        text: String,
        scroll: usize,
    },
}
