//! Implementation of the interactive debugger.

use mascal::{interpret, Vm};
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
    mode: AppMode<'a>,
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
            match (key.kind, key.code) {
                (KeyEventKind::Press, KeyCode::Char('d')) => {
                    let mut temp = vec![];
                    self.bytecode.disasm(&mut temp)?;
                    self.mode = AppMode::Disasm {
                        text: String::from_utf8(temp)?,
                        scroll: 0,
                    };
                }
                (KeyEventKind::Press, KeyCode::Char('r')) => {
                    interpret(self.bytecode)?;
                }
                (KeyEventKind::Press, KeyCode::Char('s')) => {
                    self.mode = AppMode::StepRun {
                        vm: Vm::start_main(self.bytecode)?,
                    };
                }
                (KeyEventKind::Press, KeyCode::Char('q' | 'Q')) => {
                    if !matches!(self.mode, AppMode::None) {
                        self.mode = AppMode::None;
                    } else {
                        self.exit = true;
                    }
                }
                (KeyEventKind::Press, KeyCode::Up) => {
                    if let AppMode::Disasm { ref mut scroll, .. } = self.mode {
                        *scroll = scroll.saturating_sub(1)
                    }
                }
                (KeyEventKind::Press, KeyCode::Down) => {
                    if let AppMode::Disasm { ref mut scroll, .. } = self.mode {
                        *scroll += 1
                    }
                }
                _ => {}
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
            "  Step execution mode: ".into(),
            "s".blue().bold(),
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
            AppMode::StepRun { vm } => {
                if let Some(call_info) = vm.top_call_info() {
                    Text::from(vec![format!(
                        "Execution state at {}",
                        call_info.instuction_ptr()
                    )
                    .into()])
                } else {
                    Text::from("Step exection has not started yet.")
                }
            }
        };

        Paragraph::new(inner_text).block(block).render(area, buf);
    }
}

#[derive(Default)]
enum AppMode<'a> {
    #[default]
    None,
    Disasm {
        text: String,
        scroll: usize,
    },
    StepRun {
        vm: Vm<'a>,
    },
}
