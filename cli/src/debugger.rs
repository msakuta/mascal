//! Implementation of the interactive debugger.

use std::cell::RefCell;

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
    disasm: Option<DisasmWidget>,
    exit: bool,
}

impl<'a> App<'a> {
    fn new(bytecode: &'a Bytecode) -> Self {
        Self {
            bytecode,
            mode: Default::default(),
            disasm: None,
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
                    if self.disasm.is_some() {
                        self.disasm = None;
                    } else {
                        self.disasm = DisasmWidget::new(self.bytecode).ok();
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('r')) => {
                    interpret(self.bytecode)?;
                }
                (KeyEventKind::Press, KeyCode::Char('s')) => {
                    if let AppMode::StepRun {
                        ref mut vm,
                        ref mut error,
                    } = self.mode
                    {
                        match vm.next_inst() {
                            Ok(_) => *error.borrow_mut() = None,
                            Err(e) => *error.borrow_mut() = Some(e.to_string()),
                        }
                    } else {
                        self.mode = AppMode::StepRun {
                            vm: Vm::start_main(self.bytecode)?,
                            error: RefCell::new(None),
                        };
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('q' | 'Q')) => {
                    if !matches!(self.mode, AppMode::None) {
                        self.mode = AppMode::None;
                    } else {
                        self.exit = true;
                    }
                }
                (KeyEventKind::Press, KeyCode::Up) => {
                    if let Some(ref mut da) = self.disasm {
                        da.scroll = da.scroll.saturating_sub(1)
                    }
                }
                (KeyEventKind::Press, KeyCode::Down) => {
                    if let Some(ref mut da) = self.disasm {
                        da.scroll += 1
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

        let inner_text = match &self.mode {
            AppMode::None => Text::from(vec![Line::from("Press Q to exit")]),
            AppMode::StepRun { vm, error } => {
                if let Some(call_info) = vm.top_call_info() {
                    let mut lines =
                        vec![format!("Execution state at {}", call_info.instuction_ptr()).into()];

                    let mut buf = vec![];
                    if let Err(e) = vm.dump_stack(&mut buf) {
                        *error.borrow_mut() = Some(e.to_string());
                    }
                    if let Ok(s) = String::from_utf8(buf) {
                        lines.push(s.into());
                    }

                    let mut buf = vec![];
                    if let Err(e) = vm.format_current_inst(&mut buf) {
                        *error.borrow_mut() = Some(e.to_string());
                    }
                    if let Ok(s) = String::from_utf8(buf) {
                        lines.push(s.into());
                    }

                    if let Some(error) = error.borrow().as_ref() {
                        lines.push(format!("Error: {error}").into());
                    }
                    Text::from(lines)
                } else {
                    Text::from("Step exection has not started yet.")
                }
            }
        };

        Paragraph::new(inner_text).block(block).render(area, buf);

        let mut disasm_area = area;
        disasm_area.y = disasm_area.height / 2;
        disasm_area.height /= 2;

        self.disasm.as_ref().map(|d| d.render(disasm_area, buf));
    }
}

#[derive(Default)]
enum AppMode<'a> {
    #[default]
    None,
    StepRun {
        vm: Vm<'a>,
        error: RefCell<Option<String>>,
    },
}

struct DisasmWidget {
    text: String,
    scroll: usize,
}

impl DisasmWidget {
    fn new(bytecode: &Bytecode) -> Result<Self, Box<dyn std::error::Error>> {
        let mut temp = vec![];
        bytecode.disasm(&mut temp)?;
        Ok(Self {
            text: String::from_utf8(temp)?,
            scroll: 0,
        })
    }
}

impl Widget for &DisasmWidget {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let title = Title::from(" Disassembly ".bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .border_set(border::THICK);

        let text_lines: Vec<_> = self.text.split('\n').collect();
        let mut lines = vec![format!("{}/{}", self.scroll, text_lines.len()).into()];
        if self.scroll < text_lines.len() {
            lines.extend(text_lines[self.scroll..].iter().map(|v| Line::from(*v)));
        }

        Paragraph::new(Text::from(lines))
            .block(block)
            .render(area, buf);
    }
}
