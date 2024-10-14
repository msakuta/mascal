//! Implementation of the interactive debugger.

use std::cell::RefCell;

use mascal::{interpret, FnBytecode, Vm};
use ratatui::{
    buffer::Buffer,
    crossterm::event::{self, KeyCode, KeyEventKind},
    layout::{Alignment, Rect},
    style::{Style, Stylize},
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
    stack_trace: Option<StackTraceWidget>,
    exit: bool,
}

impl<'a> App<'a> {
    fn new(bytecode: &'a Bytecode) -> Self {
        Self {
            bytecode,
            mode: Default::default(),
            disasm: None,
            stack_trace: None,
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
                (KeyEventKind::Press, KeyCode::Char('t')) => {
                    if self.stack_trace.is_some() {
                        self.stack_trace = None;
                    } else {
                        self.stack_trace = StackTraceWidget::new().ok();
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
                        if let Some(ref mut disasm) = self.disasm {
                            if let Some(ci) = vm.top_call_info() {
                                let res = disasm.update(ci.bytecode(), ci.instuction_ptr());
                                if let Err(e) = res {
                                    *error.borrow_mut() = Some(e.to_string());
                                }
                            }
                        }
                        if let Some(ref mut stack_trace) = self.stack_trace {
                            if let Err(e) = stack_trace.update(vm) {
                                *error.borrow_mut() = Some(e.to_string());
                            }
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
            " disassemby: ".into(),
            "d".blue().bold(),
            " stack trace: ".into(),
            "t".blue().bold(),
            "  run current code: ".into(),
            "r".blue().bold(),
            "  Step execution mode: ".into(),
            "s".blue().bold(),
            "  quit: ".into(),
            "q ".blue().bold(),
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

        let mut widget_area = area;
        widget_area.y = widget_area.height / 2;
        widget_area.height /= 2;

        let widget_count = self.disasm.is_some() as u16 + self.stack_trace.is_some() as u16;

        if widget_count != 0 {
            widget_area.width /= widget_count;
        }

        if let Some(d) = self.disasm.as_ref() {
            d.render(widget_area, buf);
            widget_area.x += widget_area.width;
        }
        self.stack_trace
            .as_ref()
            .map(|d| d.render(widget_area, buf));
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

    fn update(
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

struct StackTraceWidget {
    text: String,
    scroll: usize,
}

impl StackTraceWidget {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            text: String::new(),
            scroll: 0,
        })
    }

    fn update(&mut self, vm: &Vm) -> Result<(), Box<dyn std::error::Error>> {
        let mut buf = vec![];
        vm.stack_trace(&mut buf)?;
        self.text = String::from_utf8(buf)?;
        Ok(())
    }
}

impl Widget for &StackTraceWidget {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        let text_lines: Vec<_> = self.text.split('\n').collect();
        let title =
            Title::from(format!(" Stack trace {}/{} ", self.scroll, text_lines.len()).bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .border_style(Style::new().blue())
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
