//! Implementation of the interactive debugger.

mod disasm;
mod stack;
mod stack_trace;

use std::cell::RefCell;

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

use ::mascal::{interpret, Bytecode, Vm};

use self::{disasm::DisasmWidget, stack::StackWidget, stack_trace::StackTraceWidget};

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
    stack: Option<StackWidget>,
    exit: bool,
}

impl<'a> App<'a> {
    fn new(bytecode: &'a Bytecode) -> Self {
        Self {
            bytecode,
            mode: Default::default(),
            disasm: DisasmWidget::new(bytecode).ok(),
            stack_trace: StackTraceWidget::new().ok(),
            stack: StackWidget::new().ok(),
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
                (KeyEventKind::Press, KeyCode::Char('l')) => {
                    if self.stack.is_some() {
                        self.stack = None;
                    } else {
                        self.stack = StackWidget::new().ok();
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
                        if let Some(ref mut stack) = self.stack {
                            if let Err(e) = stack.update(vm) {
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
            "  disassemby: ".into(),
            "d".blue().bold(),
            "  stack trace: ".into(),
            "t".blue().bold(),
            "  Show local stack values: ".into(),
            "l".blue().bold(),
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

        let mut tr_area = area;
        if 0 < tr_area.height {
            tr_area.x = area.width / 2;
            tr_area.width /= 2;
            tr_area.y += 1;
            tr_area.height = (tr_area.height - 1) / 2;
            self.stack.as_ref().map(|d| d.render(tr_area, buf));
        }

        let mut widget_area = area;
        if 0 < widget_area.height {
            widget_area.y = widget_area.height / 2;
            widget_area.height = (widget_area.height - 1) / 2;

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
