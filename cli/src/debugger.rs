//! Implementation of the interactive debugger.

mod disasm;
mod help;
mod stack;
mod stack_trace;

use std::{cell::RefCell, collections::VecDeque};

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

use self::{
    disasm::DisasmWidget, help::HelpWidget, stack::StackWidget, stack_trace::StackTraceWidget,
};

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
    error: RefCell<Option<String>>,
    widgets: Widgets,
    exit: bool,
}

struct Widgets {
    disasm: Option<DisasmWidget>,
    stack_trace: Option<StackTraceWidget>,
    stack: Option<StackWidget>,
    help: Option<HelpWidget>,
}

impl<'a> App<'a> {
    fn new(bytecode: &'a Bytecode) -> Self {
        Self {
            bytecode,
            mode: Default::default(),
            error: RefCell::new(None),
            widgets: Widgets {
                disasm: DisasmWidget::new(bytecode).ok(),
                stack_trace: StackTraceWidget::new().ok(),
                stack: StackWidget::new().ok(),
                help: None,
            },
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
        // Consume the error and store it to a message variable, since TUI has no stdout to log the error.
        if let Err(e) = self.inner_events() {
            *self.error.get_mut() = Some(e.to_string());
        }
        return Ok(());
    }

    fn inner_events(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        if let event::Event::Key(key) = event::read()? {
            match (key.kind, key.code) {
                (KeyEventKind::Press, KeyCode::Char('D')) => {
                    if self.widgets.disasm.is_some() {
                        self.widgets.disasm = None;
                    } else {
                        self.widgets.disasm = DisasmWidget::new(self.bytecode).ok();
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('t')) => {
                    if self.widgets.stack_trace.is_some() {
                        self.widgets.stack_trace = None;
                    } else {
                        self.widgets.stack_trace = StackTraceWidget::new().ok();
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('l')) => {
                    if self.widgets.stack.is_some() {
                        self.widgets.stack = None;
                    } else {
                        self.widgets.stack = StackWidget::new().ok();
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('h')) => {
                    if self.widgets.help.is_some() {
                        self.widgets.help = None;
                    } else {
                        self.widgets.help = HelpWidget::new().ok();
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('r')) => {
                    interpret(self.bytecode)?;
                }
                (KeyEventKind::Press, KeyCode::Char('s')) => {
                    if let AppMode::StepRun {
                        ref mut vm_history,
                        btrace_level,
                        ref mut selected_history,
                    } = self.mode
                    {
                        let Some(last_vm) = vm_history.front() else {
                            return Err("Missing Vm".into());
                        };
                        let mut next_vm = last_vm.deepclone();
                        next_vm.next_inst()?;
                        self.widgets.update(&mut next_vm, btrace_level)?;
                        vm_history.push_front(next_vm);
                        if 100 < vm_history.len() {
                            vm_history.pop_back();
                        }

                        // Reset history to most recent to reflect real time state
                        *selected_history = 0;
                    } else {
                        let mut vm_history = VecDeque::new();
                        vm_history.push_front(Vm::start_main(self.bytecode)?);
                        self.mode = AppMode::StepRun {
                            vm_history,
                            btrace_level: 0,
                            selected_history: 0,
                        };
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('u')) => {
                    if let AppMode::StepRun {
                        ref mut vm_history,
                        ref mut btrace_level,
                        selected_history,
                    } = self.mode
                    {
                        let Some(vm) = vm_history.get(selected_history) else {
                            return Err("Missing Vm".into());
                        };
                        *btrace_level =
                            (*btrace_level + 1).min(vm.call_stack().len().saturating_sub(1));
                        self.widgets.update(vm, *btrace_level)?;
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('d')) => {
                    if let AppMode::StepRun {
                        ref mut vm_history,
                        ref mut btrace_level,
                        selected_history,
                    } = self.mode
                    {
                        let Some(vm) = vm_history.get(selected_history) else {
                            return Err("Missing Vm".into());
                        };
                        *btrace_level = btrace_level.saturating_sub(1);
                        self.widgets.update(vm, *btrace_level)?;
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('p')) => {
                    if let AppMode::StepRun {
                        ref vm_history,
                        btrace_level,
                        ref mut selected_history,
                    } = self.mode
                    {
                        *selected_history =
                            selected_history.saturating_add(1).min(vm_history.len() - 1);
                        if let Some(vm) = vm_history.get(*selected_history) {
                            self.widgets.update(vm, btrace_level)?;
                        }
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('n')) => {
                    if let AppMode::StepRun {
                        ref vm_history,
                        btrace_level,
                        ref mut selected_history,
                    } = self.mode
                    {
                        *selected_history = selected_history.saturating_sub(1);
                        if let Some(vm) = vm_history.get(*selected_history) {
                            self.widgets.update(vm, btrace_level)?;
                        }
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
                    if let Some(ref mut da) = self.widgets.disasm {
                        da.scroll = da.scroll.saturating_sub(1)
                    }
                }
                (KeyEventKind::Press, KeyCode::Down) => {
                    if let Some(ref mut da) = self.widgets.disasm {
                        da.scroll += 1
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn render_inner_text(&self) -> Result<Text, Box<dyn std::error::Error>> {
        let text = match &self.mode {
            AppMode::None => Text::from(vec![
                Line::from("Press Q to exit"),
                Line::from("Press H for help"),
            ]),
            AppMode::StepRun {
                vm_history,
                selected_history,
                ..
            } => {
                let Some(vm) = vm_history.get(*selected_history) else {
                    return Err("Missing Vm".into());
                };
                let mut lines = vec![format!(
                    "Running debugger with history {}/{}",
                    selected_history,
                    vm_history.len()
                )
                .into()];

                let mut buf = vec![];
                if let Err(e) = vm.dump_stack(&mut buf) {
                    *self.error.borrow_mut() = Some(e.to_string());
                }
                if let Ok(s) = String::from_utf8(buf) {
                    lines.push(s.into());
                }

                let mut buf = vec![];
                if let Err(e) = vm.format_current_inst(&mut buf) {
                    *self.error.borrow_mut() = Some(e.to_string());
                }
                if let Ok(s) = String::from_utf8(buf) {
                    lines.push(s.into());
                }

                if let Some(error) = self.error.borrow().as_ref() {
                    lines.push(format!("Error: {error}").bold().red().into());
                }
                Text::from(lines)
            }
        };
        Ok(text)
    }
}

impl Widgets {
    fn update(&mut self, vm: &Vm, level: usize) -> Result<(), Box<dyn std::error::Error>> {
        if let Some(ref mut disasm) = self.disasm {
            if let Some(ci) = vm.call_info(level) {
                disasm.update(ci.bytecode(), ci.instuction_ptr())?;
            }
        }
        if let Some(ref mut stack_trace) = self.stack_trace {
            stack_trace.update(vm, level)?;
        }
        if let Some(ref mut stack) = self.stack {
            stack.update(vm, level)?;
        }
        Ok(())
    }
}

impl<'a> Widget for &App<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" Interactive debugger ".bold());
        // Help shows on top of all widgets
        if let Some(ref help) = self.widgets.help {
            let mut help_area = area;
            help_area.x += help_area.width / 4;
            help_area.width /= 2;
            help_area.y += help_area.height / 4;
            help_area.height /= 2;
            help.render(help_area, buf);
        } else {
            let instructions = Title::from(Line::from(vec![
                "  help: ".into(),
                "h".blue().bold(),
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

            let inner_text = self.render_inner_text().unwrap_or_else(|e| {
                let e = e.to_string();
                *self.error.borrow_mut() = Some(e.clone());
                Text::from(format!("Error: {e}"))
            });

            Paragraph::new(inner_text).block(block).render(area, buf);

            let mut tr_area = area;
            if 0 < tr_area.height {
                tr_area.x = area.width / 2;
                tr_area.width /= 2;
                tr_area.y += 1;
                tr_area.height = (tr_area.height - 1) / 2;
                self.widgets.stack.as_ref().map(|d| d.render(tr_area, buf));
            }

            let mut widget_area = area;
            if 0 < widget_area.height {
                widget_area.y = widget_area.height / 2;
                widget_area.height = (widget_area.height - 1) / 2;

                let widget_count = self.widgets.disasm.is_some() as u16
                    + self.widgets.stack_trace.is_some() as u16;

                if widget_count != 0 {
                    widget_area.width /= widget_count;
                }

                if let Some(d) = self.widgets.disasm.as_ref() {
                    d.render(widget_area, buf);
                    widget_area.x += widget_area.width;
                }
                self.widgets
                    .stack_trace
                    .as_ref()
                    .map(|d| d.render(widget_area, buf));
            }
        }
    }
}

#[derive(Default)]
enum AppMode<'a> {
    #[default]
    None,
    StepRun {
        vm_history: VecDeque<Vm<'a>>,
        /// The level of backtrace, 0 means the latest.
        btrace_level: usize,
        /// Time-travel debugger offset, 0 means the latest.
        selected_history: usize,
    },
}
