//! Implementation of the interactive debugger.

mod disasm;
mod help;
mod locals;
mod output;
mod source_list;
mod stack_trace;
mod view_settings;

use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use ratatui::{
    buffer::Buffer,
    crossterm::event::{self, KeyCode, KeyEventKind, KeyModifiers},
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Style, Stylize},
    symbols::border,
    text::{Line, Text},
    widgets::{
        block::{Position, Title},
        Block, Paragraph, Widget,
    },
    DefaultTerminal, Frame,
};

use ::mascal::{interpret, Bytecode, DebugInfo, Vm};
use view_settings::{ViewSettings, ViewSettingsWidget};

use self::{
    disasm::DisasmWidget, help::HelpWidget, locals::LocalsWidget, output::OutputWidget,
    source_list::SourceListWidget, stack_trace::StackTraceWidget,
};

pub(crate) fn run_debugger(mut bytecode: Bytecode) -> Result<(), Box<dyn std::error::Error>> {
    let output_buffer = Rc::new(RefCell::new(vec![]));
    bytecode.add_std_fn(output_buffer.clone());
    let mut terminal = ratatui::init();
    terminal.clear()?;
    let app_result = App::new(&bytecode, output_buffer).run(terminal);
    ratatui::restore();
    // Error out after restore
    app_result
}

struct App<'a> {
    bytecode: &'a Bytecode,
    mode: AppMode<'a>,
    output_buffer: Rc<RefCell<Vec<u8>>>,
    /// Error string that is displayed on top of the debugger widget.
    /// Wrapped in a RefCell because rendering methods can have errors.
    error: RefCell<Option<String>>,
    view_settings: ViewSettings,
    widgets: Widgets,
    exit: bool,
}

#[derive(Clone, Copy)]
enum WidgetFocus {
    SourceList,
    Locals,
    Disasm,
    Output,
}

struct Widgets {
    focus: WidgetFocus,
    source_list: SourceListWidget,
    disasm: DisasmWidget,
    stack_trace: StackTraceWidget,
    locals: LocalsWidget,
    output: OutputWidget,
    view_settings: ViewSettingsWidget,
    help: Option<HelpWidget>,
}

impl<'a> App<'a> {
    fn new(bytecode: &'a Bytecode, output_buffer: Rc<RefCell<Vec<u8>>>) -> Self {
        let (source_list, error) = SourceListWidget::new(bytecode);
        Self {
            bytecode,
            mode: Default::default(),
            output_buffer,
            error: RefCell::new(error.map(|e| e.to_string())),
            view_settings: ViewSettings::new(bytecode.debug_info().is_some()),
            widgets: Widgets {
                focus: WidgetFocus::SourceList,
                source_list,
                disasm: DisasmWidget::new(bytecode).unwrap(),
                stack_trace: StackTraceWidget::new(),
                locals: LocalsWidget::new(),
                output: OutputWidget::new(),
                view_settings: ViewSettingsWidget::new(),
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

    fn draw(&mut self, frame: &mut Frame) {
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
            if self.view_settings.view_settings {
                if self
                    .widgets
                    .view_settings
                    .handle_events(&key, &mut self.view_settings)?
                {
                    // When view settings are changed, some widgets may need to update.
                    if let AppMode::StepRun {
                        ref vm_history,
                        btrace_level,
                        selected_history,
                        ..
                    } = self.mode
                    {
                        if let Some(vm) = vm_history.get(selected_history) {
                            let debug_fn = self
                                .bytecode
                                .debug_info()
                                .zip(vm.call_info(btrace_level))
                                .and_then(|(debug, ci)| debug.get(ci.bytecode().name()));
                            self.widgets.locals.update(
                                vm,
                                btrace_level,
                                debug_fn,
                                &self.view_settings,
                            )?;
                        }
                    }
                    return Ok(());
                }
            }

            match (key.kind, key.code) {
                (KeyEventKind::Press, KeyCode::Char('l')) => {
                    self.view_settings.source_list = !self.view_settings.source_list;
                }
                (KeyEventKind::Press, KeyCode::Char('D')) => {
                    self.view_settings.disassembly = !self.view_settings.disassembly;
                }
                (KeyEventKind::Press, KeyCode::Char('t')) => {
                    self.view_settings.stack_trace = !self.view_settings.stack_trace;
                }
                (KeyEventKind::Press, KeyCode::Char('k')) => {
                    self.view_settings.locals = !self.view_settings.locals;
                }
                (KeyEventKind::Press, KeyCode::Char('o')) => {
                    self.view_settings.output = !self.view_settings.output;
                }
                (KeyEventKind::Press, KeyCode::Char('v')) => {
                    if !self.view_settings.view_settings {
                        self.view_settings.view_settings = true;
                        self.widgets.help = None;
                    } else {
                        self.view_settings.view_settings = false;
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('h')) => {
                    if self.widgets.help.is_some() {
                        self.widgets.help = None;
                    } else {
                        self.widgets.help = Some(HelpWidget::new());
                        self.view_settings.view_settings = false;
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('r')) => {
                    interpret(self.bytecode)?;
                }
                (KeyEventKind::Press, KeyCode::Char('b')) => {
                    self.widgets.source_list.toggle_breakpoint();
                }
                (KeyEventKind::Press, KeyCode::Char('c')) => {
                    *self.error.borrow_mut() = None;
                    self.run_continue()?;
                }
                (KeyEventKind::Press, KeyCode::Char('s')) => {
                    if let AppMode::StepRun {
                        ref mut last_continue,
                        ..
                    } = self.mode
                    {
                        *last_continue = None;
                    }
                    *self.error.borrow_mut() = None;
                    self.run_step()?;
                }
                (KeyEventKind::Press, KeyCode::Char('u')) => {
                    if let AppMode::StepRun {
                        ref mut vm_history,
                        ref mut btrace_level,
                        selected_history,
                        ..
                    } = self.mode
                    {
                        let Some(vm) = vm_history.get(selected_history) else {
                            return Err("Missing Vm".into());
                        };
                        *btrace_level =
                            (*btrace_level + 1).min(vm.call_stack().len().saturating_sub(1));
                        self.widgets.update(
                            vm,
                            *btrace_level,
                            self.bytecode.debug_info(),
                            &self.output_buffer,
                            &self.view_settings,
                        )?;
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('d')) => {
                    if let AppMode::StepRun {
                        ref mut vm_history,
                        ref mut btrace_level,
                        selected_history,
                        ..
                    } = self.mode
                    {
                        let Some(vm) = vm_history.get(selected_history) else {
                            return Err("Missing Vm".into());
                        };
                        *btrace_level = btrace_level.saturating_sub(1);
                        self.widgets.update(
                            vm,
                            *btrace_level,
                            self.bytecode.debug_info(),
                            &self.output_buffer,
                            &self.view_settings,
                        )?;
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('p')) => {
                    if let AppMode::StepRun {
                        ref vm_history,
                        btrace_level,
                        ref mut selected_history,
                        ..
                    } = self.mode
                    {
                        *selected_history =
                            selected_history.saturating_add(1).min(vm_history.len() - 1);
                        if let Some(vm) = vm_history.get(*selected_history) {
                            self.widgets.update(
                                vm,
                                btrace_level,
                                self.bytecode.debug_info(),
                                &self.output_buffer,
                                &self.view_settings,
                            )?;
                        }
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('n')) => {
                    if let AppMode::StepRun {
                        ref vm_history,
                        btrace_level,
                        ref mut selected_history,
                        ..
                    } = self.mode
                    {
                        *selected_history = selected_history.saturating_sub(1);
                        if let Some(vm) = vm_history.get(*selected_history) {
                            self.widgets.update(
                                vm,
                                btrace_level,
                                self.bytecode.debug_info(),
                                &self.output_buffer,
                                &self.view_settings,
                            )?;
                        }
                    }
                }
                (KeyEventKind::Press, KeyCode::Char('q' | 'Q') | KeyCode::Esc) => {
                    if self.widgets.help.is_some() {
                        self.widgets.help = None;
                        return Ok(());
                    }
                    if !matches!(self.mode, AppMode::None) {
                        self.mode = AppMode::None;
                    } else {
                        self.exit = true;
                    }
                }
                (KeyEventKind::Press, KeyCode::Tab) => {
                    if self.widgets.help.is_none() {
                        // TODO: how to pick up Shift+Tab event?
                        self.widgets.focus = if key.modifiers.contains(KeyModifiers::SHIFT) {
                            match self.widgets.focus {
                                WidgetFocus::SourceList => WidgetFocus::Disasm,
                                WidgetFocus::Disasm => WidgetFocus::Locals,
                                WidgetFocus::Locals => WidgetFocus::Output,
                                WidgetFocus::Output => WidgetFocus::SourceList,
                            }
                        } else {
                            match self.widgets.focus {
                                WidgetFocus::SourceList => WidgetFocus::Output,
                                WidgetFocus::Disasm => WidgetFocus::SourceList,
                                WidgetFocus::Locals => WidgetFocus::Disasm,
                                WidgetFocus::Output => WidgetFocus::Locals,
                            }
                        };
                        let focus = self.widgets.focus;
                        self.widgets.source_list.focus = matches!(focus, WidgetFocus::SourceList);
                        self.widgets.disasm.focus = matches!(focus, WidgetFocus::Disasm);
                        self.widgets.locals.focus = matches!(focus, WidgetFocus::Locals);
                        self.widgets.output.focus = matches!(focus, WidgetFocus::Output);
                    }
                }
                (KeyEventKind::Press, KeyCode::Up) => self.update_scroll(-1),
                (KeyEventKind::Press, KeyCode::Down) => self.update_scroll(1),
                (KeyEventKind::Press, KeyCode::Left) => self.update_scroll_x(-1),
                (KeyEventKind::Press, KeyCode::Right) => self.update_scroll_x(1),
                _ => {}
            }
        }
        Ok(())
    }

    fn update_scroll(&mut self, delta: i32) {
        if let Some(ref mut help) = self.widgets.help {
            help.update_scroll_y(delta)
        } else {
            match self.widgets.focus {
                WidgetFocus::SourceList => self.widgets.source_list.update_scroll(delta),
                WidgetFocus::Disasm => {
                    self.widgets.disasm.update_scroll(delta);
                }
                WidgetFocus::Locals => {
                    self.widgets.locals.update_scroll(delta);
                }
                WidgetFocus::Output => {
                    self.widgets.output.update_scroll(delta);
                }
            }
        }
    }

    fn update_scroll_x(&mut self, delta: i32) {
        if let Some(ref mut help) = self.widgets.help {
            help.update_scroll_x(delta)
        }
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
                last_continue,
                ..
            } => {
                let mut message = format!(
                    "Running debugger with history {}/{}",
                    selected_history,
                    vm_history.len()
                );
                if let Some(last_continue) = last_continue {
                    message += &format!(", ran {last_continue} instructions");
                }
                let mut lines = vec![message.into()];

                if let Some(error) = self.error.borrow().as_ref() {
                    lines.push(format!("Error: {error}").bold().red().into());
                }
                Text::from(lines)
            }
        };
        Ok(text)
    }

    fn run_continue(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let debug_info = self
            .bytecode
            .debug_info()
            .ok_or_else(|| "Cannot use a breakpoint without debug info")?;

        let mut start_line_info;
        if let AppMode::StepRun { ref vm_history, .. } = self.mode {
            start_line_info = vm_history
                .front()
                .and_then(|vm| vm.call_info(0))
                .and_then(|ci| {
                    let fn_debug = debug_info.get(ci.bytecode().name())?;
                    let ip32 = ci.instruction_ptr() as u32;
                    let line_info = fn_debug
                        .line_info
                        .binary_search_by_key(&ip32, |li| li.instruction)
                        .map_or_else(|res| res, |res| res);
                    fn_debug.line_info.get(line_info).map(|li| li.src_line)
                });
        } else {
            start_line_info = None;
        }

        let mut loop_count = 0;

        loop {
            self.run_step()?;
            let AppMode::StepRun { ref vm_history, .. } = self.mode else {
                break;
            };
            let Some(vm) = vm_history.front() else { break };
            let Some(ci) = vm.call_info(0) else { break };
            let fn_debug = debug_info.get(ci.bytecode().name()).ok_or_else(|| {
                format!(
                    "Cannot find debug info for a function {}",
                    ci.bytecode().name()
                )
            })?;
            if self.widgets.source_list.check_breakpoint(
                ci.instruction_ptr(),
                Some(&fn_debug.line_info[..]),
                start_line_info,
            ) {
                break;
            }

            let ip32 = ci.instruction_ptr() as u32;

            let li_index = fn_debug
                .line_info
                .binary_search_by_key(&ip32, |li| li.instruction)
                .map_or_else(|res| res, |res| res);

            if let Some(line_info) = fn_debug.line_info.get(li_index) {
                if Some(line_info.src_line) != start_line_info {
                    start_line_info = None;
                }
            }
            loop_count += 1;
        }

        if let AppMode::StepRun {
            ref mut last_continue,
            ..
        } = self.mode
        {
            *last_continue = Some(loop_count);
        }

        Ok(())
    }

    fn run_step(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        if let AppMode::StepRun {
            ref mut vm_history,
            btrace_level,
            ref mut selected_history,
            ..
        } = self.mode
        {
            // Here is a very esoteric maneuver that needs explanation.
            // We can't use a newly created Vm from deepclone for the next vm, because its
            // internal Rc will break links to the shared ArrayInt buffer.
            // However, the original object (which `deepclone()` is called against) does not lose
            // internal links, so we can keep calling `next_inst()` to make progress.
            // In order to keep the original always on front and old Vm snapshots pushed back,
            // we need to take the most recent one, make a deepclone of it, and push it back,
            // and finally push the original Vm back to front.
            let Some(mut next_vm) = vm_history.pop_front() else {
                return Err("Missing Vm".into());
            };
            let prev_vm = next_vm.deepclone();
            let res = next_vm.next_inst();
            self.widgets.update(
                &mut next_vm,
                btrace_level,
                self.bytecode.debug_info(),
                &self.output_buffer,
                &self.view_settings,
            )?;
            vm_history.push_front(prev_vm);
            // We break here in case of an error to keep the state just before the error.
            res?;
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
                last_continue: None,
            };
            // Clear the output buffer since it could contain output from previous run
            self.output_buffer.borrow_mut().clear();
        }
        Ok(())
    }
}

impl Widgets {
    fn update(
        &mut self,
        vm: &Vm,
        level: usize,
        debug: Option<&DebugInfo>,
        output_buffer: &Rc<RefCell<Vec<u8>>>,
        vs: &ViewSettings,
    ) -> Result<(), Box<dyn std::error::Error>> {
        if let Some(ci) = vm.call_info(level) {
            let debug_fn = debug.and_then(|debug| debug.get(ci.bytecode().name()));
            let ip = ci.instruction_ptr();
            self.source_list
                .update(ip, debug_fn.map(|v| &v.line_info[..]))?;
            self.disasm
                .update(ci.bytecode(), ip, debug_fn.map(|v| &v.line_info[..]))?;
            self.locals.update(vm, level, debug_fn, vs)?;
        }
        self.stack_trace.update(vm, level)?;
        self.output.update(&output_buffer.borrow())?;
        Ok(())
    }
}

impl<'a> Widget for &mut App<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let title = Title::from(" Interactive debugger ".bold());
        let instructions = Title::from(Line::from(vec![
            "  help: ".into(),
            "h".blue().bold(),
            if matches!(self.mode, AppMode::StepRun { .. }) {
                "  stop running program: ".into()
            } else {
                "  quit: ".into()
            },
            "q ".blue().bold(),
        ]));
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            .title(
                instructions
                    .alignment(Alignment::Center)
                    .position(Position::Bottom),
            )
            .border_style(if matches!(self.mode, AppMode::StepRun { .. }) {
                Style::new().light_yellow()
            } else {
                Style::new()
            })
            .border_set(border::THICK);

        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(block.inner(area));

        let inner_text = self.render_inner_text().unwrap_or_else(|e| {
            let e = e.to_string();
            *self.error.borrow_mut() = Some(e.clone());
            Text::from(format!("Error: {e}"))
        });

        Paragraph::new(inner_text).block(block).render(area, buf);

        let top_area = layout[0];

        let top_widgets = self.view_settings.output as u16 + self.view_settings.source_list as u16;
        let top_constraints: Vec<_> = (0..top_widgets)
            .map(|_| Constraint::Percentage(100 / top_widgets))
            .collect();

        let top_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(top_constraints)
            .split(top_area);

        let mut horz_i = 0;
        if self.view_settings.output {
            let mut output_area = top_layout[horz_i];
            if 4 < output_area.height {
                output_area.y += 2;
                output_area.height = output_area.height.saturating_sub(2);
                self.widgets.output.render(output_area, buf);
            }
            horz_i += 1;
        }

        if self.view_settings.source_list {
            let source_area = top_layout[horz_i];
            self.widgets.source_list.render(source_area, buf);
            // horz_i += 1;
        }

        let bottom_area = layout[1];
        if 0 < bottom_area.height {
            let widget_count = self.view_settings.disassembly as u16
                + self.view_settings.stack_trace as u16
                + self.view_settings.locals as u16;
            let bottom_constraints: Vec<_> = (0..widget_count)
                .map(|_| Constraint::Percentage(100 / widget_count))
                .collect();

            let bottom_layout = Layout::default()
                .direction(Direction::Horizontal)
                .constraints(bottom_constraints)
                .split(bottom_area);

            horz_i = 0;
            if self.view_settings.locals {
                let tr_area = bottom_layout[horz_i];
                if 0 < tr_area.height {
                    self.widgets.locals.render(tr_area, buf);
                }
                horz_i += 1;
            }

            if self.view_settings.disassembly {
                let disasm_area = bottom_layout[horz_i];
                self.widgets.disasm.render(disasm_area, buf);
                horz_i += 1;
            }

            if self.view_settings.stack_trace {
                self.widgets.stack_trace.render(bottom_layout[horz_i], buf);
            }
        }

        // View settings shows on the second top of all widgets
        if self.view_settings.view_settings {
            let mut help_area = area;
            help_area.x += help_area.width / 4;
            help_area.width /= 2;
            help_area.y += help_area.height / 4;
            help_area.height /= 2;
            self.widgets
                .view_settings
                .render(help_area, buf, &self.view_settings);
        }

        // Help shows on top of all widgets
        if let Some(ref mut help) = self.widgets.help {
            let mut help_area = area;
            help_area.x += help_area.width / 4;
            help_area.width /= 2;
            help_area.y += help_area.height / 4;
            help_area.height /= 2;
            help.render(help_area, buf);
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
        last_continue: Option<usize>,
    },
}
