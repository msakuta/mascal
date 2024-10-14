//! Implementation of the interactive debugger.

use ratatui::{
    crossterm::event::{self, KeyCode, KeyEventKind},
    style::Stylize,
    widgets::Paragraph,
    DefaultTerminal,
};

use ::mascal::Bytecode;

pub(crate) fn run_debugger(bytecode: &Bytecode) -> Result<(), Box<dyn std::error::Error>> {
    let mut terminal = ratatui::init();
    terminal.clear()?;
    let app_result = main_loop(terminal, &bytecode);
    ratatui::restore();
    // Error out after restore
    app_result
}

fn main_loop(
    mut terminal: DefaultTerminal,
    bytecode: &Bytecode,
) -> Result<(), Box<dyn std::error::Error>> {
    let message = "\
    d: disassemble current code
    r: run current code
    q: quit";
    let mut buffer: Option<String> = None;
    loop {
        terminal.draw(|frame| {
            let greeting = Paragraph::new(buffer.as_ref().map_or(message, |b| b.as_str()))
                .white()
                .on_blue();
            frame.render_widget(greeting, frame.area());
        })?;

        if let event::Event::Key(key) = event::read()? {
            if key.kind == KeyEventKind::Press && key.code == KeyCode::Char('d') {
                let mut temp = vec![];
                bytecode.disasm(&mut temp)?;
                buffer = Some(format!(
                    "Showing disassembly (q to quit):\n{}",
                    String::from_utf8(temp)?
                ));
            }
            if key.kind == KeyEventKind::Press && matches!(key.code, KeyCode::Char('q' | 'Q')) {
                if buffer.is_some() {
                    buffer = None;
                } else {
                    return Ok(());
                }
            }
        }
    }
}
