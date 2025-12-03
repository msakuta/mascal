use ratatui::{
    buffer::Buffer,
    crossterm::event::{KeyCode, KeyEvent, KeyEventKind},
    layout::{Alignment, Rect},
    style::{Color, Style, Stylize},
    symbols::{border, scrollbar},
    text::Line,
    widgets::{
        block::Title, Block, Clear, Paragraph, Scrollbar, ScrollbarOrientation, ScrollbarState,
        StatefulWidget, Widget,
    },
};

pub(super) struct ViewSettingsWidget {
    scroll: (usize, usize),
    scroll_state: (ScrollbarState, ScrollbarState),
    cursor: usize,
    render_height: u16,
}

#[derive(Clone, Copy)]
pub(super) struct ViewSettings {
    pub view_settings: bool,
    pub source_list: bool,
    pub disassembly: bool,
    pub stack_trace: bool,
    pub locals: bool,
    pub output: bool,
    /// Filter only named locals in the Stack widget.
    pub show_named_locals: bool,
}

impl ViewSettings {
    pub fn new(show_named_locals: bool) -> Self {
        Self {
            view_settings: false,
            source_list: true,
            disassembly: true,
            stack_trace: true,
            locals: true,
            output: true,
            show_named_locals,
        }
    }
}

impl ViewSettingsWidget {
    pub(super) fn new() -> Self {
        let text = Self::text_lines(&ViewSettings::new(true));
        Self {
            scroll: (0, 0),
            scroll_state: (
                ScrollbarState::new(text.len()),
                ScrollbarState::new(
                    text.iter()
                        .map(|line| line.iter().fold(0, |acc, cur| acc + cur.content.len()))
                        .max()
                        .unwrap_or(0),
                ),
            ),
            cursor: 0,
            render_height: 10,
        }
    }

    /// Returns true if the event is consumed
    pub(super) fn handle_events(
        &mut self,
        key: &KeyEvent,
        settings: &mut ViewSettings,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        match (key.kind, key.code) {
            (KeyEventKind::Press, KeyCode::Char(' ')) => {
                match self.cursor {
                    0 => {
                        settings.source_list = !settings.source_list;
                    }
                    1 => {
                        settings.disassembly = !settings.disassembly;
                    }
                    2 => {
                        settings.stack_trace = !settings.stack_trace;
                    }
                    3 => {
                        settings.locals = !settings.locals;
                    }
                    4 => {
                        settings.output = !settings.output;
                    }
                    5 => {
                        settings.show_named_locals = !settings.show_named_locals;
                    }
                    _ => {}
                }
                return Ok(true);
            }
            (KeyEventKind::Press, KeyCode::Up) => {
                self.update_scroll_y(-1);
                return Ok(true);
            }
            (KeyEventKind::Press, KeyCode::Down) => {
                self.update_scroll_y(1);
                return Ok(true);
            }
            (KeyEventKind::Press, KeyCode::Left) => {
                self.update_scroll_x(-1);
                return Ok(true);
            }
            (KeyEventKind::Press, KeyCode::Right) => {
                self.update_scroll_x(1);
                return Ok(true);
            }
            (KeyEventKind::Press, KeyCode::Char('q') | KeyCode::Esc) => {
                settings.view_settings = false;
                return Ok(true);
            }
            _ => {}
        }
        Ok(false)
    }

    pub(super) fn update_scroll_y(&mut self, delta: i32) {
        self.cursor = if delta < 0 {
            self.cursor.saturating_sub(1)
        } else {
            self.cursor.saturating_add(1)
        }
        .min(5);
        self.scroll.0 = self.scroll.0.clamp(
            (self.cursor + 3).saturating_sub(self.render_height as usize),
            self.cursor.saturating_sub(3),
        ); // Leave 3 lines before
    }

    pub(super) fn update_scroll_x(&mut self, delta: i32) {
        Self::update_scroll(delta, &mut self.scroll.1, &mut self.scroll_state.1);
    }

    fn update_scroll(delta: i32, scroll: &mut usize, state: &mut ScrollbarState) {
        if delta < 0 {
            *scroll = scroll.saturating_sub(delta.unsigned_abs() as usize);
        } else {
            *scroll = scroll.saturating_add(delta as usize);
        }
        *state = state.position(*scroll);
    }

    fn text_lines<'a>(view_settings: &ViewSettings) -> Vec<Line<'a>> {
        fn bool_to_cross(b: bool) -> &'static str {
            if b {
                "x"
            } else {
                " "
            }
        }

        let text = vec![
            Line::from(vec![format!(
                " [{}] Show Source List widget",
                bool_to_cross(view_settings.source_list)
            )
            .into()]),
            Line::from(vec![format!(
                " [{}] Show Disassembly widget",
                bool_to_cross(view_settings.disassembly)
            )
            .into()]),
            Line::from(vec![format!(
                " [{}] Show Stack trace widget",
                bool_to_cross(view_settings.stack_trace)
            )
            .into()]),
            Line::from(vec![format!(
                " [{}] Show Local variables widget",
                bool_to_cross(view_settings.locals)
            )
            .into()]),
            Line::from(vec![format!(
                " [{}] Show Output widget",
                bool_to_cross(view_settings.output)
            )
            .into()]),
            Line::from(vec![format!(
                " [{}] Show only named locals in stack widget",
                bool_to_cross(view_settings.show_named_locals)
            )
            .into()]),
        ];

        text
    }

    pub(super) fn render(&mut self, area: Rect, buf: &mut Buffer, view_settings: &ViewSettings) {
        let mut text_lines = ViewSettingsWidget::text_lines(view_settings);

        let title = Title::from(" View Settings ".bold());
        let block = Block::bordered()
            .title(title.alignment(Alignment::Center))
            // .bg(Color::Blue)
            .style(Style::default().bg(Color::Black))
            .border_style(Style::new().white())
            .border_set(border::THICK);
        let inner = block.inner(area);

        Clear.render(area, buf);

        if let Some(line) = text_lines.get_mut(self.cursor) {
            *line = line
                .iter()
                .map(|span| span.clone().bg(Color::DarkGray))
                .collect();

            // For some reason, a loop like below won't compile by "cannot move out of `text_lines`" error.
            // I don't like pushing functional programming style too much like above, but there is no other option.
            // I will leave dirty comments here because I'm curious why the below doesn't compile and want to know
            // if it's possible to avoid errors.
            //
            // for span in line {
            //     *span = span.clone().bg(Color::DarkGray);
            // }
        }

        Paragraph::new(text_lines)
            .block(block)
            .scroll((self.scroll.0 as u16, self.scroll.1 as u16))
            .render(area, buf);

        let vert_sbar = Scrollbar::new(ScrollbarOrientation::VerticalRight)
            .symbols(scrollbar::VERTICAL)
            .begin_symbol(None)
            .track_symbol(None)
            .end_symbol(None);

        vert_sbar.render(inner, buf, &mut self.scroll_state.0);

        self.render_height = inner.height;
    }
}
