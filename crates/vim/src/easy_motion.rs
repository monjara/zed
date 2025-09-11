use easy_motion_state::{EasyMotionState, OverlayState};
use editor::{
    Addon, DisplayPoint, Editor, EditorEvent, MultiBufferSnapshot, SelectionEffects, ToPoint,
    display_map::DisplaySnapshot, overlay::Overlay,
};
use gpui::{
    Action, AppContext, Context, Entity, HighlightStyle, Hsla, KeystrokeEvent, Render, WeakEntity,
    Window, actions,
};
use schemars::JsonSchema;
use search::{enumerate_points_by_text, sort_matches_display};
use serde::Deserialize;
use settings::Settings;
use text::{Bias, SelectionGoal};
use theme::ThemeSettings;
use trie::{Trie, TrimResult};

use crate::{Vim, VimSettings, state::Mode};

mod easy_motion_state;
mod search;
mod search_text;
mod trie;

#[derive(Eq, PartialEq, Copy, Clone, Deserialize, Debug, Default, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub(crate) enum Direction {
    #[default]
    Both,
    Forwards,
    Backwards,
}

#[derive(Clone, Deserialize, PartialEq, JsonSchema, Action)]
#[action(namespace = easy_motion)]
#[serde(deny_unknown_fields)]
struct Word(Direction);

#[derive(Clone, Deserialize, PartialEq, JsonSchema, Action)]
#[action(namespace = easy_motion)]
#[serde(deny_unknown_fields)]
struct NChar(u8);

actions!(easy_motion, [Cancel]);

pub(crate) fn register(editor: &mut Editor, cx: &mut Context<EasyMotion>) {
    EasyMotion::action(editor, cx, EasyMotion::n_char);
    EasyMotion::action(editor, cx, EasyMotion::cancel);
}

pub(crate) struct EasyMotion {
    pending_input: bool,
    record_count: u8,
    search_pattern: Option<String>,
    state: Option<EasyMotionState>,
    editor: WeakEntity<Editor>,
    vim: WeakEntity<Vim>,
}

impl EasyMotion {
    pub(crate) fn new(
        vim: WeakEntity<Vim>,
        window: &mut Window,
        cx: &mut Context<Editor>,
    ) -> Entity<Self> {
        let editor = cx.entity();
        cx.new(|cx| {
            cx.subscribe_in(&editor, window, Self::update_overlays)
                .detach();

            cx.observe_keystrokes(Self::observe_keystrokes).detach();

            Self {
                pending_input: false,
                record_count: 0,
                search_pattern: None,
                state: None,
                editor: editor.downgrade(),
                vim,
            }
        })
    }

    fn update_overlays(
        &mut self,
        editor: &Entity<Editor>,
        event: &EditorEvent,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        if !matches!(event, EditorEvent::Fold | EditorEvent::UnFold) {
            return;
        }
        let Some(state) = self.state.as_ref() else {
            return;
        };

        editor.update(cx, |this, cx| {
            let snapshot = this.snapshot(window, cx);
            this.clear_overlays::<Self>(cx);
            Self::add_overlays(
                this,
                state.trie().iter(),
                state.trie().len(),
                &snapshot.buffer_snapshot,
                &snapshot.display_snapshot,
                window,
                cx,
            );
        });
    }

    fn observe_keystrokes(
        &mut self,
        event: &KeystrokeEvent,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        if event.action.is_some() {
            return;
        }

        if window.has_pending_keystrokes() {
            return;
        }

        if self.pending_input {
            self.record_search_pattern(event);
            self.overlay_n_char(window, cx);
            return;
        }

        let Some((state, editor)) = self.state.take().zip(self.editor.upgrade()) else {
            return;
        };

        let keys = event.keystroke.key.as_str();
        let new_state = editor.update(cx, |editor, cx| {
            Self::handle_trim(state, keys, editor, window, cx)
        });
        let Some(new_state) = new_state else {
            let Some(vim) = self.vim.upgrade() else {
                return;
            };
            vim.update(cx, |vim, cx| {
                vim.switch_mode(Mode::Normal, false, window, cx)
            });
            return;
        };

        self.state = Some(new_state);
    }

    /// Register an action on the editor.
    pub(crate) fn action<A: Action>(
        editor: &mut Editor,
        cx: &mut Context<Self>,
        f: impl Fn(&mut Self, &A, &mut Window, &mut Context<Self>) + 'static,
    ) {
        let subscription = editor.register_action(cx.listener(f));
        cx.on_release(|_, _| drop(subscription)).detach();
    }

    fn n_char(&mut self, action: &NChar, window: &mut Window, cx: &mut Context<Self>) {
        let Some(vim) = self.vim.upgrade() else {
            return;
        };
        let record_count = action.0;
        self.record_count = record_count;

        vim.update(cx, |vim, cx| {
            let mode = vim.mode;
            assert_ne!(mode, Mode::EasyMotion);
            vim.switch_mode(Mode::EasyMotion, false, window, cx);
            mode
        });

        self.pending_input = true;
        self.search_pattern = Some(String::new());
    }

    fn record_search_pattern(&mut self, event: &KeystrokeEvent) {
        if let (Some(text), Some(key)) = (&self.search_pattern, &event.keystroke.key_char) {
            let new_text = format!("{}{}", text, key);
            self.search_pattern = Some(new_text);
        }
    }

    fn overlay_n_char(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        let Some(text) = self.search_pattern.as_ref() else {
            return;
        };

        if text.len() < self.record_count as usize {
            return;
        }

        let Some((vim, editor)) = self.vim.upgrade().zip(self.editor.upgrade()) else {
            return;
        };

        let new_state = editor.update(cx, |editor, cx| {
            let direction = Direction::Both;
            let points = enumerate_points_by_text(text, direction, editor, window, cx);
            Self::handle_new_matches(points, direction, editor, window, cx)
        });

        let Some(new_state) = new_state else {
            vim.update(cx, move |vim, cx| {
                vim.switch_mode(Mode::Normal, false, window, cx);
            });
            return;
        };

        self.state = Some(new_state);
        self.pending_input = false;
    }

    fn handle_new_matches(
        mut matches: Vec<DisplayPoint>,
        direction: Direction,
        editor: &mut Editor,
        window: &mut Window,
        cx: &mut Context<Editor>,
    ) -> Option<EasyMotionState> {
        if matches.is_empty() {
            return None;
        }

        editor.blink_manager.update(cx, |blink, cx| {
            blink.disable(cx);
            cx.notify();
        });

        let selections = editor.selections.newest_display(cx);
        sort_matches_display(&mut matches, &selections.start);

        let keys = VimSettings::get_global(cx).easy_motion.keys.clone();

        let snapshot = editor.snapshot(window, cx);
        let display_snapshot = &snapshot.display_snapshot;

        let (style_0, style_1, style_2) = Self::get_highlights(cx);
        let trie = Trie::new_from_vec(keys, matches, |depth, point| {
            let style = match depth {
                0 | 1 => style_0,
                2 => style_1,
                3.. => style_2,
            };
            OverlayState {
                style,
                offset: point.to_offset(display_snapshot, Bias::Right),
            }
        });
        Self::add_overlays(
            editor,
            trie.iter(),
            trie.len(),
            &snapshot.buffer_snapshot,
            &snapshot.display_snapshot,
            window,
            cx,
        );

        let start = match direction {
            Direction::Both | Direction::Backwards => DisplayPoint::zero(),
            Direction::Forwards => selections.start,
        };
        let end = match direction {
            Direction::Both | Direction::Forwards => display_snapshot.max_point(),
            Direction::Backwards => selections.end,
        };
        let anchor_start = display_snapshot.display_point_to_anchor(start, Bias::Left);
        let anchor_end = display_snapshot.display_point_to_anchor(end, Bias::Left);
        let highlight = HighlightStyle {
            fade_out: Some(0.7),
            ..Default::default()
        };
        editor.highlight_text::<Self>(vec![anchor_start..anchor_end], highlight, cx);

        let new_state = EasyMotionState::new(trie);
        Some(new_state)
    }

    fn handle_trim(
        selection: EasyMotionState,
        keys: &str,
        editor: &mut Editor,
        window: &mut Window,
        cx: &mut Context<Editor>,
    ) -> Option<EasyMotionState> {
        let (selection, res) = selection.record_str(keys);
        match res {
            TrimResult::Found(overlay) => {
                let snapshot = editor.snapshot(window, cx);
                let point = overlay.offset.to_point(&snapshot.buffer_snapshot);
                let point = snapshot
                    .display_snapshot
                    .point_to_display_point(point, Bias::Right);
                editor.change_selections(SelectionEffects::default(), window, cx, |selection| {
                    selection.move_cursors_with(|_, _, _| (point, SelectionGoal::None))
                });
                Self::clear_editor(editor, cx);
                None
            }
            TrimResult::Changed => {
                let trie = selection.trie();
                let len = trie.len();
                editor.clear_overlays::<Self>(cx);
                let snapshot = editor.snapshot(window, cx);
                Self::add_overlays(
                    editor,
                    trie.iter(),
                    len,
                    &snapshot.buffer_snapshot,
                    &snapshot.display_snapshot,
                    window,
                    cx,
                );
                Some(selection)
            }
            TrimResult::Err => {
                Self::clear_editor(editor, cx);
                None
            }
            TrimResult::NoChange => Some(selection),
        }
    }

    fn add_overlays<'a>(
        editor: &mut Editor,
        trie_iter: impl Iterator<Item = (String, &'a OverlayState)>,
        len: usize,
        buffer_snapshot: &MultiBufferSnapshot,
        display_snapshot: &DisplaySnapshot,
        _window: &mut Window,
        cx: &mut Context<Editor>,
    ) {
        let overlays = trie_iter.filter_map(|(seq, overlay)| {
            let mut highlights = vec![(0..1, overlay.style)];
            if seq.len() > 1 {
                highlights.push((
                    1..seq.len(),
                    HighlightStyle {
                        fade_out: Some(0.3),
                        ..overlay.style
                    },
                ));
            }
            let point = buffer_snapshot.offset_to_point(overlay.offset);
            if display_snapshot.is_point_folded(point) {
                None
            } else {
                let overlay = Overlay {
                    text: seq,
                    highlights,
                    point: display_snapshot.point_to_display_point(point, text::Bias::Left),
                };
                Some(overlay)
            }
        });
        editor.add_overlays_with_reserve::<Self>(overlays, len, cx);
        cx.notify();
    }

    fn cancel(&mut self, _: &Cancel, window: &mut Window, cx: &mut Context<Self>) {
        let Some((vim, editor)) = self.vim.upgrade().zip(self.editor.upgrade()) else {
            return;
        };

        vim.update(cx, |vim, cx| {
            let mode = vim.mode;
            assert_eq!(mode, Mode::EasyMotion);
            vim.switch_mode(Mode::Normal, false, window, cx);
        });

        self.pending_input = false;
        self.search_pattern = None;
        self.state = None;
        editor.update(cx, Self::clear_editor);
    }

    fn clear_editor(editor: &mut Editor, cx: &mut Context<Editor>) {
        editor.blink_manager.update(cx, |blink, cx| {
            blink.enable(cx);
        });
        editor.clear_overlays::<Self>(cx);
        editor.clear_highlights::<Self>(cx);
    }

    fn get_highlights(
        cx: &mut Context<Editor>,
    ) -> (HighlightStyle, HighlightStyle, HighlightStyle) {
        let theme = &ThemeSettings::get_global(cx).active_theme;
        let players = &theme.players().0;
        let bg = theme.colors().background;
        let style_0 = HighlightStyle {
            color: Some(Self::saturate(players[0].cursor, 1.0)),
            background_color: Some(bg),
            ..HighlightStyle::default()
        };
        let style_1 = HighlightStyle {
            color: Some(Self::saturate(players[2].cursor, 1.0)),
            background_color: Some(bg),
            ..HighlightStyle::default()
        };
        let style_2 = HighlightStyle {
            color: Some(Self::saturate(players[3].cursor, 1.0)),
            background_color: Some(bg),
            ..HighlightStyle::default()
        };
        (style_0, style_1, style_2)
    }

    fn saturate(mut color: Hsla, s: f32) -> Hsla {
        color.s = s;
        color
    }
}

impl Render for EasyMotion {
    fn render(&mut self, _window: &mut Window, _cx: &mut Context<Self>) -> impl gpui::IntoElement {
        gpui::Empty
    }
}

pub(crate) struct EasyMotionAddon {
    pub(crate) _view: Entity<EasyMotion>,
}

impl Addon for EasyMotionAddon {
    fn to_any(&self) -> &dyn std::any::Any {
        self
    }
}
