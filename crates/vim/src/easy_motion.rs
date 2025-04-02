use easy_motion_state::{EasyMotionState, OverlayState};
use editor::{
    Addon, DisplayPoint, Editor, EditorEvent, MultiBufferSnapshot, ToPoint,
    display_map::DisplaySnapshot, overlay::Overlay, scroll::Autoscroll,
};
use gpui::{
    Action, AppContext, Context, Entity, HighlightStyle, Hsla, KeystrokeEvent, Render, WeakEntity,
    Window, actions, impl_actions,
};
use schemars::JsonSchema;
use search::{enumerate_word_beggings, sort_matches_display};
use serde::Deserialize;
use settings::Settings;
use text::{Bias, SelectionGoal};
use theme::ThemeSettings;
use trie::{Trie, TrimResult};

use crate::{Vim, VimSettings, state::Mode};

mod easy_motion_state;
mod search;
mod trie;

#[derive(Eq, PartialEq, Copy, Clone, Deserialize, Debug, Default, JsonSchema)]
#[serde(rename_all = "camelCase")]
pub(crate) enum Direction {
    #[default]
    Both,
    Forwards,
    Backwards,
}

#[derive(Clone, Deserialize, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
struct Word(Direction);

impl_actions!(easy_motion, [Word]);

actions!(easy_motion, [Cancel]);

pub(crate) fn register(editor: &mut Editor, cx: &mut Context<EasyMotion>) {
    EasyMotion::action(editor, cx, EasyMotion::word);
    EasyMotion::action(editor, cx, EasyMotion::cancel);
}

pub(crate) struct EasyMotion {
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
        let editor = cx.entity().clone();
        cx.new(|cx| {
            // cx.subscribe(&editor, move |this, editor, event, cx| {
            //     Self::update_overlays(this, editor, event, *window, cx);
            // })
            // .detach();

            cx.subscribe_in(&editor, window, Self::update_overlays)
                .detach();

            cx.observe_keystrokes(Self::observe_keystrokes).detach();

            Self {
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
        } else if window.has_pending_keystrokes() {
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

    fn word(&mut self, action: &Word, window: &mut Window, cx: &mut Context<Self>) {
        let direction = action.0;
        self.word_impl(direction, window, cx);
    }

    fn word_impl(&mut self, direction: Direction, window: &mut Window, cx: &mut Context<Self>) {
        let Some((vim, editor)) = self.vim.upgrade().zip(self.editor.upgrade()) else {
            return;
        };
        let mode = vim.update(cx, |vim, cx| {
            let mode = vim.mode;
            assert_ne!(mode, Mode::EasyMotion);
            vim.switch_mode(Mode::EasyMotion, false, window, cx);
            mode
        });

        let new_state = editor.update(cx, |editor, cx| {
            let points = enumerate_word_beggings(direction, editor, window, cx);
            Self::handle_new_matches(points, direction, editor, window, cx)
        });

        let Some(new_state) = new_state else {
            vim.update(cx, move |vim, cx| {
                vim.switch_mode(mode, false, window, cx);
            });
            return;
        };

        self.state = Some(new_state);
    }

    fn handle_new_matches(
        mut matches: Vec<DisplayPoint>,
        direction: Direction,
        editor: &mut Editor,
        window: &mut Window,
        cx: &mut Context<Editor>,
    ) -> Option<EasyMotionState> {
        editor.blink_manager.update(cx, |blink, cx| {
            blink.disable(cx);
            cx.notify();
            // blink.hide_cursor(cx);
        });

        let selections = editor.selections.newest_display(cx);
        let snapshot = editor.snapshot(window, cx);
        let map = &snapshot.display_snapshot;

        if matches.is_empty() {
            return None;
        }
        sort_matches_display(&mut matches, &selections.start);

        let keys = VimSettings::get_global(cx).easy_motion.keys.clone();

        let (style_0, style_1, style_2) = Self::get_highlights(cx);
        let trie = Trie::new_from_vec(keys, matches, |depth, point| {
            let style = match depth {
                0 | 1 => style_0,
                2 => style_1,
                3.. => style_2,
            };
            OverlayState {
                style,
                offset: point.to_offset(map, Bias::Right),
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
            Direction::Both | Direction::Forwards => map.max_point(),
            Direction::Backwards => selections.end,
        };
        let anchor_start = map.display_point_to_anchor(start, Bias::Left);
        let anchor_end = map.display_point_to_anchor(end, Bias::Left);
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
                editor.change_selections(Some(Autoscroll::fit()), window, cx, |selection| {
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
