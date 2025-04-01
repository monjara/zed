use editor::{Addon, Editor, EditorEvent};
use gpui::{
    Action, AppContext, Context, Entity, KeystrokeEvent, WeakEntity, Window, actions, impl_actions,
};
use schemars::JsonSchema;
use serde::Deserialize;

use crate::{Vim, state::Mode};

#[derive(Eq, PartialEq, Copy, Clone, Deserialize, Debug, Default, JsonSchema)]
#[serde(rename_all = "camelCase")]
enum Direction {
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

#[derive(Clone, Copy, Debug)]
enum WordType {
    Word,
}

pub(crate) fn register(editor: &mut Editor, cx: &mut Context<EasyMotion>) {
    EasyMotion::action(editor, cx, EasyMotion::word);
    EasyMotion::action(editor, cx, EasyMotion::cancel);
}

pub(crate) struct EasyMotion {
    editor: WeakEntity<Editor>,
    vim: WeakEntity<Vim>,
}

impl EasyMotion {
    pub(crate) fn new(
        _window: &mut Window,
        cx: &mut Context<Editor>,
        vim: WeakEntity<Vim>,
    ) -> Entity<Self> {
        let editor = cx.entity().clone();
        cx.new(|cx| {
            cx.subscribe(&editor, Self::update_overlays).detach();
            cx.observe_keystrokes(Self::observe_keystrokes).detach();
            Self {
                editor: editor.downgrade(),
                vim,
            }
        })
    }

    fn update_overlays(&mut self, _: Entity<Editor>, _: &EditorEvent, _cx: &mut Context<Self>) {}

    fn observe_keystrokes(
        &mut self,
        event: &KeystrokeEvent,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
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
        self.word_impl(WordType::Word, direction, window, cx);
    }

    fn word_impl(
        &mut self,
        word_type: WordType,
        direction: Direction,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let Some((vim, editor)) = self.vim.upgrade().zip(self.editor.upgrade()) else {
            return;
        };
        let mode = vim.update(cx, |vim, cx| {
            let mode = vim.mode;
            assert_ne!(mode, Mode::EasyMotion);
            vim.switch_mode(Mode::EasyMotion, false, window, cx);
            mode
        });
        println!("mode: {mode:?}");
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
