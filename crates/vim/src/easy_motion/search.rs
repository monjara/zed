use std::{cmp::Ordering, ops::Range};

use editor::{
    DisplayPoint, Editor, RowExt,
    display_map::{DisplayRow, DisplaySnapshot},
    movement::TextLayoutDetails,
};
use gpui::{Context, Window};
use text::{Bias, Selection};

use crate::{
    easy_motion::search_text::bm_search,
    motion::{window_bottom, window_top},
};

use super::Direction;

pub(crate) fn enumerate_points_by_text(
    pattern: &str,
    direction: Direction,
    editor: &mut Editor,
    window: &mut Window,
    cx: &mut Context<Editor>,
) -> Vec<DisplayPoint> {
    let selections = editor.selections.newest_display(cx);
    let snapshot = editor.snapshot(window, cx);
    let display_snapshot = snapshot.display_snapshot;

    let mut text_layout_details = editor.text_layout_details(window);
    text_layout_details.vertical_scroll_margin = 0.0;
    let Range { start, end } = ranges(
        direction,
        &display_snapshot,
        &selections,
        &text_layout_details,
    );

    enumerate_points_by_text_in_range(pattern, &display_snapshot, start, end)
}

fn enumerate_points_by_text_in_range(
    pattern: &str,
    display_snapshot: &DisplaySnapshot,
    from: DisplayPoint,
    to: DisplayPoint,
) -> Vec<DisplayPoint> {
    let start = display_snapshot
        .display_point_to_fold_point(from, Bias::Left)
        .to_offset(&display_snapshot.fold_snapshot);

    let end = display_snapshot
        .display_point_to_fold_point(to, Bias::Right)
        .to_offset(&display_snapshot.fold_snapshot);

    let text = display_snapshot
        .fold_snapshot
        .chars_for_range(start, end)
        .collect::<String>();

    bm_search(&text, pattern, from.row())
}

fn ranges(
    direction: Direction,
    map: &DisplaySnapshot,
    selection: &Selection<DisplayPoint>,
    text_layout_details: &TextLayoutDetails,
) -> Range<DisplayPoint> {
    let start = match direction {
        Direction::Both | Direction::Backwards => {
            let times = if text_layout_details.scroll_anchor.offset.y == 0. {
                0
            } else {
                1
            };
            window_top(map, DisplayPoint::zero(), &text_layout_details, times).0
        }
        Direction::Forwards => selection.end,
    };
    let end = match direction {
        Direction::Both | Direction::Forwards => {
            window_bottom(
                map,
                DisplayPoint::new(DisplayRow(0), u32::max_value()),
                &text_layout_details,
                0,
            )
            .0
        }
        Direction::Backwards => selection.start,
    };
    start..end
}

pub(crate) fn sort_matches_display(matches: &mut [DisplayPoint], cursor: &DisplayPoint) {
    matches.sort_unstable_by(|a, b| {
        let a_distance = manh_distance(a, cursor, 2.5);
        let b_distance = manh_distance(b, cursor, 2.5);
        if a_distance == b_distance {
            Ordering::Equal
        } else if a_distance < b_distance {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    });
}

fn manh_distance(point_1: &DisplayPoint, point_2: &DisplayPoint, x_bias: f32) -> f32 {
    x_bias * (point_1.row().as_f32() - point_2.row().as_f32()).abs()
        + (point_1.column() as i32 - point_2.column() as i32).abs() as f32
}
