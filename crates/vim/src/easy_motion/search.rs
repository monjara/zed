use std::{cmp::Ordering, ops::Range};

use editor::{
    DisplayPoint, Editor, RowExt,
    display_map::{DisplayRow, DisplaySnapshot},
    movement::{TextLayoutDetails, find_boundary_range_fold},
};
use gpui::{Context, Window};
use language::CharClassifier;
use text::{Bias, Selection};

use crate::motion::{window_bottom, window_top};

use super::Direction;

pub(crate) fn enumerate_word_beginnings(
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
    enumerate_word_beginings_in_range(&display_snapshot, start, end)
}

fn enumerate_word_beginings_in_range(
    display_snapshot: &DisplaySnapshot,
    from: DisplayPoint,
    to: DisplayPoint,
) -> Vec<DisplayPoint> {
    let classifier = display_snapshot
        .buffer_snapshot
        .char_classifier_at(from.to_point(display_snapshot));
    let mut results = Vec::new();

    let fold_snapshot = &display_snapshot.fold_snapshot;

    if from.is_zero()
        && fold_snapshot
            .chars_at(display_snapshot.display_point_to_fold_point(from, Bias::Right))
            .next()
            .map(|first_char| classifier.is_word(first_char))
            .unwrap_or_default()
    {
        results.push(DisplayPoint::zero());
    }

    let mut from = display_snapshot.display_point_to_fold_point(from, Bias::Right);
    let to = display_snapshot.display_point_to_fold_point(to, Bias::Right);
    while from < to {
        let Some(new_point) = find_boundary_range_fold(fold_snapshot, from, to, |left, right| {
            is_boundary(&classifier, left, right)
        }) else {
            break;
        };
        if from == new_point {
            break;
        }

        let new_point_display = display_snapshot.fold_point_to_display_point(new_point);
        results.push(new_point_display);
        from = new_point;
    }
    results
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

fn is_boundary(classifier: &CharClassifier, left: char, right: char) -> bool {
    classifier.kind(left) != classifier.kind(right) && classifier.is_word(right)
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
