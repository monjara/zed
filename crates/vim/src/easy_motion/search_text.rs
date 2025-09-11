use std::collections::HashMap;

use editor::{DisplayPoint, display_map::DisplayRow};

pub(crate) fn bm_search(text: &str, pattern: &str, start_row: DisplayRow) -> Vec<DisplayPoint> {
    let pattern = pattern.as_bytes();
    let m = pattern.len();
    let mut result: Vec<DisplayPoint> = Vec::new();

    if m == 0 {
        return result;
    }

    let bad_char_table = build_bad_character_table(pattern);
    let good_suffix_table = build_good_suffix_table(pattern);

    for (line_num, line) in text.lines().enumerate() {
        let line_bytes = line.as_bytes();
        let n = line_bytes.len();

        if n < m {
            continue;
        }

        let mut s = 0;
        while s <= n - m {
            let mut j = (m - 1) as isize;

            while j >= 0 && pattern[j as usize].eq_ignore_ascii_case(&line_bytes[s + j as usize]) {
                j -= 1;
            }

            if j < 0 {
                let point = DisplayPoint::new(start_row + line_num as u32, s as u32);
                result.push(point);
                s += good_suffix_table[0];
            } else {
                let bad_char_shift = bad_char_table
                    .get(&line_bytes[s + j as usize])
                    .cloned()
                    .unwrap_or(m);
                let good_suffix_shift = if (j + 1) < m as isize {
                    good_suffix_table[(j + 1) as usize]
                } else {
                    1
                };
                s += std::cmp::max(bad_char_shift, good_suffix_shift);
            }
        }
    }

    result
}

fn build_bad_character_table(pattern: &[u8]) -> HashMap<u8, usize> {
    let mut table = HashMap::new();
    let m = pattern.len();

    for i in 0..m - 1 {
        table.insert(pattern[i], m - 1 - i);
    }

    table
}

fn build_good_suffix_table(pattern: &[u8]) -> Vec<usize> {
    let m = pattern.len();
    let mut table = vec![m; m];
    let mut last_prefix_position = m;

    for i in (0..m).rev() {
        if is_prefix(pattern, i + 1) {
            last_prefix_position = i + 1;
        }
        table[m - 1 - i] = last_prefix_position - i + m - 1;
    }

    for i in 0..m - 1 {
        let slen = suffix_length(pattern, i);
        table[slen] = m - 1 - i + slen;
    }

    table
}

fn is_prefix(pattern: &[u8], p: usize) -> bool {
    let m = pattern.len();
    for i in p..m {
        if pattern[i] != pattern[i - p] {
            return false;
        }
    }
    true
}

fn suffix_length(pattern: &[u8], p: usize) -> usize {
    let m = pattern.len();
    let mut len = 0;

    for i in 0..=p {
        if pattern[p - i] == pattern[m - 1 - i] {
            len += 1;
        } else {
            break;
        }
    }

    len
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bm_search_with_lines() {
        let start_row = DisplayRow(0);
        let text = "abc\nabc\nabc";
        let pattern = "abc";

        assert_eq!(
            bm_search(&text, &pattern, start_row),
            vec![
                DisplayPoint::new(start_row, 0),
                DisplayPoint::new(start_row + 1, 0),
                DisplayPoint::new(start_row + 2, 0),
            ],
        );
    }

    #[test]
    fn test_bm_search_with_no_lines() {
        let start_row = DisplayRow(0);
        let text = "abc";
        let pattern = "abc";

        assert_eq!(
            bm_search(&text, &pattern, start_row),
            vec![DisplayPoint::new(start_row, 0)]
        );
    }

    #[test]
    fn test_fn_search_no_match() {
        let start_row = DisplayRow(0);
        let text = "abc\nabc\nabc";
        let pattern = "def";

        assert_eq!(bm_search(&text, &pattern, start_row), vec![]);
    }
}
