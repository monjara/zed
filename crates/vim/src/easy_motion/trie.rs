use std::{fmt::Debug, mem};

// Represents the matches for easy motion in a trie.
// Nodes store their leaves in an array with the indices of that array corresponding
// to indices of the key string

// ex: keys: "abc", root: Node { Leaf, Leaf }
// would have leaves with strings of "a" and "b" respectively

// ex: keys: "abc", root: Node { Leaf, Leaf, Node { Leaf, Leaf } }
// would give permutations of "a", "b", "ca", and "cb" respectively

// When new layers are necessary, new layers are assigned to the latest indices first
// so the most preferred keys are kept as short as possible

// notes: There will only ever be two layers separated by one.
// "upper layer" always refers to layer with a smaller depth.
// Ex: in the above leaf_count=4 example the "a" and "b" leaves are in the upper layer
// while the other two are in the lower

#[derive(Debug)]
enum TrieNode<T> {
    Leaf(T),
    Node(Vec<TrieNode<T>>),
}

impl<T> TrieNode<T> {
    fn len(&self) -> usize {
        match self {
            TrieNode::Node(node) => node.iter().fold(0, |acc, curr| acc + curr.len()),
            TrieNode::Leaf(_) => 1,
        }
    }
}

impl<T: Default> Default for TrieNode<T> {
    fn default() -> Self {
        TrieNode::Leaf(Default::default())
    }
}

pub(crate) struct Trie<T> {
    keys: String,
    root: TrieNode<T>,
}

impl<T> Debug for Trie<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Trie")
            .field("keys", &self.keys)
            .finish_non_exhaustive()
    }
}

impl<T> Trie<T> {
    pub fn new_from_vec<TItem, F: Fn(usize, TItem) -> T>(
        keys: String,
        list: Vec<TItem>,
        func: F,
    ) -> Self {
        TrieBuilder::new_from_vec(keys, list, func).populate()
    }

    pub fn len(&self) -> usize {
        self.root.len()
    }

    pub fn trim(&mut self, character: char) -> TrimResult<&T> {
        let node = match &mut self.root {
            TrieNode::Leaf(_) => {
                return TrimResult::Err;
            }
            TrieNode::Node(map) => {
                let index = self.keys.find(character);
                let Some(index) = index else {
                    return TrimResult::NoChange;
                };
                if index >= map.len() {
                    return TrimResult::NoChange;
                }
                map.swap_remove(index)
            }
        };
        self.root = node;
        match &self.root {
            TrieNode::Leaf(val) => TrimResult::Found(&val),
            TrieNode::Node(_) => TrimResult::Changed,
        }
    }

    pub fn iter(&self) -> TrieIterator<T> {
        TrieIterator::new(self)
    }
}

#[derive(Debug)]
pub(crate) enum TrimResult<T> {
    Found(T),
    Changed,
    NoChange,
    Err,
}

impl<T: Clone> TrimResult<&T> {
    pub fn cloned(&self) -> TrimResult<T> {
        match *self {
            TrimResult::Found(t) => TrimResult::Found(t.clone()),
            TrimResult::NoChange => TrimResult::NoChange,
            TrimResult::Changed => TrimResult::Changed,
            TrimResult::Err => TrimResult::Err,
        }
    }
}

fn trie_max_depth(keys_len: usize, leaf_count: usize) -> usize {
    if leaf_count > 1 {
        let max_len = f32::from(leaf_count as u16 - 1);
        let keys_len = f32::from(keys_len as u16);
        max_len.log(keys_len) as usize + 1
    } else {
        1
    }
}

/// Gives the count of leaves which will be in the upper layer
/// ex: keys: "abc", leaf_count: 4
/// a b  c
///     b a
/// => 2
fn upper_layer_count(keys_len: usize, leaf_count: usize, max_trie_depth: usize) -> usize {
    if leaf_count <= keys_len {
        return leaf_count;
    }

    // count of nodes in the previous layer
    let lower_layer_count = keys_len.pow((max_trie_depth - 1) as u32);

    // count of elements we are placing in new lowest layer
    let diff = leaf_count - lower_layer_count;

    // when we start adding leaves to a node which has none two leaves will be created.
    // ex ... b   c     next perm    ...  b      c
    //    ...   a b c     --->       ... a b   a b c
    let extra_leaves = (diff - 1) / (keys_len - 1) + 1;

    // higher_count = diff + extra_leaves;
    // lower_count = leaf_count - higher_count;
    // >> simplified
    lower_layer_count - extra_leaves
}

pub(crate) struct TrieBuilder<TItem, TOut, F: Fn(usize, TItem) -> TOut> {
    keys: String,
    list: Vec<TItem>,
    total_leaf_count: usize,
    current_leaf_count: usize,
    upper_node_count: usize,
    max_depth: usize,
    func: F,
}

impl<TItem, TOut, F: Fn(usize, TItem) -> TOut> TrieBuilder<TItem, TOut, F> {
    fn new_from_vec(keys: String, list: Vec<TItem>, func: F) -> Self {
        let keys_len = keys.len();
        let total_leaf_count = list.len();
        let max_depth = trie_max_depth(keys_len, total_leaf_count);
        let upper_node_count = upper_layer_count(keys_len, total_leaf_count, max_depth);
        TrieBuilder {
            total_leaf_count,
            current_leaf_count: 0,
            upper_node_count,
            max_depth,
            keys,
            list,
            func,
        }
    }

    fn populate(mut self) -> Trie<TOut> {
        let iter = mem::take(&mut self.list).into_iter();
        let root = if self.total_leaf_count <= self.keys.len() {
            let (root, mut iter) = self.node_from_iter(1, iter, self.total_leaf_count);
            debug_assert!(iter.next().is_none());
            root
        } else {
            let (root, mut iter) = self.populate_rec(1, iter);
            debug_assert!(iter.next().is_none());
            root
        };
        Trie {
            root,
            keys: self.keys,
        }
    }

    fn populate_rec<I>(&mut self, curr_depth: usize, mut values: I) -> (TrieNode<TOut>, I)
    where
        I: Iterator<Item = TItem>,
    {
        debug_assert!(curr_depth <= self.max_depth);

        let mut new_vec = Vec::new();
        if curr_depth < self.max_depth - 1 {
            for _ in 0..self.keys.len() {
                let (new_node, new_values) = self.populate_rec(curr_depth + 1, values);
                values = new_values;
                new_vec.push(new_node);
            }
            return (TrieNode::Node(new_vec), values);
        }

        for _ in 0..self.keys.len() {
            if self.current_leaf_count < self.upper_node_count {
                new_vec.push(self.oper(curr_depth, values.next().unwrap()));
                self.current_leaf_count += 1;
                continue;
            } else if self.current_leaf_count == self.upper_node_count {
                // the first node on the upper depth which has leaves will not necessarily be full
                let lower_leaf_count = self.total_leaf_count - self.upper_node_count;
                let modulo = lower_leaf_count % self.keys.len();
                let len = if modulo == 0 { self.keys.len() } else { modulo };
                let (new_node, new_values) = self.node_from_iter(curr_depth + 1, values, len);
                values = new_values;
                self.current_leaf_count += len;
                new_vec.push(new_node);
            } else {
                // all the nodes after that will be full though
                let (node, new_values) =
                    self.node_from_iter(curr_depth + 1, values, self.keys.len());
                new_vec.push(node);
                self.current_leaf_count += self.keys.len();
                values = new_values;
            }
        }
        (TrieNode::Node(new_vec), values)
    }

    fn node_from_iter<I>(&self, depth: usize, mut values: I, len: usize) -> (TrieNode<TOut>, I)
    where
        I: Iterator<Item = TItem>,
    {
        let mut new_vec = Vec::new();
        new_vec.reserve_exact(len);
        for _ in 0..len {
            new_vec.push(self.oper(depth, values.next().unwrap()));
        }
        (TrieNode::Node(new_vec), values)
    }

    fn oper(&self, depth: usize, val: TItem) -> TrieNode<TOut> {
        TrieNode::Leaf((self.func)(depth, val))
    }
}

pub struct TrieIterator<'a, T> {
    keys: &'a str,
    stack: Vec<(&'a TrieNode<T>, String)>,
}

impl<'a, T> TrieIterator<'a, T> {
    fn new(trie: &'a Trie<T>) -> Self {
        TrieIterator {
            stack: vec![(&trie.root, String::new())],
            keys: trie.keys.as_ref(),
        }
    }
}

impl<'a, T> Iterator for TrieIterator<'a, T> {
    type Item = (String, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let mut node = self.stack.pop();
        while let Some(curr) = node {
            match curr {
                (TrieNode::Leaf(val), path) => {
                    return Some((path, val));
                }
                (TrieNode::Node(list), path) => {
                    let old_stack = mem::take(&mut self.stack);
                    let new_stack = list
                        .iter()
                        .enumerate()
                        .map(|(i, child)| {
                            let mut path = path.clone();
                            path.push(self.keys.chars().nth(i).unwrap());
                            (child, path)
                        })
                        .rev();
                    self.stack = old_stack.into_iter().chain(new_stack).collect();
                }
            }
            node = self.stack.pop();
        }
        return None;
    }
}
