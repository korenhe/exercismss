use std::cmp::Ordering;

pub fn find(array: &[i32], key: i32) -> Option<usize> {
    let mid = array.len() / 2;
    let mkey = array.get(mid)?;
    match mkey.cmp(&key) {
        Ordering::Equal => Some(mid),
        Ordering::Greater => find(&array[..mid], key),
        Ordering::Less => find(&array[mid + 1..], key).map(|y| mid + y + 1),
    }
}
