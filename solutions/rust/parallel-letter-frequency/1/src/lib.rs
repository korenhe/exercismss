use std::{collections::HashMap, sync::Arc, thread::{self, JoinHandle}};

fn count(input: &[String]) -> HashMap<char, usize> {
    let mut hmap = HashMap::<char, usize>::new();

    input.iter().for_each(|word| {
        word.chars().for_each(|ch| {
            if ch.is_alphabetic() {
                *hmap.entry(ch.to_ascii_lowercase()).or_insert(0)+=1;
            }
        });
    });
    hmap
}

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    let chunk_size = input.len().div_ceil(worker_count);
    let mut mergedmap = HashMap::<char, usize>::new();
    if chunk_size == 0 {
        return mergedmap;
    }

    let owned_chunks: Vec<Vec<String>> =
    input.chunks(chunk_size)
         .map(|c| c.iter().map(|s| s.to_string()).collect())
        .collect();
    let owned_chunks = Arc::new(owned_chunks);
    let mut thread_handles = Vec::<JoinHandle<_>>::new();

    for i in 0..owned_chunks.len() {
        let chunks = Arc::clone(&owned_chunks);
        let handle = thread::spawn(move || {
            count(&chunks[i])
        });
        thread_handles.push(handle);
    }


    for th in thread_handles {
        let map = th.join().unwrap();
        for iter in map {
            *mergedmap.entry(iter.0).or_insert(0) += iter.1;
        }
    }

    mergedmap
}
