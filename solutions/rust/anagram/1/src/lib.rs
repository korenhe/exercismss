use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let mut a1 = word.to_lowercase().chars().collect::<Vec<_>>();
    a1.sort();

    possible_anagrams.iter().filter(|x| {
        let mut a2 = x.to_lowercase().chars().collect::<Vec<_>>();
        a2.sort();
        (a2 == a1) && (x.to_lowercase() != word.to_lowercase())
    })
        .copied()
        .collect::<HashSet<&'a str>>()
}
