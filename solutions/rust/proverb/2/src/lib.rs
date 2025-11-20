pub fn build_proverb(list: &[&str]) -> String {
    let mut a = list.windows(2)
        .map(|window| {
            format!("For want of a {} the {} was lost.\n", window[0], window[1])
        })
        .collect::<String>();

    if let Some(first) = list.first() {
        a.push_str(&format!("And all for the want of a {}.", first));
    }
    a
}
