pub fn build_proverb(list: &[&str]) -> String {
    let mut result = String::new();

    if let Some(mut lastword) = list.first() {
        for (i, word) in list.iter().enumerate() {
            if i != 0 {
                result.push_str(&format!("For want of a {} the {} was lost.\n", lastword, word));
                lastword = word;
            }
        }

        result.push_str(&format!("And all for the want of a {}.", list.first().unwrap()));}
    result
}
