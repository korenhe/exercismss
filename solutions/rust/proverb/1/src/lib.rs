pub fn build_proverb(list: &[&str]) -> String {
    list.windows(2).map(|tuple| {
        format!("For want of a {} the {} was lost.\n",
                tuple.first().expect("x"),
                tuple.get(1).expect("y"))
    }).chain(vec![
        if !list.is_empty() {
            format!("And all for the want of a {}.", list[0])
        } else {
            "".to_string()
        }
    ]).collect::<String>()
}
