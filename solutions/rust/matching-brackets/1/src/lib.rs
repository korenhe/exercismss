pub fn brackets_are_balanced(string: &str) -> bool {
    let leftlist = ['(','{','['];
    let rightlist = [')','}',']'];
    let iter = string.chars().filter(|x| leftlist.contains(x) || rightlist.contains(x))
        .collect::<Vec<_>>();

    let mut queue = vec![];
    for it in iter.into_iter() {
        if leftlist.contains(&it) {
            queue.push(it);
        } else {
            match (queue.last(), it) {
                (Some('('), ')') | (Some('['), ']') | (Some('{'), '}') => {
                    queue.pop();
                },
                _ => return false,
            }
        }
    }
    queue.is_empty()
}
