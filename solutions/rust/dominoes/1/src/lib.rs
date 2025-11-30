fn chainup(head: Option<u8>, last: Option<u8>, input: Vec<(u8, u8)>) -> Option<Vec<(u8, u8)>> {
    if input.is_empty() {
        if let (Some(h), Some(l)) = (head, last) {
            if h != l {
                return None;
            }
        }
        return Some(vec![]);
    }

    for (i, pair) in input.iter().enumerate() {
        let mut remain = input.clone();
        remain.remove(i);

        if let Some(last) = last {
            if last == pair.0 {
                let res = chainup(head, Some(pair.1), remain);
                if let Some(mut rr) = res {
                    rr.push(*pair);
                    return Some(rr);
                }
            } else if last == pair.1 {
                let res = chainup(head, Some(pair.0), remain);
                if let Some(mut rr) = res {
                    rr.push((pair.1, pair.0));
                    return Some(rr);
                }
            }
        } else {
            let res = chainup(Some(pair.0), Some(pair.1), remain.clone());
            if let Some(mut rr) = res {
                rr.push(*pair);
                return Some(rr);
            }

            let res = chainup(Some(pair.1), Some(pair.0), remain);
            if let Some(mut rr) = res {
                rr.push((pair.1, pair.0));
                return Some(rr);
            }
        }
    }
    None
}

pub fn chain(input: &[(u8, u8)]) -> Option<Vec<(u8, u8)>> {
    let res = chainup(None, None, input.to_vec());
    if let Some(mut res) = res {
        res.reverse();
        Some(res)
    } else {
        None
    }
}
