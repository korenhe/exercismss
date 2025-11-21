#[derive(Debug, PartialEq, Eq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

fn sublist_inner(first_list: &[i32], second_list: &[i32]) -> Comparison {
    assert!(first_list.len() >= second_list.len());
    if second_list.len() == 0 || first_list.windows(second_list.len()).any(|x| x == second_list) {
        Comparison::Superlist
    } else{
        Comparison::Unequal
    }
}

pub fn sublist(first_list: &[i32], second_list: &[i32]) -> Comparison {
    let l1 = first_list.len();
    let l2 = second_list.len();

    if l1 > l2 {sublist_inner(first_list, second_list)}
    else if l1 == l2 {
        let x = sublist_inner(second_list, first_list);
        match x {
            Comparison::Superlist => Comparison::Equal,
            _ => x,
        }
    }
    else {
        let x = sublist_inner(second_list, first_list);
        match x {
            Comparison::Superlist => Comparison::Sublist,
            _ => x,
        }
    }
}
