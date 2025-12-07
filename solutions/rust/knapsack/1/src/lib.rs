#[derive(Debug)]
pub struct Item {
    pub weight: u32,
    pub value: u32,
}

pub fn maximum_value(max_weight: u32, items: &[Item]) -> u32 {
    if items.is_empty() || max_weight == 0 {
        return 0;
    }

    // choisi ou pas the priemiere item
    let s1 = &items[0];
    let mut v1 = 0;
    if s1.weight <= max_weight {
        v1 = s1.value + maximum_value(max_weight - s1.weight, &items[1..]);
    }
    let v2 = maximum_value(max_weight, &items[1..]);
    v1.max(v2)
}
