use std::collections::HashMap;

// get N combinations from a vector
fn combination(keys: &[u32], n: usize) -> Vec<Vec<u32>> {
    if keys.is_empty() || n == 0 {
        return vec![];
    }

    let firstk = keys[0];
    let mut res = Vec::<Vec<u32>>::new();
    let mut res1 = combination(&keys[1..], n - 1);

    if res1.is_empty() {
        res1.push(vec![firstk]);
    } else {
        for r in res1.iter_mut() {
            r.push(firstk);
        }
    }

    let mut res2 = combination(&keys[1..], n);
    res.append(&mut res1);
    res.append(&mut res2);

    res
}

fn price_for_n (n: usize) -> u32 {
    match n {
        1 => 800,
        2 => ((800f32 * 0.95) * n as f32) as u32,
        3 => ((800f32 * 0.90) * n as f32) as u32,
        4 => ((800f32 * 0.80) * n as f32) as u32,
        5 => ((800f32 * 0.75) * n as f32) as u32,
        _ => 0,
    }
}

fn canonical(hm: &HashMap<u32, usize>) -> Vec<(u32, usize)> {
    hm.iter().map(|(k,v)| (*k, *v)).collect::<Vec<(u32, usize)>>()
}

fn lowest(hm: HashMap<u32, usize>, memo: &mut HashMap<Vec<(u32, usize)>, u32>) -> u32 {
    if hm.is_empty() {
        return 0;
    }
    let keys = hm.keys().cloned().collect::<Vec<_>>();
    let mut lmin = 100000;

    for i in 1..=hm.len() {
        // extract i different books from hm
        let list = combination(&keys, i);
        for l in list {
            let mut hmc = hm.clone();

            for item in l {
                hmc.entry(item).and_modify(|v| *v = v.saturating_sub(1));
            }

            hmc.retain(|_, count| *count > 0);

            let can = canonical(&hmc);
            if memo.contains_key(&can) {
                let low = memo.get(&can).unwrap();
                lmin = lmin.min(low + price_for_n(i));
            } else {
                let low =lowest(hmc, memo);
                memo.insert(can, low);
                lmin = lmin.min(low + price_for_n(i));
            }

        }
    }

    lmin
}

pub fn lowest_price(books: &[u32]) -> u32 {
    let mut hm = HashMap::<u32, usize>::new();
    books.iter().for_each(|x| {
        *hm.entry(*x).or_insert(0) += 1;
    });

    let mut memo = HashMap::<Vec<(u32, usize)>, u32>::new();
    lowest(hm, &mut memo)
}
