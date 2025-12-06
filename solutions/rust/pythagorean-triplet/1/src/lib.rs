use std::collections::HashSet;

pub fn find(sum: u32) -> HashSet<[u32; 3]> {
    let a = sum / 3;
    let mut hs: HashSet<[u32; 3]> = HashSet::<[u32; 3]>::new();
    for i in 1..=a {
        let ii = i * i;
        for j in i..sum {
            if i + j < sum {
                let k = sum - i - j;
                if k < j {
                    break;
                }

                if i + j > k {
                    let jj = j * j;
                    let kk = k * k;
                    if ii + jj == kk {
                        hs.insert([i, j, k]);
                    }
                }
            } else {
                break;
            }
        }
    }
    hs
}
