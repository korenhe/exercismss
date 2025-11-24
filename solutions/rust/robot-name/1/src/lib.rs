use once_cell::sync::Lazy;
use std::{collections::HashSet, iter, sync::Mutex};

pub struct Robot {
    name: String,
}

// private static global name set
static USED_NAMES: Lazy<Mutex<HashSet<String>>> = Lazy::new(|| Mutex::new(HashSet::new()));

impl Robot {
    fn genname() -> String {
        let mut name: String = String::new();
        name.push_str(
            &iter::repeat_with(|| (rand::random_range(0..26) as u8 + b'A') as char)
                .take(2)
                .collect::<String>(),
        );
        name.push_str(
            &iter::repeat_with(|| (rand::random_range(0..10) as u8 + b'0') as char)
                .take(3)
                .collect::<String>(),
        );
        name
    }

    fn genunique() -> String {
        loop {
            let n = Self::genname();
            let mut hs = USED_NAMES.lock().unwrap();
            if !hs.contains(&n) {
                hs.insert(n.clone());
                return n;
            }
        }
    }

    pub fn new() -> Self {
        Robot {
            name: Self::genunique(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn reset_name(&mut self) {
        {
            let mut set = USED_NAMES.lock().unwrap();
            set.remove(&self.name);
        }
        self.name = Self::genunique();
    }
}
