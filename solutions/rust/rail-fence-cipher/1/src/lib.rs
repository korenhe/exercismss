pub struct RailFence {
    rails: u32,
}

impl RailFence {
    pub fn new(rails: u32) -> RailFence {
        RailFence { rails }
    }

    fn nexty(&self, direction: i32, y: i32) -> (i32, i32) {
        let mut direction = direction;
        let mut y = y;
        y += direction;
        if y == self.rails as i32 - 1 || y == 0 {
            direction = -direction;
        }
        (direction, y)
    }

    pub fn encode(&self, text: &str) -> String {
        let mut direction = 1i32;
        let mut y = 0i32;
        let mut bars = vec![Vec::<char>::new(); self.rails as usize];
        for tx in text.chars() {
            bars[y as usize].push(tx);
            (direction, y) = self.nexty(direction, y);
        }
        bars.iter()
            .flat_map(|row| row.iter())
            .collect::<String>()
    }

    pub fn decode(&self, cipher: &str) -> String {
        let railsize = cipher.len();
        let mut direction = 1i32;
        let mut y = 0i32;
        let mut bars = vec![Vec::<usize>::new(); self.rails as usize];
        for (x, _tx) in cipher.chars().enumerate() {
            bars[y as usize].push(x);
            (direction, y) = self.nexty(direction, y);
        }
        let mut res = Vec::<char>::new();
        res.resize(railsize, ' ');

        let map = bars.iter()
            .flat_map(|row| row.iter().cloned())
            .collect::<Vec<usize>>();

        for (i, ch) in cipher.chars().enumerate() {
            res[map[i]] = ch;
        }

        res.iter().collect::<String>()
    }
}
