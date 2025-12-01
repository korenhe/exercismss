pub struct PascalsTriangle {
    rows: usize,
}

impl PascalsTriangle {
    pub fn new(row_count: u32) -> Self {
        PascalsTriangle {
            rows: row_count as usize,
        }
    }

    fn row(&self, row: usize, lastrow: &[u32]) -> Vec<u32> {
        if row == 0 {
            return vec![1];
        } else if row == 1 {
            return vec![1,1];
        }

        // row >= 2
        let mut res = Vec::<u32>::new();
        res.push(1);
        for i in 1..row {
            res.push(lastrow[i-1]+lastrow[i]);
        }
        res.push(lastrow[row-1]);
        res
    }

    pub fn rows(&self) -> Vec<Vec<u32>> {
        let mut res = Vec::<Vec<u32>>::new();
        let mut lastrow: &[u32] = &[];
        for i in 0..self.rows {
            res.push(self.row(i, lastrow));
            lastrow = res.last().unwrap();
        }
        res
    }
}
