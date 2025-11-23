pub struct Triangle {
    edges: Vec<u64>
}

impl Triangle {
    pub fn build(sides: [u64; 3]) -> Option<Triangle> {
        let mut ss = sides;
        ss.sort();
        if ss[0] + ss[1] > ss[2] {
            Some(Triangle {edges: ss.to_vec()})
        } else {
            None
        }
    }

    pub fn is_equilateral(&self) -> bool {
        self.edges[0] == self.edges[1] && self.edges[1] == self.edges[2]
    }

    pub fn is_scalene(&self) -> bool {
        self.edges[0]!=self.edges[1] && self.edges[1] != self.edges[2]
    }

    pub fn is_isosceles(&self) -> bool {
        self.edges[0]==self.edges[1] || self.edges[1] == self.edges[2]
    }
}
