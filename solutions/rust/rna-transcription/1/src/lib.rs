#[derive(Debug, PartialEq, Eq)]
pub struct Dna {
    strand: Vec<char>
}

#[derive(Debug, PartialEq, Eq)]
pub struct Rna {
    strand: Vec<char>
}

impl Dna {
    pub fn new(dna: &str) -> Result<Dna, usize> {
        let mut strand = Vec::<char>::new();
        for (i, x) in dna.chars().enumerate() {
            match x {
                'A'|'T'|'G'|'C' => strand.push(x),
                _ => return Err(i),
            }
        }
        Ok(Dna {
            strand
        })
    }

    pub fn into_rna(self) -> Rna {
        Rna {
            strand: self.strand.iter().map(|x| {
                match x {
                    'A' => 'U',
                    'T' => 'A',
                    'C' => 'G',
                    'G' => 'C',
                    _ => 'x',
                }
            }).collect::<Vec<char>>()}
    }
}

impl Rna {
    pub fn new(rna: &str) -> Result<Rna, usize> {
        let mut strand = Vec::<char>::new();
        for (i, x) in rna.chars().enumerate() {
            match x {
                'A'|'U'|'G'|'C' => strand.push(x),
                _ => return Err(i),
            }
        }
        Ok(Rna {
            strand
        })
    }
}
