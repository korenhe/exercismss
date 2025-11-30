pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    if input.is_empty() {
        return vec![];
    }

    // find row's max and col's min
    let mut colmins = vec![10000; input[0].len()];
    let mut rowmaxs = vec![10000; input.len()];
    for (j, row) in input.iter().enumerate() {
        let mut rowmax = 0u64;
        for (i, col) in row.iter().enumerate() {
            rowmax = rowmax.max(*col);
            colmins[i] = colmins[i].min(*col);
        }
        rowmaxs [j] = rowmax;
    }

    let mut res = Vec::<(usize, usize)>::new();
    for (j, row) in input.iter().enumerate() {
        for (i, col) in row.iter().enumerate() {
            if *col == colmins[i] && *col == rowmaxs[j] {
                res.push((j, i));
            }
        }
    }

    res
}
