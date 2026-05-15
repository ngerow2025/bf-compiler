pub fn cartesian_product_iter<T>(items: &Vec<T>) -> impl Iterator<Item = (&T, &T)> {
    items
        .iter()
        .enumerate()
        .flat_map(move |(i, item1)| items.iter().skip(i + 1).map(move |item2| (item1, item2)))
}

pub fn transpose<T>(input: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let cols = input[0].len();
    let mut result: Vec<Vec<T>> = (0..cols).map(|_| Vec::new()).collect();

    for row in input {
        for (i, item) in row.into_iter().enumerate() {
            result[i].push(item);
        }
    }

    result
}
