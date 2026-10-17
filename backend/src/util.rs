pub fn cartesian_product_iter<T>(items: &[T]) -> impl Iterator<Item = (&T, &T)> {
    items
        .iter()
        .enumerate()
        .flat_map(|(i, a)| items.iter().skip(i + 1).map(move |b| (a, b)))
}

#[must_use]
pub fn transpose<T>(input: Vec<Vec<T>>) -> Vec<Vec<T>> {
    if input.is_empty() {
        Vec::new()
    } else {
        let cols = input.first().unwrap().len();
        let mut result: Vec<Vec<T>> = (0..cols).map(|_| Vec::with_capacity(input.len())).collect();

        for row in input {
            result
                .iter_mut()
                .zip(row)
                .for_each(|(col, item)| col.push(item));
        }

        result
    }
}
