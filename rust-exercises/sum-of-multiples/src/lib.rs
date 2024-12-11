pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {

    let mut sum = 0;

    for i in 0..limit {
        for j in factors {
            if *j != 0 && i % *j == 0 {
                sum += i;
                break;
            }
        }
    }

    return sum

}