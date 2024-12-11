pub fn collatz(mut n: u64) -> Option<u64> {

  let mut count = 0;

  while n != 1 {
    if (n % 2) == 0 {
      n = n/2;
    }
    else{
      n = 3 * n + 1
    }
    count+= 1;
  }

  return Some(count);

}
