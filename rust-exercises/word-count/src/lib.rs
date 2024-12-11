use std::collections::HashMap;

/// Count occurrences of words.
pub fn word_count(phrase: &str) -> HashMap<String, u32> {

  let mut map = HashMap::new();

  for word in phrase.split_whitespace() {
    let word = word.to_lowercase();
    let count = map.entry(word).or_insert(0);
    *count += 1;
  }

  map

}