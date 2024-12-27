use std::collections::HashSet;
use std::sync::LazyLock;

static KEYWORDS: LazyLock<Vec<&'static str>> = LazyLock::new(|| {
    include_str!("../files/tables/keywords.txt")
        .lines()
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .collect()
});

static KEYWORDS_SET: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    KEYWORDS.iter().cloned().collect()
});

pub fn check_keyword(keyword: &str) -> bool {
    KEYWORDS_SET.contains(keyword)
}