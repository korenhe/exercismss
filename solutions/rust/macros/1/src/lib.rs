#[macro_export]
macro_rules! hashmap {
    () => {
        ::std::collections::HashMap::new()
    };
    ($($a:expr => $b:expr),+ $(,)?) => {{
        let mut hm = ::std::collections::HashMap::new();
        $(
            hm.insert($a, $b);
        )+
        hm
    }};
}
