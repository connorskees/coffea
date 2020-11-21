#[derive(Debug)]
pub(crate) enum ArgType<'a> {
    Stack,
    Pool(&'a str),
}

#[derive(Debug)]
pub(crate) struct InvokeDynamicArgs<'a> {
    inner: Vec<&'a str>,
}

impl<'a> InvokeDynamicArgs<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut inner = Vec::new();

        let mut last = 0;
        for (index, matched) in s.match_indices('\u{1}') {
            if last != index {
                inner.push(&s[last..index]);
            }
            inner.push(matched);
            last = index + matched.len();
        }
        if last < s.len() {
            inner.push(&s[last..]);
        }

        Self { inner }
    }

    pub fn next(&mut self) -> Option<ArgType> {
        self.inner.pop().map(|s| match s {
            "\u{1}" => ArgType::Stack,
            s => ArgType::Pool(s),
        })
    }
}
