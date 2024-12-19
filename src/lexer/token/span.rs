#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpanIndex(u32);

impl SpanIndex {
    pub const fn new(value: u32) -> Self {
        Self(value)
    }

    pub const fn to_usize(&self) -> usize {
        self.0 as usize
    }
}

impl From<SpanIndex> for usize {
    fn from(value: SpanIndex) -> Self {
        value.0 as usize
    }
}

impl From<usize> for SpanIndex {
    fn from(value: usize) -> Self {
        Self(value as u32)
    }
}

impl std::ops::Add<SpanLength> for SpanIndex {
    type Output = Self;

    fn add(self, rhs: SpanLength) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl std::ops::Add<usize> for SpanIndex {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs as u32)
    }
}

impl std::ops::Sub<SpanIndex> for SpanIndex {
    type Output = SpanLength;

    fn sub(self, rhs: SpanIndex) -> Self::Output {
        SpanLength(self.0 - rhs.0)
    }
}

impl std::ops::Sub<SpanIndex> for usize {
    type Output = SpanLength;

    fn sub(self, rhs: SpanIndex) -> Self::Output {
        SpanLength(self as u32 - rhs.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SpanLength(u32);

impl SpanLength {
    pub const fn new(value: u32) -> Self {
        Self(value)
    }

    pub const fn to_usize(&self) -> usize {
        self.0 as usize
    }
}

impl From<SpanLength> for usize {
    fn from(value: SpanLength) -> Self {
        value.0 as usize
    }
}

impl From<usize> for SpanLength {
    fn from(value: usize) -> Self {
        Self(value as u32)
    }
}

impl std::ops::Add<usize> for SpanLength {
    type Output = SpanLength;

    fn add(self, rhs: usize) -> Self::Output {
        SpanLength(self.0 + rhs as u32)
    }
}

impl std::ops::Sub<usize> for SpanLength {
    type Output = SpanLength;

    fn sub(self, rhs: usize) -> Self::Output {
        SpanLength(self.0 - rhs as u32)
    }
}
