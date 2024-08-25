#[repr(C, i64)]
#[derive(Clone, Debug)]
pub enum BitSet {
    Small([usize; 2]),
    Big(Vec<usize>),
}

const BITS: usize = usize::BITS as usize;
impl BitSet {
    /// Whatever capacity you can get without allocating.
    pub fn empty() -> BitSet {
        BitSet::Small([0, 0])
    }

    pub fn with_capacity(size: usize) -> Self {
        if size <= 128 {
            BitSet::Small([0, 0])
        } else {
            BitSet::Big(vec![0; size / BITS + 1])
        }
    }

    pub fn get(&self, i: usize) -> bool {
        let v = match self {
            BitSet::Small(v) => v.as_ref(),
            BitSet::Big(v) => v.as_ref(),
        };
        if i >= v.len() * BITS {
            return false;
        }
        let index = i / BITS;
        let bit = i % BITS;
        (v[index] & (1 << bit)) != 0
    }

    pub fn put(&mut self, i: usize, value: bool) {
        let v = match self {
            BitSet::Small(v) => v.as_mut(),
            BitSet::Big(v) => v.as_mut(),
        };
        let index = i / BITS;
        let bit = i % BITS;
        if value {
            v[index] |= (value as usize) << bit;
        } else {
            v[index] &= !((value as usize) << bit);
        }
    }

    pub fn set(&mut self, i: usize) {
        self.insert(i, true)
    }

    pub fn insert(&mut self, i: usize, value: bool) {
        match self {
            BitSet::Small(v) => {
                if i >= 128 {
                    *self = Self::Big(v.to_vec());
                    self.insert(i, value);
                } else {
                    self.put(i, value);
                }
            }
            BitSet::Big(v) => {
                while i >= (v.len() * BITS) {
                    v.push(0);
                }
            }
        }
        self.put(i, value)
    }

    pub fn clear(&mut self) {
        match self {
            BitSet::Small(v) => {
                *v = [0, 0];
            }
            BitSet::Big(v) => {
                v.clear();
            }
        }
    }
}

impl Default for BitSet {
    fn default() -> Self {
        Self::empty()
    }
}

pub fn pops<T>(v: &mut Vec<T>, count: usize) {
    for _ in 0..count {
        v.pop().unwrap();
    }
}

pub fn extend_options<T>(v: &mut Vec<Option<T>>, index: usize) {
    if v.len() > index {
        return;
    }

    let count = index - v.len() + 1;
    v.reserve(count);
    for _ in 0..count {
        v.push(None);
    }
}
