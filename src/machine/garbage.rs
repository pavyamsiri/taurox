#[derive(Debug, Clone, Copy)]
pub struct VMStringRef {
    index: u32,
    generation: u32,
}

pub struct StringAllocator {
    data: Vec<String>,
    markers: Vec<bool>,
    generation: Vec<u32>,
}

pub struct StringAllocatorIterator<'a> {
    strings: &'a [String],
    markers: &'a [bool],
    generation: &'a [u32],
    index: usize,
}

impl<'a> std::iter::Iterator for StringAllocatorIterator<'a> {
    type Item = (&'a str, bool, u32);

    fn next(&mut self) -> Option<Self::Item> {
        let string = self.strings.get(self.index)?;
        let marker = self.markers.get(self.index)?;
        let generation = self.generation.get(self.index)?;
        self.index += 1;
        Some((string, *marker, *generation))
    }
}

impl StringAllocator {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            markers: Vec::new(),
            generation: Vec::new(),
        }
    }

    pub fn iter<'a>(&'a self) -> StringAllocatorIterator<'a> {
        StringAllocatorIterator {
            strings: &self.data,
            markers: &self.markers,
            generation: &self.generation,
            index: 0,
        }
    }

    pub fn allocate(&mut self, text: &str) -> VMStringRef {
        const MSG: &'static str = "All string allocator arrays should have the same length.";
        self.verify_integrity();

        // Find the first empty cell
        match self.markers.iter().enumerate().find(|&(_, &v)| !v) {
            Some((index, _)) => {
                let generation = *self.generation.get(index).expect(MSG);
                // Allocate string
                *self.data.get_mut(index).expect(MSG) = text.to_string();

                VMStringRef {
                    index: index as u32,
                    generation,
                }
            }
            None => {
                // Allocate new data
                self.data.push(text.to_string());
                self.markers.push(true);
                self.generation.push(0);

                VMStringRef {
                    index: (self.data.len() - 1) as u32,
                    generation: 0,
                }
            }
        }
    }

    pub fn get(&self, handle: VMStringRef) -> Option<&str> {
        const MSG: &'static str = "All string allocator arrays should have the same length.";
        self.verify_integrity();

        let generation = self.generation.get(handle.index as usize)?;

        if generation != &handle.generation {
            panic!("Given a stale handle!");
        }

        let marker = self.markers.get(handle.index as usize).expect(MSG);

        if !marker {
            panic!("Given a handle to a dead value!");
        }

        let value = self.data.get(handle.index as usize).expect(MSG);
        Some(value)
    }

    pub fn mark(&mut self, handles: &[VMStringRef]) {
        self.verify_integrity();
        // Mark everything as dead
        self.markers.iter_mut().for_each(|m| {
            *m = false;
        });
        // Mark all given as alive
        for handle in handles {
            let generation = self
                .generation
                .get(handle.index as usize)
                .expect("[Mark]: Invalid handle encountered.");
            if generation != &handle.generation {
                panic!("[Mark]: Given handle is stale.");
            }
            *self.markers.get_mut(handle.index as usize).expect(
                "[Mark]: This handle was verified as valid when it indexed the generations.",
            ) = true;
        }
    }

    fn verify_integrity(&self) {
        assert_eq!(
            self.data.len(),
            self.markers.len(),
            "Data and markers should have same length."
        );
        assert_eq!(
            self.markers.len(),
            self.generation.len(),
            "Data, markers and generation should have same length."
        );
    }
}
