pub struct VMObjectAllocator<T> {
    data: Vec<T>,
    markers: Vec<bool>,
    generation: Vec<u32>,
}

pub struct VMObjectAllocatorIterator<'a, T> {
    objects: &'a [T],
    markers: &'a [bool],
    generation: &'a [u32],
    index: usize,
}

impl<'a, T> std::iter::Iterator for VMObjectAllocatorIterator<'a, T> {
    type Item = (&'a T, bool, u32);

    fn next(&mut self) -> Option<Self::Item> {
        let object = self.objects.get(self.index)?;
        let marker = self.markers.get(self.index)?;
        let generation = self.generation.get(self.index)?;
        self.index += 1;
        Some((object, *marker, *generation))
    }
}

#[derive(Debug)]
pub struct VMObjectRef<T> {
    index: u32,
    generation: u32,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Clone for VMObjectRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for VMObjectRef<T> {}

impl<T> PartialEq for VMObjectRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index.eq(&other.index) && self.generation.eq(&other.index)
    }
}

impl<T> VMObjectAllocator<T> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            markers: Vec::new(),
            generation: Vec::new(),
        }
    }

    pub fn iter<'a>(&'a self) -> VMObjectAllocatorIterator<'a, T> {
        self.verify_integrity();
        VMObjectAllocatorIterator {
            objects: &self.data,
            markers: &self.markers,
            generation: &self.generation,
            index: 0,
        }
    }

    pub fn get_size(&self) -> usize {
        let num_alive = self.markers.iter().filter(|v| **v).count();
        num_alive * std::mem::size_of::<T>()
    }

    pub fn should_collect(&self) -> bool {
        true
    }

    pub fn allocate(&mut self, object: T) -> VMObjectRef<T> {
        const MSG: &'static str = "All allocator arrays should have the same length.";
        self.verify_integrity();

        // Find the first empty cell
        match self.markers.iter().enumerate().find(|&(_, &v)| !v) {
            Some((index, _)) => {
                let generation = *self.generation.get(index).expect(MSG);
                // Allocate object
                *self.data.get_mut(index).expect(MSG) = object;

                VMObjectRef {
                    index: index as u32,
                    generation,
                    _marker: std::marker::PhantomData,
                }
            }
            None => {
                // Allocate new data
                self.data.push(object);
                self.markers.push(true);
                self.generation.push(0);

                VMObjectRef {
                    index: (self.data.len() - 1) as u32,
                    generation: 0,
                    _marker: std::marker::PhantomData,
                }
            }
        }
    }

    pub fn get(&self, handle: VMObjectRef<T>) -> Option<&T> {
        const MSG: &'static str = "All allocator arrays should have the same length.";
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

    pub fn debug_get(&self, handle: VMObjectRef<T>) -> Option<&T> {
        const MSG: &'static str = "All allocator arrays should have the same length.";
        self.verify_integrity();
        let value = self.data.get(handle.index as usize).expect(MSG);
        Some(value)
    }

    pub fn mark(&mut self, handles: &[VMObjectRef<T>]) {
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

impl VMObjectAllocator<String> {
    pub fn allocate_from_ref(&mut self, object: &str) -> VMObjectRef<String> {
        let object = object.to_owned();
        self.allocate(object)
    }
}
