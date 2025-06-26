use std::{
    collections::HashMap,
    ptr::addr_of_mut,
    sync::{Arc, atomic::AtomicPtr},
};

use bytemuck::Pod;

pub struct SharedMem {
    map: HashMap<u16, Arc<dyn MemoryMappedRegion>>,
    table: EntryTable,
}

impl Drop for SharedMem {
    fn drop(&mut self) {
        for (id, entry) in self.table.entries.iter_mut().enumerate() {
            if let Some(handler) = self.map.get(&(id as u16)) {
                handler.unmap_region(id as u16, *entry.get_mut());
            } else if !entry.get_mut().is_null() {
                unsafe {
                    drop(Box::from_raw(*entry.get_mut()));
                }
            }
        }
    }
}

pub trait MemoryMappedRegion {
    fn read(&self, addr: u32, buffer: &mut [u8]);
    fn read_optional(&self, addr: u32, buffer: &mut [u8]) -> Result<(), ()> {
        self.read(addr, buffer);
        Ok(())
    }
    fn write(&self, addr: u32, buffer: &[u8]);
    fn write_optional(&self, addr: u32, buffer: &[u8]) -> Result<(), ()> {
        self.write(addr, buffer);
        Ok(())
    }
    #[allow(unused)]
    fn unmap_region(&self, region_id: u16, page: *mut Page) {}
}

#[repr(align(65536))]
#[allow(unused)]
pub struct Page {
    data: [u8; 65536],
}

impl Page {
    pub const unsafe fn offset<T>(page: *mut Page, addr: u32) -> *mut T {
        unsafe {
            page.cast::<T>()
                .byte_add((addr & 0xFFFF) as usize & !(std::mem::align_of::<T>() - 1))
        }
    }
}

struct EntryTable {
    entries: [AtomicPtr<Page>; 65536],
}

impl SharedMem {
    pub fn new() -> Arc<Self> {
        let mut s = Arc::<Self>::new_uninit();
        unsafe {
            let ptr = Arc::get_mut(&mut s).unwrap().as_mut_ptr();
            addr_of_mut!((*ptr).table).write_bytes(0, 1);
            addr_of_mut!((*ptr).map).write(HashMap::new());
            s.assume_init()
        }
    }

    pub fn add_mapped_region(
        mut self: &mut Arc<Self>,
        region_id: u16,
        handler: Arc<dyn MemoryMappedRegion>,
    ) -> Result<(), ()> {
        if let Some(s) = Arc::get_mut(&mut self) {
            s.map.insert(region_id, handler);
            Ok(())
        } else {
            Err(())
        }
    }

    #[inline(always)]
    pub fn read<T: Pod>(&self, addr: u32) -> T {
        const {
            assert!(std::mem::align_of::<T>() <= std::mem::align_of::<Page>());
            assert!(std::mem::size_of::<T>() <= std::mem::size_of::<Page>());
        }
        let page =
            self.table.entries[(addr >> 16) as usize].load(std::sync::atomic::Ordering::Relaxed);
        if page.is_null() {
            self.fail_read(addr)
        } else {
            unsafe { Page::offset::<T>(page, addr).read() }
        }
    }

    #[inline(always)]
    pub fn read_optional<T: Pod>(&self, addr: u32) -> Option<T> {
        const {
            assert!(std::mem::align_of::<T>() <= std::mem::align_of::<Page>());
            assert!(std::mem::size_of::<T>() <= std::mem::size_of::<Page>());
        }
        let page =
            self.table.entries[(addr >> 16) as usize].load(std::sync::atomic::Ordering::Relaxed);
        if page.is_null() {
            self.fail_read_optional(addr)
        } else {
            Some(unsafe { Page::offset::<T>(page, addr).read() })
        }
    }

    #[inline(always)]
    pub fn write<T: Pod>(&self, addr: u32, value: T) {
        const {
            assert!(std::mem::align_of::<T>() <= std::mem::align_of::<Page>());
            assert!(std::mem::size_of::<T>() <= std::mem::size_of::<Page>());
        }
        let page =
            self.table.entries[(addr >> 16) as usize].load(std::sync::atomic::Ordering::Relaxed);
        if page.is_null() {
            self.fail_write(addr, value)
        } else {
            unsafe { Page::offset::<T>(page, addr).write(value) }
        }
    }

    #[inline(always)]
    pub fn write_optional<T: Pod>(&self, addr: u32, value: T) -> Result<(), ()> {
        const {
            assert!(std::mem::align_of::<T>() <= std::mem::align_of::<Page>());
            assert!(std::mem::size_of::<T>() <= std::mem::size_of::<Page>());
        }
        let page =
            self.table.entries[(addr >> 16) as usize].load(std::sync::atomic::Ordering::Relaxed);
        if page.is_null() {
            self.fail_write_optional(addr, value)
        } else {
            unsafe { Page::offset::<T>(page, addr).write(value) };
            Ok(())
        }
    }

    fn create_memory_page(&self, addr: u32) -> *mut Page {
        let mut page =
            self.table.entries[(addr >> 16) as usize].load(std::sync::atomic::Ordering::Relaxed);
        loop {
            if !page.is_null() {
                break page;
            }

            let mut new = Box::<Page>::new_uninit();
            unsafe {
                new.as_mut_ptr().write_bytes(0, 1);
            }
            let new = unsafe { new.assume_init() };
            let ptr = Box::leak(new) as *mut Page;
            match self.table.entries[(addr >> 16) as usize].compare_exchange(
                page,
                ptr,
                std::sync::atomic::Ordering::Relaxed,
                std::sync::atomic::Ordering::Relaxed,
            ) {
                Ok(_) => break ptr,
                Err(curr) => unsafe {
                    drop(Box::from_raw(ptr));
                    page = curr;
                },
            }
        }
    }

    #[inline(never)]
    #[cold]
    fn fail_read<T: Pod>(&self, addr: u32) -> T {
        if let Some(handler) = self.map.get(&((addr >> 16) as u16)) {
            let mut data = T::zeroed();
            let slice = unsafe {
                std::slice::from_raw_parts_mut(
                    (&raw mut data).cast::<u8>(),
                    std::mem::size_of::<T>(),
                )
            };
            handler.read(addr, slice);
            data
        } else {
            let page = self.create_memory_page(addr);
            unsafe { Page::offset::<T>(page, addr).read() }
        }
    }

    #[inline(never)]
    #[cold]
    fn fail_read_optional<T: Pod>(&self, addr: u32) -> Option<T> {
        if let Some(handler) = self.map.get(&((addr >> 16) as u16)) {
            let mut data = T::zeroed();
            let slice = unsafe {
                std::slice::from_raw_parts_mut(
                    (&raw mut data).cast::<u8>(),
                    std::mem::size_of::<T>(),
                )
            };
            if handler.read_optional(addr, slice).is_ok() {
                Some(data)
            } else {
                None
            }
        } else {
            None
        }
    }

    #[inline(never)]
    #[cold]
    fn fail_write<T: Pod>(&self, addr: u32, value: T) {
        if let Some(handler) = self.map.get(&((addr >> 16) as u16)) {
            let slice = unsafe {
                std::slice::from_raw_parts(
                    (&raw const value).cast::<u8>(),
                    std::mem::size_of::<T>(),
                )
            };
            handler.write(addr, slice);
        } else {
            let page = self.create_memory_page(addr);
            unsafe { Page::offset::<T>(page, addr).write(value) }
        }
    }

    #[inline(never)]
    #[cold]
    fn fail_write_optional<T: Pod>(&self, addr: u32, value: T) -> Result<(), ()> {
        if let Some(handler) = self.map.get(&((addr >> 16) as u16)) {
            let slice = unsafe {
                std::slice::from_raw_parts(
                    (&raw const value).cast::<u8>(),
                    std::mem::size_of::<T>(),
                )
            };
            handler.write_optional(addr, slice)
        } else {
            Err(())
        }
    }
}

#[test]
fn meow() {
    let mut mem = SharedMem::new();
    struct Test {}

    impl MemoryMappedRegion for Test {
        fn read(&self, addr: u32, buffer: &mut [u8]) {
            println!("read {addr} {buffer:?}");
            buffer.iter_mut().for_each(|b| *b = 0x11);
        }

        fn write(&self, addr: u32, buffer: &[u8]) {
            println!("write {addr} {buffer:?}");
        }
    }

    mem.add_mapped_region(0, Arc::new(Test {})).unwrap();
    test(&mem);
}

#[inline(never)]
pub fn meowww(mem: &SharedMem, nya: u32) -> u32 {
    mem.read(nya)
}

#[inline(never)]
pub fn test(mem: &SharedMem) -> u32 {
    let mut sum = 0u32;
    for i in (0..10).step_by(4) {
        mem.write(i, i)
    }
    for i in (0..10).step_by(4) {
        sum += mem.read::<u32>(i)
    }
    return sum;
}
