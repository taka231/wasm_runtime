use std::{fs::File, io::Write, os::fd::FromRawFd};

use super::{
    importer::Importer,
    store::{Memory, Store},
    value::Value,
};

#[derive(Debug)]
pub struct WasiSnapshotPreview1 {
    pub file_table: Vec<File>,
}

impl WasiSnapshotPreview1 {
    pub fn new() -> Self {
        unsafe {
            Self {
                file_table: vec![
                    File::from_raw_fd(0),
                    File::from_raw_fd(1),
                    File::from_raw_fd(2),
                ],
            }
        }
    }
    fn fd_write(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let fd = args[0].as_i32()?;
        let mut iovs = args[1].as_i32()? as u32;
        let iovs_len = args[2].as_i32()?;
        let rp = args[3].as_i32()?;

        let file = &mut self.file_table.get_mut(fd as usize).ok_or("Not found fd")?;

        let mut nwritten = 0;
        let memory = &mut store.memory;

        for _ in 0..iovs_len {
            let start = memory_read_4byte(memory, iovs)? as usize;
            iovs += 4;
            let len = memory_read_4byte(memory, iovs)? as usize;
            iovs += 4;
            let end = start + len;
            nwritten += file
                .write(&memory.data[start..end])
                .map_err(|e| e.to_string())?;
        }
        memory.store(rp as u32, 0, 4, &nwritten.to_le_bytes())?;

        Ok(Value::I32(0))
    }
}

fn memory_read_4byte(memory: &Memory, addr: u32) -> Result<i32, String> {
    let bytes = memory.load(addr, 0, 4)?;
    Ok(i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}

impl Importer for WasiSnapshotPreview1 {
    fn call(&mut self, store: &mut Store, name: &str, args: Vec<Value>) -> Result<Value, String> {
        match name {
            "fd_write" => self.fd_write(store, args),
            _ => unimplemented!("{name}"),
        }
    }
}
