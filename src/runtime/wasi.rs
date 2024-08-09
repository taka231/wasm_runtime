use std::{
    env::{self},
    fs::File,
    io::Write,
    os::fd::FromRawFd,
};

use super::{
    importer::Importer,
    store::{Memory, Store},
    value::Value,
};

#[derive(Debug)]
pub struct WasiSnapshotPreview1 {
    pub file_table: Vec<Option<File>>,
    pub file_path: Vec<Option<String>>,
}

impl WasiSnapshotPreview1 {
    pub fn new() -> Self {
        let current_dir = File::open(".").unwrap();
        let current_dir_path = env::current_dir().unwrap().to_str().map(|s| s.to_owned());
        unsafe {
            Self {
                file_table: vec![
                    Some(File::from_raw_fd(0)),
                    Some(File::from_raw_fd(1)),
                    Some(File::from_raw_fd(2)),
                    Some(current_dir),
                ],
                file_path: vec![None, None, None, current_dir_path],
            }
        }
    }
    fn fd_write(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let fd = args[0].as_i32()?;
        let mut iovs = args[1].as_i32()? as u32;
        let iovs_len = args[2].as_i32()?;
        let rp = args[3].as_i32()?;

        let file = self
            .file_table
            .get_mut(fd as usize)
            .ok_or("Not found fd")?
            .as_mut()
            .ok_or("Not found fd")?;

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

    fn random_get(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let buf = args[0].as_i32()? as usize;
        let buf_len = args[1].as_i32()? as usize;
        for i in 0..buf_len {
            let random = rand::random();
            store.memory.data[buf + i] = random;
        }
        Ok(Value::I32(0))
    }

    fn fd_prestat_get(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let fd = args[0].as_i32()?;
        let buf = args[1].as_i32()? as u32;

        let Some(Some(path)) = self.file_path.get(fd as usize) else {
            return Ok(Self::ERRNO_BADF.into());
        };
        store.memory.store(buf as u32, 0, 1, &[0])?;
        store
            .memory
            .store(buf, 4, 4, &(path.len() as i32).to_le_bytes())?;
        Ok(Value::I32(0))
    }

    fn fd_prestat_dir_name(
        &mut self,
        store: &mut Store,
        args: Vec<Value>,
    ) -> Result<Value, String> {
        let fd = args[0].as_i32()? as usize;
        let buf = args[1].as_i32()? as usize;

        let Some(Some(path)) = self.file_path.get(fd as usize) else {
            return Ok(Self::ERRNO_BADF.into());
        };
        for i in 0..path.len() {
            store.memory.data[buf + i] = path.as_bytes()[i];
        }
        Ok(Value::I32(0))
    }

    fn fd_close(&mut self, args: Vec<Value>) -> Result<Value, String> {
        let fd = args[0].as_i32()? as usize;
        self.file_table[fd as usize] = None;
        self.file_path[fd as usize] = None;
        Ok(Value::I32(0))
    }

    const ERRNO_BADF: i32 = 8;
}

fn memory_read_4byte(memory: &Memory, addr: u32) -> Result<i32, String> {
    let bytes = memory.load(addr, 0, 4)?;
    Ok(i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}

impl Importer for WasiSnapshotPreview1 {
    fn call(&mut self, store: &mut Store, name: &str, args: Vec<Value>) -> Result<Value, String> {
        match name {
            "fd_write" => self.fd_write(store, args),
            "random_get" => self.random_get(store, args),
            "fd_prestat_get" => self.fd_prestat_get(store, args),
            "fd_prestat_dir_name" => self.fd_prestat_dir_name(store, args),
            "fd_close" => self.fd_close(args),
            _ => unimplemented!("{name}"),
        }
    }
}
