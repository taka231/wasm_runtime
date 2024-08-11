use std::{
    env::{self},
    fs::{File, OpenOptions},
    io::{Read, Seek, SeekFrom, Write},
    os::fd::FromRawFd,
    path::Path,
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
        unsafe {
            Self {
                file_table: vec![
                    Some(File::from_raw_fd(0)),
                    Some(File::from_raw_fd(1)),
                    Some(File::from_raw_fd(2)),
                    Some(current_dir),
                ],
                file_path: vec![None, None, None, Some(".".to_string())],
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
        if fd >= 3 {
            self.file_table[fd as usize] = None;
            self.file_path[fd as usize] = None;
        }
        Ok(Value::I32(0))
    }

    fn fd_read(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
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

        let mut nread = 0;
        let memory = &mut store.memory;

        for _ in 0..iovs_len {
            let start = memory_read_4byte(memory, iovs)? as usize;
            iovs += 4;
            let len = memory_read_4byte(memory, iovs)? as usize;
            iovs += 4;
            let end = start + len;
            nread += file
                .read(&mut memory.data[start..end])
                .map_err(|e| e.to_string())?;
        }
        memory.store(rp as u32, 0, 4, &nread.to_le_bytes())?;

        Ok(Value::I32(0))
    }

    fn environ_sizes_get(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let environc_offset = args[0].as_i32()? as u32;
        let environ_buf_size_offset = args[1].as_i32()? as u32;
        let mut environc: i32 = 0;
        let mut environ_buf_size = 0;
        for env in env::vars() {
            environc += 1;
            environ_buf_size += env.0.len() + 1 + env.1.len() + 1;
        }
        let memory = &mut store.memory;
        memory.store(environc_offset, 0, 4, &environc.to_le_bytes())?;
        memory.store(
            environ_buf_size_offset,
            0,
            4,
            &environ_buf_size.to_le_bytes(),
        )?;
        Ok(Value::I32(0))
    }

    fn environ_get(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let mut environ_offset = args[0].as_i32()? as u32;
        let mut environ_buf_offset = args[1].as_i32()? as i32;
        for (key, value) in env::vars() {
            store
                .memory
                .store(environ_offset, 0, 4, &environ_buf_offset.to_le_bytes())?;
            environ_offset += 4;
            let text = format!("{key}={value}\0");
            store.memory.store(
                environ_buf_offset as u32,
                0,
                text.len() as u32,
                text.as_bytes(),
            )?;
            environ_buf_offset += text.len() as i32;
        }
        Ok(Value::I32(0))
    }

    fn path_open(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let fd = args[0].as_i32()?;
        // let dirflags = args[1].as_i32()?;
        let path_offset = args[2].as_i32()? as u32;
        let path_len = args[3].as_i32()?;
        let oflags = args[4].as_i32()?;
        let rights_base = args[5].as_i64()?;
        // let rights_inheriting = args[6].as_i64()?;
        let fdflags = args[7].as_i32()?;
        let opened_fd_offset = args[8].as_i32()? as u32;

        let Some(Some(path)) = self.file_path.get(fd as usize) else {
            return Ok(Self::ERRNO_INVAL.into());
        };

        let file_path = store
            .memory
            .load(path_offset, 0, path_len as u32)?
            .into_iter()
            .map(|b| *b as char)
            .collect::<String>();
        let file_path = file_path.trim_matches('\0');
        let resolved_path = Path::new(path).join(file_path);
        let open_options = OpenOptions::new()
            .create((oflags & Self::OFLAGS_CREAT) != 0)
            .truncate((oflags & Self::OFLAGS_TRUNC) != 0)
            .create_new((oflags & Self::OFLAGS_EXCL) != 0)
            .read((rights_base & (Self::RIGHTS_FD_READ | Self::RIGHTS_FD_READDIR)) != 0)
            .write(
                (rights_base
                    & (Self::RIGHTS_FD_DATASYNC
                        | Self::RIGHTS_FD_WRITE
                        | Self::RIGHTS_FD_ALLOCATE
                        | Self::RIGHTS_FD_FILESTAT_SET_SIZE))
                    != 0,
            )
            .append((fdflags & Self::FDFLAGS_APPEND) != 0)
            .open(&resolved_path)
            .map_err(|e| e.to_string())?;
        self.file_table.push(Some(open_options));
        let opened_fd = self.file_table.len() as i32 - 1;
        self.file_path
            .push(Some(resolved_path.to_str().unwrap().to_string()));
        store
            .memory
            .store(opened_fd_offset, 0, 4, &opened_fd.to_le_bytes())?;

        Ok(Value::I32(0))
    }

    #[cfg(unix)]
    fn fd_filestat_get(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let fd = args[0].as_i32()?;
        let mut buf = args[1].as_i32()? as u32;

        let Some(Some(file)) = self.file_table.get(fd as usize) else {
            return Ok(Self::ERRNO_BADF.into());
        };

        let metadata = file.metadata().map_err(|e| e.to_string())?;
        let file_type = metadata.file_type();
        let file_type = if file_type.is_file() {
            Self::FILETYPE_REGULAR_FILE
        } else if file_type.is_dir() {
            Self::FILETYPE_DIRECTORY
        } else if file_type.is_symlink() {
            Self::FILETYPE_SYMBOLIC_LINK
        } else {
            Self::FILETYPE_UNKNOWN
        };
        use std::os::unix::fs::MetadataExt;
        store
            .memory
            .store(buf, 0, 8, &(metadata.dev() as i64).to_le_bytes())?;
        buf += 8;
        store
            .memory
            .store(buf, 0, 8, &(metadata.ino() as i64).to_le_bytes())?;
        buf += 8;
        store.memory.store(buf, 0, 1, &[file_type])?;
        buf += 8;
        store
            .memory
            .store(buf, 0, 4, &(metadata.nlink() as i32).to_le_bytes())?;
        buf += 8;
        store
            .memory
            .store(buf, 0, 8, &(metadata.size() as i64).to_le_bytes())?;
        buf += 8;
        store
            .memory
            .store(buf, 0, 8, &(metadata.atime()).to_le_bytes())?;
        buf += 8;
        store
            .memory
            .store(buf, 0, 8, &(metadata.mtime()).to_le_bytes())?;
        buf += 8;
        store
            .memory
            .store(buf, 0, 8, &(metadata.ctime()).to_le_bytes())?;

        Ok(Value::I32(0))
    }

    #[cfg(not(unix))]
    fn fd_filestat_get(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let fd = args[0].as_i32()?;
        let mut buf = args[1].as_i32()? as u32;

        let Some(Some(file)) = self.file_table.get(fd as usize) else {
            return Ok(Self::ERRNO_BADF.into());
        };

        let metadata = file.metadata().map_err(|e| e.to_string())?;
        let file_type = metadata.file_type();
        let file_type = if file_type.is_file() {
            Self::FILETYPE_REGULAR_FILE
        } else if file_type.is_dir() {
            Self::FILETYPE_DIRECTORY
        } else if file_type.is_symlink() {
            Self::FILETYPE_SYMBOLIC_LINK
        } else {
            Self::FILETYPE_UNKNOWN
        };
        // store
        //     .memory
        //     .store(buf, 0, 8, &(metadata.dev() as i64).to_le_bytes())?;
        // buf += 8;
        // store
        //     .memory
        //     .store(buf, 0, 8, &(metadata.ino() as i64).to_le_bytes())?;
        // buf += 8;
        // store.memory.store(buf, 0, 1, &[file_type])?;
        // buf += 8;
        // store
        //     .memory
        //     .store(buf, 0, 4, &(metadata.nlink() as i32).to_le_bytes())?;
        // buf += 8;
        // store
        //     .memory
        //     .store(buf, 0, 8, &(metadata.size() as i64).to_le_bytes())?;
        // buf += 8;
        // store
        //     .memory
        //     .store(buf, 0, 8, &(metadata.atime()).to_le_bytes())?;
        // buf += 8;
        // store
        //     .memory
        //     .store(buf, 0, 8, &(metadata.mtime()).to_le_bytes())?;
        // buf += 8;
        // store
        //     .memory
        //     .store(buf, 0, 8, &(metadata.ctime()).to_le_bytes())?;

        Ok(Value::I32(0))
    }

    fn fd_seek(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let fd = args[0].as_i32()?;
        let offset = args[1].as_i64()?;
        let whence = args[2].as_i32()?;
        let new_offset_offset = args[3].as_i32()?;

        let Some(Some(file)) = self.file_table.get_mut(fd as usize) else {
            return Ok(Self::ERRNO_BADF.into());
        };

        let new_offset = match whence {
            0 => file.seek(SeekFrom::Start(offset as u64)),
            1 => file.seek(SeekFrom::Current(offset)),
            2 => file.seek(SeekFrom::End(offset)),
            _ => return Ok(Self::ERRNO_INVAL.into()),
        }
        .map_err(|e| e.to_string())?;

        store
            .memory
            .store(new_offset_offset as u32, 0, 8, &new_offset.to_le_bytes())?;

        Ok(Value::I32(0))
    }

    fn args_sizes_get(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let argc_offset = args[0].as_i32()? as u32;
        let argv_buf_size_offset = args[1].as_i32()? as u32;
        let mut argc: i32 = 0;
        let mut argv_buf_size: i32 = 0;
        for arg in env::args() {
            argc += 1;
            argv_buf_size += arg.len() as i32 + 1;
        }
        let memory = &mut store.memory;
        memory.store(argc_offset, 0, 4, &argc.to_le_bytes())?;
        memory.store(argv_buf_size_offset, 0, 4, &argv_buf_size.to_le_bytes())?;
        Ok(Value::I32(0))
    }

    fn args_get(&mut self, store: &mut Store, args: Vec<Value>) -> Result<Value, String> {
        let mut argv_offset = args[0].as_i32()? as u32;
        let mut argv_buf_offset = args[1].as_i32()? as u32;
        for arg in env::args() {
            store
                .memory
                .store(argv_offset, 0, 4, &argv_buf_offset.to_le_bytes())?;
            argv_offset += 4;
            let text = format!("{arg}\0");
            store
                .memory
                .store(argv_buf_offset, 0, text.len() as u32, text.as_bytes())?;
            argv_buf_offset += text.len() as u32;
        }
        Ok(Value::I32(0))
    }

    const RIGHTS_FD_READ: i64 = 2;
    const RIGHTS_FD_READDIR: i64 = 0x4000;
    const RIGHTS_FD_DATASYNC: i64 = 0x1;
    const RIGHTS_FD_WRITE: i64 = 0x40;
    const RIGHTS_FD_ALLOCATE: i64 = 0x100;
    const RIGHTS_FD_FILESTAT_SET_SIZE: i64 = 0x400000;
    const FDFLAGS_APPEND: i32 = 0x1;
    const OFLAGS_CREAT: i32 = 0x1;
    const OFLAGS_EXCL: i32 = 0x4;
    const OFLAGS_TRUNC: i32 = 0x8;
    const FILETYPE_UNKNOWN: u8 = 0;
    const FILETYPE_DIRECTORY: u8 = 3;
    const FILETYPE_REGULAR_FILE: u8 = 4;
    const FILETYPE_SYMBOLIC_LINK: u8 = 7;

    const ERRNO_BADF: i32 = 8;
    const ERRNO_INVAL: i32 = 28;
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
            "fd_read" => self.fd_read(store, args),
            "environ_sizes_get" => self.environ_sizes_get(store, args),
            "environ_get" => self.environ_get(store, args),
            "path_open" => self.path_open(store, args),
            "fd_filestat_get" => self.fd_filestat_get(store, args),
            "fd_seek" => self.fd_seek(store, args),
            "args_sizes_get" => self.args_sizes_get(store, args),
            "args_get" => self.args_get(store, args),
            _ => unimplemented!("{name}"),
        }
    }
    fn name(&self) -> &str {
        "wasi_snapshot_preview1"
    }
}
