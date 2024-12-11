#[derive(Debug, PartialEq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
}

pub struct CircularBuffer<T:Clone> {
    buffer: Vec<Option<T>>,
    size: usize,
    next: usize,
}

impl<T:Clone> CircularBuffer<T> {
    pub fn new(size: usize) -> CircularBuffer<T> {
        CircularBuffer {
            buffer: vec![None; size],
            size: 0,
            next: 0,
        }
    }

    pub fn read(&mut self) -> Result<T, Error> {
        if self.is_empty() {
            return Err(Error:: EmptyBuffer);
        }else{
            let result = self.buffer[(self.buffer.len() + self.next - self.size) % self.buffer.len()].clone();
            self.size -= 1;
            match result {
                Some(res) => Ok(res),
                None => Err(Error:: EmptyBuffer)
            }
        }
    }

    pub fn write(&mut self, byte: T) -> Result<(), Error> {
        if self.is_full() {
            return Err(Error:: FullBuffer);
        }else{
            self.buffer[self.next] = Some(byte);
            self.next = (self.next + 1)%self.buffer.len();
            self.size += 1;
            Ok(())
        }
    }

    pub fn clear(&mut self) {
        self.size = 0;
        self.next = 0;
        self.buffer = vec![None; self.buffer.len()];
    }

    pub fn is_empty(&self) -> bool {
        return self.size == 0;
    }

    pub fn is_full(&self) -> bool {
        return self.buffer.len() == self.size;
    }
}