use super::SystemContext;

pub struct StdioContext;

impl SystemContext for StdioContext {
    fn writeln(&mut self, text: &str) {
        println!("{text}");
    }
}

pub struct BufferedContext {
    buffer: String,
}

impl BufferedContext {
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
        }
    }

    pub fn into_data(self) -> String {
        self.buffer
    }
}

impl SystemContext for BufferedContext {
    fn writeln(&mut self, text: &str) {
        self.buffer.push_str(text);
        self.buffer.push('\n');
    }
}
