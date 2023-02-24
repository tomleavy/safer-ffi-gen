use safer_ffi_gen::{safer_ffi_gen, safer_ffi_gen_func};

pub struct TestStruct {
    private_vec: Vec<u8>,
    private_string: String,
}

#[safer_ffi_gen]
impl TestStruct {
    #[safer_ffi_gen_func]
    pub fn new(vec_input: Vec<u8>, string_input: String) -> TestStruct {
        TestStruct {
            private_vec: vec_input,
            private_string: string_input,
        }
    }

    pub fn do_something(&self, input: &str) -> String {
        input.to_string()
    }

    pub fn do_something_else(&self, input: &[u8]) -> Vec<u8> {
        input.to_vec()
    }

    pub fn inner_vec(&self) -> &[u8] {
        &self.private_vec
    }

    pub fn inner_str(&self) -> &str {
        &self.private_string
    }
}

pub fn main() {
    println!("This example is just to aid with development")
}
