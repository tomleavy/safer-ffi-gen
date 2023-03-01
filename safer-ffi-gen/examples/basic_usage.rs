use safer_ffi::derive_ReprC;
use safer_ffi_gen::{safer_ffi_gen, safer_ffi_gen_func};

#[derive_ReprC]
#[ReprC::opaque]
pub struct TestStruct {
    private_vec: Vec<u8>,
    private_string: String,
}

impl safer_ffi_gen::FfiType for TestStruct {
    type Safe = safer_ffi::boxed::Box<TestStruct>;

    fn from_safe(x: Self::Safe) -> Self {
        *x.into()
    }

    fn into_safe(self) -> Self::Safe {
        safer_ffi::boxed::Box::new(self)
    }
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

    #[safer_ffi_gen_func]
    pub fn do_something_with_string(&self, input: String) -> String {
        format!("{} was the input", input)
    }

    #[safer_ffi_gen_func]
    pub fn do_something_mut_with_string(&mut self, input: String) -> String {
        format!("{} was the input", input)
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
