use std::io::ErrorKind;

use safer_ffi_gen::{ffi_type, safer_ffi_gen, safer_ffi_gen_func};

#[ffi_type(opaque)]
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

    #[safer_ffi_gen_func]
    pub fn do_something_with_string(&self, input: String) -> String {
        format!("{} was the input", input)
    }

    #[safer_ffi_gen_func]
    pub fn do_something_with_str(&self, input: &str) -> String {
        format!("{} was the input", input)
    }

    #[safer_ffi_gen_func]
    pub fn do_something_mut_with_string(&mut self, input: String) -> String {
        format!("{} was the input", input)
    }

    #[safer_ffi_gen_func]
    pub fn do_something_that_can_fail(&self, input: &[u8]) -> Result<String, std::io::Error> {
        Ok(format!("{:?} was the input", input))
    }

    #[safer_ffi_gen_func]
    pub fn do_something_that_can_fail_no_return(&self) -> Result<(), std::io::Error> {
        Ok(())
    }

    #[safer_ffi_gen_func]
    pub fn do_something_that_fails(&self) -> Result<(), std::io::Error> {
        Err(std::io::Error::new(ErrorKind::Other, "test failure"))
    }

    #[safer_ffi_gen_func]
    pub fn do_something_with_slice(&self, input: &[u8]) -> Vec<u8> {
        input.to_vec()
    }

    #[safer_ffi_gen_func]
    pub fn inner_vec(&self) -> &[u8] {
        &self.private_vec
    }

    #[safer_ffi_gen_func]
    pub fn inner_str(&self) -> &str {
        &self.private_string
    }
}

pub fn main() {
    let test_struct = test_struct_new(vec![0, 1].into(), "test".to_string().into());

    assert_eq!(test_struct_do_something_that_fails(&test_struct), -1);

    let err = test_struct_last_error();

    assert!(err.is_some());

    println!("Got error {:?}", err.unwrap());
}
