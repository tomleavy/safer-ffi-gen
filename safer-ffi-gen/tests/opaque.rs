use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type(opaque)]
#[derive(Debug, Eq, PartialEq)]
pub struct Foo {
    i: i32,
}

#[safer_ffi_gen]
impl Foo {
    pub fn new(i: i32) -> Foo {
        Self { i }
    }

    pub fn get(&self) -> i32 {
        self.i
    }

    pub fn inc(&mut self) {
        self.i += 1;
    }
}

#[test]
fn opaque_type_can_be_created() {
    let _ = foo_new(33);
}

#[test]
fn by_ref_method_works() {
    let x = foo_new(33);
    assert_eq!(foo_get(&x), 33);
}

#[test]
fn by_ref_mut_method_works() {
    let mut x = foo_new(33);
    foo_inc(&mut x);
    assert_eq!(foo_get(&x), 34);
}

#[test]
fn slice_access_works() {
    let foos = [Foo::new(33), Foo::new(34)];
    assert_eq!(foo_get(foo_slice_get((&foos[..]).into(), 0)), 33);
    assert_eq!(foo_get(foo_slice_get((&foos[..]).into(), 1)), 34);
}

#[test]
fn mutable_slice_access_works() {
    let mut foos = [Foo::new(33), Foo::new(34)];
    assert_eq!(foo_get(foo_slice_get_mut((&mut foos[..]).into(), 0)), 33);
    assert_eq!(foo_get(foo_slice_get_mut((&mut foos[..]).into(), 1)), 34);
    foo_inc(foo_slice_get_mut((&mut foos[..]).into(), 0));
    foo_inc(foo_slice_get_mut((&mut foos[..]).into(), 1));
    assert_eq!(foo_get(foo_slice_get_mut((&mut foos[..]).into(), 0)), 34);
    assert_eq!(foo_get(foo_slice_get_mut((&mut foos[..]).into(), 1)), 35);
}

#[test]
fn vec_of_opaque_items_can_be_created() {
    let foos = foo_vec_new();
    assert_eq!(foos.len(), 0);
}

#[test]
fn vec_of_opaque_items_can_be_freed() {
    let mut foos = foo_vec_new();
    foo_vec_push(&mut foos, foo_new(33));
    foo_vec_free(foos);
}

#[test]
fn vec_of_opaque_items_can_be_pushed_to() {
    let mut foos = foo_vec_new();
    foo_vec_push(&mut foos, foo_new(33));
    foo_vec_push(&mut foos, foo_new(34));
    assert_eq!(&*foos, &[Foo::new(33), Foo::new(34)]);
}

#[test]
fn vec_of_opaque_items_can_be_accessed_as_slice() {
    let mut foos = foo_vec_new();
    foo_vec_push(&mut foos, foo_new(33));
    assert_eq!(&*foo_vec_as_slice(&foos), &[Foo::new(33)]);
}

#[test]
fn vec_of_opaque_items_can_be_accessed_as_mutable_slice() {
    let mut foos = foo_vec_new();
    foo_vec_push(&mut foos, foo_new(33));
    foo_inc(foo_slice_get_mut(foo_vec_as_slice_mut(&mut foos), 0));
    assert_eq!(&*foos, &[Foo::new(34)]);
}
