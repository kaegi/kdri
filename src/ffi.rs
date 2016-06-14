#![allow(dead_code,
         non_camel_case_types,
         non_upper_case_globals,
         non_snake_case)]

extern crate bluetooth;

pub type int8_t = i8;
pub type int16_t = i16;
pub type int32_t = i32;
pub type int64_t = i64;
pub type uint8_t = u8;
pub type uint16_t = u16;
pub type uint32_t = u32;
pub type uint64_t = u64;
pub type int_least8_t = ::std::os::raw::c_char;
pub type int_least16_t = ::std::os::raw::c_short;
pub type int_least32_t = ::std::os::raw::c_int;
pub type int_least64_t = ::std::os::raw::c_long;
pub type uint_least8_t = ::std::os::raw::c_uchar;
pub type uint_least16_t = ::std::os::raw::c_ushort;
pub type uint_least32_t = ::std::os::raw::c_uint;
pub type uint_least64_t = ::std::os::raw::c_ulong;
pub type int_fast8_t = ::std::os::raw::c_char;
pub type int_fast16_t = ::std::os::raw::c_long;
pub type int_fast32_t = ::std::os::raw::c_long;
pub type int_fast64_t = ::std::os::raw::c_long;
pub type uint_fast8_t = ::std::os::raw::c_uchar;
pub type uint_fast16_t = ::std::os::raw::c_ulong;
pub type uint_fast32_t = ::std::os::raw::c_ulong;
pub type uint_fast64_t = ::std::os::raw::c_ulong;
pub type intptr_t = isize;
pub type uintptr_t = usize;
pub type intmax_t = ::std::os::raw::c_long;
pub type uintmax_t = ::std::os::raw::c_ulong;


#[repr(C)]
#[derive(Copy, Clone)]
#[derive(Debug)]
pub struct KettlerDeviceC {
    pub name: *mut ::std::os::raw::c_char,
    pub addr: [uint8_t; 6usize],
}
impl ::std::default::Default for KettlerDeviceC {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}




////////////////////////////////////////////////////////////////////////////////////////////////////
// This part handles inquiries of devices. It has to be directed to a C function call because
// at the time of writing this comment, no BlueZ/HCI-Rust-Binding has been created. The effort
// to write these two following functions is far less than developing the binding.
////////////////////////////////////////////////////////////////////////////////////////////////////

extern "C" {
    pub fn c__kdri_scan_devices(device_array: *mut KettlerDeviceC, max_devices: uint32_t, timeout: uint32_t) -> int32_t;
    pub fn c__kdri_free_device(device: *mut KettlerDeviceC);
    pub fn c__get_rfcomm_channel(addr: bluetooth::BtAddr, class16: uint16_t) -> int32_t;
}
