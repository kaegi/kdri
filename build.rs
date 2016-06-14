extern crate gcc;

fn main() {
    println!("cargo:rustc-flags=-l dylib=bluetooth");
    gcc::Config::new().include("inc/").file("cextras/lib.c").compile("libcextras.a");
}
