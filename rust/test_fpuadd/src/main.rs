
use std::io::prelude::*;
use std::process::{Command, Stdio};

fn main() {

	let mut child_shell = Command::new("/home/ash/temp/clash04/Test")
		.stdin(Stdio::piped())
		.stdout(Stdio::piped())
		.spawn()
		.unwrap();

	let mut ia: u32 = 2147483648;
	let mut ib: u32 = 0;

	let child_in = child_shell.stdin.as_mut().unwrap();
	let mut child_out = std::io::BufReader::new(child_shell.stdout.as_mut().unwrap());
	let mut line = String::new();

	for _ in 0..100000 {

		line.clear();

		let text = format!("{}\n{}\n", ia, ib);

		child_in.write(text.as_bytes()).unwrap();
		child_out.read_line(&mut line).unwrap();

		line.pop();

		let fa: f32 = unsafe {
			std::mem::transmute(ia)
		};
		let fb: f32 = unsafe {
			std::mem::transmute(ib)
		};
		let fc = fa + fb;
		let ic: u32 = unsafe {
			std::mem::transmute(fc)
		};

		let output_c: u32 = line.parse::<u32>().unwrap();

		println!("{} + {} = {}", ia, ib, ic);
		println!("from haskell {}", output_c);

		assert!(output_c == ic);

		ia = ia + 1;
	}
}
