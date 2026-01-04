// Assembler program for converting .bapu assembly code into a runable binary
// and data file

mod program;

use std::fs;

use crate::program::Program;

fn main() -> Result<(), ()> {
	let args : Vec<String> = std::env::args().collect();

	if args.len() != 2 {
		eprintln!(
			"Too {} args! Correct usage: bad-apu-assembler path/to/file",
			if args.len() < 2 {
				"few"
			} else {
				"many"
			}
		);

		return Err(());
	}

	let file_path : &str = &args[1];

	let file_content : String =
		fs::read_to_string(file_path).map_err(|e : std::io::Error| -> () {
			eprintln!("{e}");
		})?;

	let program : Program = file_content
		.parse::<Program>()
		.map_err(|s : String| -> () {
			eprintln!("{s}");
		})?;

	dbg!(program);

	Ok(())
}
