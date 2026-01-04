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

	let data : Vec<u8> = program.data_binary();
	let instr : Vec<u8> = program.instruction_binary();

	let file_name : &str = file_path
		.strip_suffix(".bapu")
		.ok_or_else(|| -> () {
			eprintln!("File name {file_path} must contain \".bapu\" ending!")
		})?;

	fs::write(String::from(file_name) + "_instruct.vci", instr).map_err(
		|e : std::io::Error| -> () {
			eprintln!("Failed to write data file because \"{e}\"!");
		},
	)?;
	fs::write(String::from(file_name) + "_data.vci", data).map_err(
		|e : std::io::Error| -> () {
			eprintln!("Failed to write data file because \"{e}\"!");
		},
	)?;

	Ok(())
}
