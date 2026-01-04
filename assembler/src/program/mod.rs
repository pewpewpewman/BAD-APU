use std::collections::HashMap;
use std::str::FromStr;
use std::ops::Mul;
use std::ops::Sub;
use std::ops::BitOr;

#[derive(Default, Debug)]
pub struct Program {
	pub data : DataSegment,
	pub instruction : InstructionSegment,
}

// Primary parsing function - most of the BAD-APU assembler lives here
impl FromStr for Program {
	type Err = String;

	fn from_str(s : &str) -> Result<Program, Self::Err> {
		// A function for filtering out empty lines and comments. Returns true if a
		// line is not a comment or an empty line
		fn is_meaningful(line : &str) -> bool {
			let line : &str = line.trim();
			!(line.is_empty() || line.starts_with("||"))
		}

		let mut program : Program = Program::default();

		let (data, instructions) : (&str, &str) = s
			.split_once("--INSTRUCT--")
			.ok_or_else(|| -> String {
				String::from("Program must contain an \"--INSTRUCT--\" header!")
			})?;

		// TODO: Move parsing code to FromStr implementation of DataSegment and
		// InstructionSegment
		data
			.lines()
			.filter(|s : &&str| -> bool { is_meaningful(*s) })
			.map(str::trim)
			.enumerate()
			.try_fold(
				0,
				|mut data_idx : u16,
				 (iter_idx, s) : (usize, &str)|
				 -> Result<u16, String> {
					// Check for --DATA-- header (such a shit way to do this lmao)
					if iter_idx == 0 {
						if s != "--DATA--" {
							return Err(format!(
								"Content above the \"--INSTRUCT--\" header must begin with a \
								 \"--DATA--\" header, instead begins with \"{s}\"!",
							));
						}
						return Ok(data_idx);
					}

					let data_chunk : DataChunk = s.parse::<DataChunk>()?;

					// Map label to offset before it's incremented
					// proccessing
					program
						.data
						.label_map
						.insert(
							data_chunk
								.label
										.clone(),
							data_idx,
						)
						// Insert returns Some on finding a value (an error to us) so we need to
						// "swap" Some and None. try_insert() is nightly only (the github
						// issue is from 2021 😭)
						.map_or_else(
							|| -> Option<()> { Some(()) },
							|_ : u16| -> Option<()> { None },
						)
						.ok_or_else(|| -> String {
							// Shouldn't .clone() here be uneeded because this closure is only
							// evaluated when we're about to exit and so we won't used the
							// consumed data_chunk? Is that too advanced of a case for the
							// borrow checker?
								format!(
								"Line \"{s}\" has duplicate label {}!",
								data_chunk
									.label
									.clone()
							)
						})?;

					// To appease borrow rules
					let content_len : u16 = data_chunk
						.content
						.len() as u16;

					program
						.data
						.chunks
						.push(data_chunk);

					// We now have a valid immediate and can increment the
					// offset cursor

					data_idx = data_idx
						.checked_add(content_len)
						.ok_or_else(|| -> String {
							format!(
								"Source has too big of a data section! The Bad APU's and main \
								 memory address is 16 bits wide, meaning there can only be a \
								 max of 2^16 - 1 ({}) individual data values!",
								u16::MAX
							)
						})?;

					Ok(data_idx)
				},
			)?;

		// That's the data section all done! :D Time for the actual instructions
		// Instruction parsing requires two passes:
		// 1. Counting each instruction and marking each label location
		// 2. Parsing actual instructions and filling in data from the label map

		// First pass
		instructions
			.lines()
			.filter(|s : &&str| -> bool { is_meaningful(*s) })
			.try_fold(0, |i : u16, s : &str| -> Result<u16, String> {
				if s
					.trim_end()
					.ends_with(':')
				{
					let label : &str = s
						.trim_end()
						.trim_end_matches(':')
						.trim();

					if label.contains(char::is_whitespace) {
						return Err(format!(
							"Invalid label {label}. Label is not a whole word!"
						));
					}

					program
						.instruction
						.label_map
						.insert(String::from(label), i)
						.map_or_else(
							|| -> Option<()> { Some(()) },
							|_ : u16| -> Option<()> { None },
						)
						.ok_or_else(|| -> String {
							format!("Line \"{s}\" has duplicate label {label}!")
						})?;

					Ok(i)
				} else {
					Ok(
						i.checked_add(1)
							.ok_or_else(|| -> String {
								format!(
									"Source has too many instructions! The Bad APU's program \
									 counter and program memory address is 16 bits wide, \
									 meaning there can only be a max of 2^16 - 1 ({}) \
									 instructions!",
									u16::MAX
								)
							})?,
					)
				}
			})?;
		program
			.instruction
			.instructions = instructions
			.lines()
			.filter(|s : &&str| -> bool {
				// Filter out unimportant lines and labels
				is_meaningful(*s)
					&& !s
						.trim_end()
						.ends_with(':')
			})
			.zip(0_u16..)
			.try_fold(
				Vec::<Instruction>::new(),
				|mut v : Vec<Instruction>,
				 (s, instr_num) : (&str, u16)|
				 -> Result<Vec<Instruction>, String> {
					v.push(
						s.parse::<Instruction>()
							.or_else(
								|e : InstructionParseError| -> Result<Instruction, String> {
									match e {
										InstructionParseError::BranchContextNeeded(instr, lab) => {
											// Branching distance is target line number minus
											// current line number

											match instr {
												Instruction::Branch {
													instruction,
													operand_1,
													operand_2,
													..
												} => {
													Ok(Instruction::Branch {
														instruction,
														operand_1,
														operand_2,
														jump : Immediate(
															((*program
																.instruction
																.label_map
																.get(&lab)
																.ok_or_else(|| -> String {
																	format!(
																		"Line {} uses non-existant instruction \
																		 label \"{lab}\"!",
																		s.trim()
																	)
																})?) as i16)
																.sub(instr_num as i16),
														),
													})
												},

												_ => {
													unreachable!()
												},
											}
										},
										InstructionParseError::AddrContextNeeded(instr, lab) => {
											// Addr is simply whatever value the data section label
											// map holds
											match instr {
												Instruction::Alias {
													instruction,
													write_back,
													..
												} => {
													Ok(Instruction::Alias {
														instruction,
														write_back,
														operand_1 : RegImmed::Immed(Immediate(
															*program
																.data
																.label_map
																.get(&lab)
																.ok_or_else(|| -> String {
																	format!(
																		"Line {} uses non-existant data label \
																		 \"{lab}\"!",
																		s.trim()
																	)
																})? as i16,
														)),
													})
												},

												_ => {
													unreachable!()
												},
											}
										},
										InstructionParseError::Typical(e) => {
											Err(format!(
												"Failed to parse instruction \"{}\" because \"{e}\"",
												s.trim()
											))
										},
									}
								},
							)?,
					);

					Ok(v)
				},
			)?;

		Ok(program)
	}
}

impl Program {
	pub fn data_binary(self: &Program) -> Vec<u8> {
		self
			.data
			.chunks
			.iter()
			.map(|dc : &DataChunk| -> &Vec<Immediate> { &dc.content })
			.flatten()
			.map(|i : &Immediate| -> [u8; 2] {
				i.0
					.to_le_bytes()
			})
			.flatten()
			.collect::<Vec<u8>>()
	}

	pub fn instruction_binary(self: &Program) -> Vec<u8> {
		self
			.instruction
			.instructions
			.iter()
			.map(|i : &Instruction| -> [u8; 4] {
				// SAFTEY: I mean can *you* think of a reason any of these these are
				// unsafe?
				match i {
					Instruction::Arith {
						instruction,
						write_back,
						operand_1,
						operand_2,
					} => {
						0b00_u32
							.bitor(
								Into::<u32>::into(unsafe {
									*(instruction as *const ArithInstruction as *const u8)
								}) << 2,
							)
							.bitor(
								Into::<u32>::into(unsafe {
									*(write_back as *const Register as *const u8)
								}) << 7,
							)
							.bitor(
								Into::<u32>::into(unsafe {
									*(operand_1 as *const Register as *const u8)
								}) << 11,
							)
							.bitor(
								match operand_2 {
									RegImmed::Reg(r) => {
										0b1.bitor(
											(unsafe { *(r as *const Register as *const u8) } as u32)
												<< 1,
										)
									},
									RegImmed::Immed(i) => 0b0.bitor((i.0 as u32) << 1),
								} << 15,
							)
					},

					Instruction::Branch {
						instruction,
						operand_1,
						operand_2,
						jump,
					} => {
						0b01
							.bitor(
								Into::<u32>::into(unsafe {
									*(instruction as *const BranchInstruction as *const u8)
								}) << 2,
							)
							.bitor(
								Into::<u32>::into(unsafe {
									*(operand_1 as *const Register as *const u8)
								}) << 11,
							)
							.bitor(
								Into::<u32>::into(unsafe {
									*(operand_2 as *const Register as *const u8)
								}) << 7,
							)
							.bitor((jump.0 as u32) << 16)
					},

					Instruction::Memory {
						instruction,
						wb_and_op,
						memory_address,
						offset,
					} => {
						0b10
							.bitor(
								Into::<u32>::into(unsafe {
									*(instruction as *const MemoryInstruction as *const u8)
								}) << 2,
							)
							.bitor(
								Into::<u32>::into(unsafe {
									*(wb_and_op as *const Register as *const u8)
								}) << 7,
							)
							.bitor(
								Into::<u32>::into(unsafe {
									*(memory_address as *const Register as *const u8)
								}) << 11,
							)
							.bitor(
								match offset {
									RegImmed::Reg(r) => {
										0b1.bitor(
											(unsafe { *(r as *const Register as *const u8) } as u32)
												<< 1,
										)
									},
									RegImmed::Immed(i) => 0b0.bitor((i.0 as u32) << 1),
								} << 15,
							)
					},

					Instruction::Graphic {
						instruction,
						wb_and_op,
						memory_address,
						offset,
					} => {
						0b10
							.bitor(
								Into::<u32>::into(unsafe {
									*(instruction as *const GraphicInstruction as *const u8)
								}) << 2,
							)
							.bitor(
								Into::<u32>::into(unsafe {
									*(wb_and_op as *const Register as *const u8)
								}) << 7,
							)
							.bitor(
								Into::<u32>::into(unsafe {
									*(memory_address as *const Register as *const u8)
								}) << 11,
							)
							.bitor(
								match offset {
									RegImmed::Reg(r) => {
										0b1.bitor(
											(unsafe { *(r as *const Register as *const u8) } as u32)
												<< 1,
										)
									},
									RegImmed::Immed(i) => 0b0.bitor((i.0 as u32) << 1),
								} << 15,
							)
					},

					Instruction::Alias {
						instruction: _,
						write_back,
						operand_1,
					} => {
						0b000000
							.bitor(
								Into::<u32>::into(unsafe {
									*(write_back as *const Register as *const u8)
								}) << 7,
							)
							.bitor((Register::XZERO as u8 as u32) << 11)
							.bitor(
								match operand_1 {
									RegImmed::Reg(r) => {
										0b1.bitor(
											(unsafe { *(r as *const Register as *const u8) } as u32)
												<< 1,
										)
									},
									RegImmed::Immed(i) => 0b0.bitor((i.0 as u32) << 1),
								} << 15,
							)
					},
				}
				.to_le_bytes()
			})
			.flatten()
			.collect::<Vec<u8>>()
	}
}

#[derive(Default, Debug)]
pub struct DataSegment {
	pub chunks : Vec<DataChunk>,
	pub label_map : HashMap<String, u16>,
}

#[derive(Default, Debug)]
pub struct DataChunk {
	pub label : String,
	pub content : Vec<Immediate>,
}

impl FromStr for DataChunk {
	type Err = String;

	fn from_str(s : &str) -> Result<DataChunk, Self::Err> {
		let (label, values) : (&str, &str) = s
			.split_once("<-")
			.map(|(l, v) : (&str, &str)| -> (&str, &str) { (l.trim(), v.trim()) })
			.ok_or_else(|| -> String {
				format!(
					"Data chunk \"{s}\" does not contain \"<-\" for assinging a label \
					 to data!"
				)
			})?;

		Ok(DataChunk {
			label : String::from(label),
			content : values
				.strip_prefix('[')
				.ok_or_else(|| -> String {
					format!(
						"Data chunk \"{s}\" is missing a \"[\" character at the start of \
						 the list of values"
					)
				})?
				.strip_suffix(']')
				.ok_or_else(|| -> String {
					format!(
						"Data chunk \"{s}\" is missing a \"]\" character at the end of \
						 the list of values"
					)
				})?
				.split(',')
				.map(str::trim)
				.map(Immediate::from_str)
				.try_fold(
					// try_collect() is nightly only. Conside this a poor man's
					// try_collect()
					Vec::<Immediate>::new(),
					|mut v : Vec<Immediate>,
					 r : Result<Immediate, String>|
					 -> Result<Vec<Immediate>, String> {
						v.push(r.map_err(|e : String| -> String {
							format!("Data chunk \"{s}\" has invalid data caused by \"{e}\"")
						})?);

						Ok(v)
					},
				)?,
		})
	}
}

#[derive(Default, Debug)]
pub struct InstructionSegment {
	pub instructions : Vec<Instruction>,
	pub label_map : HashMap<String, u16>,
}

#[derive(Debug)]
pub enum Instruction {
	Arith {
		instruction : ArithInstruction,
		write_back : Register,
		operand_1 : Register,
		operand_2 : RegImmed,
	},

	Branch {
		instruction : BranchInstruction,
		operand_1 : Register,
		operand_2 : Register,
		jump : Immediate,
	},

	Memory {
		instruction : MemoryInstruction,
		wb_and_op : Register,
		memory_address : Register,
		offset : RegImmed,
	},

	Graphic {
		instruction : GraphicInstruction,
		wb_and_op : Register,
		memory_address : Register,
		offset : RegImmed,
	},

	// Variant for instructions that are just shorthands for existing
	// instructions. Those being MOVE and ADR, which just map to an ADD
	// instruction with XZERO as the second operand
	Alias {
		instruction : AliasInstruction,
		write_back : Register,
		operand_1 : RegImmed,
	},
}

impl Default for Instruction {
	fn default() -> Instruction {
		Instruction::Arith {
			instruction : ArithInstruction::ADD,
			write_back : Register::default(),
			operand_1 : Register::default(),
			operand_2 : RegImmed::default(),
		}
	}
}

// Enum for failing to parse instructions. Two instructions are special in that
// they require some greater context to fully fill in their fields. Branching
// instructions need their instruction memory offset and ADDR needs the main
// memory address of the label being accessed. Parsing a branching instruction
// will return an error with an Instruction::Branch {} and parsing ADDR will
// give back an Instruction::Alias with the instruction field set to
// AliasInstruction::ADDR. These errors also come with the label they wish to
// use. All other fields other than the needed offsets will be filled in as
// expected. Obviously the parsing function could just use it's own from_str()
// function that takes &Program, but then we wouldn't get to use FromStr, which,
// like, I really want to do that ok. If you know a better way of achieving this
// design goal, leave them in the comment section below.
pub enum InstructionParseError {
	BranchContextNeeded(Instruction, String),
	AddrContextNeeded(Instruction, String),
	Typical(String),
}

impl From<String> for InstructionParseError {
	fn from(s : String) -> InstructionParseError {
		InstructionParseError::Typical(s)
	}
}

impl FromStr for Instruction {
	type Err = InstructionParseError;

	fn from_str(s : &str) -> Result<Instruction, InstructionParseError> {
		let (target, instruction_and_arg) : (&str, &str) = s
			.split_once("<-")
			.map(|(t, iag) : (&str, &str)| -> (&str, &str) { (t.trim(), iag.trim()) })
			.ok_or_else(|| -> InstructionParseError {
				InstructionParseError::Typical(format!(
					"Line \"{}\" is missing \"<-\" for assignment / specifying  \
					 instruction target",
					s.trim()
				))
			})?;

		if !instruction_and_arg.ends_with(')') {
			return Err(InstructionParseError::Typical(format!(
				"Instruction {s} has content after the ending parenthesis!"
			)));
		}

		let open_paren : usize = instruction_and_arg
			.find('(')
			.ok_or_else(|| -> InstructionParseError {
				InstructionParseError::Typical(format!(
					"Instruction {s} is missing its open parenthesis!"
				))
			})?;

		let close_paren : usize = instruction_and_arg
			.find(')')
			.ok_or_else(|| -> InstructionParseError {
				InstructionParseError::Typical(format!(
					"Instruction {s} is missing its close parenthesis!"
				))
			})?;

		let instruction : &str = &instruction_and_arg[..open_paren];

		let arg : &str = instruction_and_arg[open_paren + 1..close_paren].trim();

		// Here we go...
		if let Ok(ai) = instruction.parse::<ArithInstruction>() {
			let ai : ArithInstruction = ai;

			let instruction : ArithInstruction = ai;

			let write_back : Register = target
				.parse::<Register>()
				.map_err(|e : String| -> InstructionParseError {
					InstructionParseError::Typical(e)
				})?;

			let (operand_1, operand_2) : (Register, RegImmed) = match instruction {
				ArithInstruction::RAND => {
					(Register::XZERO, RegImmed::Reg(Register::XZERO))
				},
				_ => {
					arg
						.split_once(',')
						.ok_or_else(|| -> InstructionParseError {
							InstructionParseError::Typical(
								"Arithmatic arguments require 2, comma seperated arguments!"
									.to_owned(),
							)
						})
						.map(|(o1, o2) : (&str, &str)| -> (&str, &str) {
							(o1.trim(), o2.trim())
						})
						.and_then(|(o1, o2) : (&str, &str)| -> Result<(Register, RegImmed), InstructionParseError>  {
							Ok((o1.parse::<Register>()?, o2.parse::<RegImmed>()?))
						})?
				},
			};

			Ok(Instruction::Arith {
				instruction,
				write_back,
				operand_1,
				operand_2,
			})
		} else if let Ok(bi) = instruction.parse::<BranchInstruction>() {
			let bi : BranchInstruction = bi;

			let instruction : BranchInstruction = bi;

			let (operand_1, operand_2) : (Register, Register) = match instruction {
				BranchInstruction::B => (Register::XZERO, Register::XZERO),
				_ => {
					arg
						.split_once(',')
						.map(|(o1, o2) : (&str, &str)| -> (&str, &str) {
							(o1.trim(), o2.trim())
						})
						.ok_or_else(|| {
							InstructionParseError::Typical(
								"Conditional branching arguments require 2, comma seperated \
								 arguments!"
									.to_owned(),
							)
						})
						.and_then(|(o1, o2) : (&str, &str)| -> Result<(Register, Register), InstructionParseError> {
							Ok((o1.parse::<Register>()?, o2.parse::<Register>()?))
						})?
				},
			};

			let jump : Immediate = Immediate(0); // To be filled in by program::from_str()

			Err(InstructionParseError::BranchContextNeeded(
				Instruction::Branch {
					instruction,
					operand_1,
					operand_2,
					jump,
				},
				String::from(target),
			))
		} else if let Ok(mi) = instruction.parse::<MemoryInstruction>() {
			let mi : MemoryInstruction = mi;

			let instruction : MemoryInstruction = mi;

			let (wb_and_op, (memory_address, offset)) : (
				Register,
				(Register, RegImmed),
			) = match instruction {
				MemoryInstruction::LOAD => {
					(target.parse::<Register>()?, parse_memory_argument(arg)?)
				},

				MemoryInstruction::STORE => {
					(arg.parse::<Register>()?, parse_memory_argument(target)?)
				},
			};

			Ok(Instruction::Memory {
				instruction,
				wb_and_op,
				memory_address,
				offset,
			})
		} else if let Ok(gi) = instruction.parse::<GraphicInstruction>() {
			let gi : GraphicInstruction = gi;

			// Bad code duplication of MemoryInstruction, all the more reason to group
			// MemoryInstructuin and
			// GraphicInstruction together in the future

			let instruction : GraphicInstruction = gi;

			let (wb_and_op, (memory_address, offset)) : (
				Register,
				(Register, RegImmed),
			) = match instruction {
				GraphicInstruction::LOAD => {
					(target.parse::<Register>()?, parse_memory_argument(arg)?)
				},

				GraphicInstruction::STORE => {
					(arg.parse::<Register>()?, parse_memory_argument(target)?)
				},
			};

			Ok(Instruction::Graphic {
				instruction,
				wb_and_op,
				memory_address,
				offset,
			})
		} else if let Ok(ai) = instruction.parse::<AliasInstruction>() {
			let ai : AliasInstruction = ai;

			let instruction : AliasInstruction = ai;

			let write_back : Register = target.parse::<Register>()?;

			match instruction {
				AliasInstruction::MOVE => {
					let operand_1 : RegImmed = arg.parse::<RegImmed>()?;
					Ok(Instruction::Alias {
						instruction,
						write_back,
						operand_1,
					})
				},

				AliasInstruction::ADDR => {
					let operand_1 : RegImmed = RegImmed::Immed(Immediate(0));
					Err(InstructionParseError::AddrContextNeeded(
						Instruction::Alias {
							instruction,
							write_back,
							operand_1,
						},
						String::from(arg),
					))
				},
			}
		} else {
			Err(InstructionParseError::Typical(format!(
				"Invalid instruction \"{instruction}\"!"
			)))
		}
	}
}

#[derive(Debug)]
#[repr(u8)]
pub enum ArithInstruction {
	ADD,
	SUB,
	UMULT,
	MULT,
	UDIVI,
	DIVI,
	MODU,
	NOT,
	AND,
	OR,
	XOR,
	XNOR,
	RAND,
	LSL,
	LSR,
	ASR,
}

impl FromStr for ArithInstruction {
	type Err = String;

	fn from_str(s : &str) -> Result<ArithInstruction, Self::Err> {
		match s {
			"ADD" => Ok(ArithInstruction::ADD),
			"SUB" => Ok(ArithInstruction::SUB),
			"UMULT" => Ok(ArithInstruction::UMULT),
			"MULT" => Ok(ArithInstruction::MULT),
			"UDIVI" => Ok(ArithInstruction::UDIVI),
			"DIVI" => Ok(ArithInstruction::DIVI),
			"MODU" => Ok(ArithInstruction::MODU),
			"NOT" => Ok(ArithInstruction::NOT),
			"AND" => Ok(ArithInstruction::AND),
			"OR" => Ok(ArithInstruction::OR),
			"XOR" => Ok(ArithInstruction::XOR),
			"XNOR" => Ok(ArithInstruction::XNOR),
			"RAND" => Ok(ArithInstruction::RAND),
			"LSL" => Ok(ArithInstruction::LSL),
			"LSR" => Ok(ArithInstruction::LSR),
			"ASR" => Ok(ArithInstruction::ASR),
			_ => Err(String::from("Not a valid arithmatic instruction")),
		}
	}
}

#[derive(Debug)]
#[repr(u8)]
pub enum BranchInstruction {
	B,
	BEQ,
	BNEQ,
	BLT,
	BLTEQ,
	BTG,
	BGTEQ,
	BULT,
	BULTEQ,
	BUGT,
	BUGTEQ,
}

impl FromStr for BranchInstruction {
	type Err = String;

	fn from_str(s : &str) -> Result<BranchInstruction, Self::Err> {
		match s {
			"B" => Ok(BranchInstruction::B),
			"BEQ" => Ok(BranchInstruction::BEQ),
			"BNEQ" => Ok(BranchInstruction::BNEQ),
			"BLT" => Ok(BranchInstruction::BLT),
			"BLTEQ" => Ok(BranchInstruction::BLTEQ),
			"BTG" => Ok(BranchInstruction::BTG),
			"BGTEQ" => Ok(BranchInstruction::BGTEQ),
			"BULT" => Ok(BranchInstruction::BULT),
			"BULTEQ" => Ok(BranchInstruction::BULTEQ),
			"BUGT" => Ok(BranchInstruction::BUGT),
			"BUGTEQ" => Ok(BranchInstruction::BUGTEQ),
			_ => Err(String::from("Not a valid branching instruction")),
		}
	}
}

// TODO: At a later time, it might make more sense to describe graphic
// instructions with this enum as well and adjust the Instruction Memory variant
// to accomodate. This may also require changing the APU itself to accomodate
#[derive(Debug)]
#[repr(u8)]
pub enum MemoryInstruction {
	LOAD,
	STORE,
}

impl FromStr for MemoryInstruction {
	type Err = String;

	fn from_str(s : &str) -> Result<MemoryInstruction, Self::Err> {
		match s {
			"LOAD" => Ok(MemoryInstruction::LOAD),
			"STORE" => Ok(MemoryInstruction::STORE),
			_ => Err(String::from("Not a valid memory instruction")),
		}
	}
}

#[derive(Debug)]
#[repr(u8)]
pub enum GraphicInstruction {
	LOAD,
	STORE,
}

impl FromStr for GraphicInstruction {
	type Err = String;

	fn from_str(s : &str) -> Result<GraphicInstruction, Self::Err> {
		match s {
			"GRAPHIC-LOAD" => Ok(GraphicInstruction::LOAD),
			"GRAPHIC-STORE" => Ok(GraphicInstruction::STORE),
			_ => Err(String::from("Not a valid graphic instruction")),
		}
	}
}

#[derive(Debug)]
#[repr(u8)]
pub enum AliasInstruction {
	MOVE,
	ADDR,
}

impl FromStr for AliasInstruction {
	type Err = String;

	fn from_str(s : &str) -> Result<AliasInstruction, Self::Err> {
		match s {
			"MOVE" => Ok(AliasInstruction::MOVE),
			"ADDR" => Ok(AliasInstruction::ADDR),
			_ => Err(String::from("Not a valid graphic instruction")),
		}
	}
}

// An enum for describing a register as an opperand
#[derive(Default, Debug)]
#[repr(u8)]
pub enum Register {
	X0,
	X1,
	X2,
	X3,
	X4,
	X5,
	X6,
	X7,
	X8,
	X9,
	X10,
	X11,
	X12,
	X13,
	X14,
	#[default]
	XZERO,
}

impl FromStr for Register {
	type Err = String;

	fn from_str(s : &str) -> Result<Register, Self::Err> {
		match s {
			s if s.starts_with("x") => {
				Err(format!(
					"Register {s} must start with a capital x! That is the BAD-APU way!",
				))
			},

			s if !s.starts_with("X") => {
				Err(format!("Register {s} must begin with an \"X\"!"))
			},

			"X0" => Ok(Register::X0),
			"X1" => Ok(Register::X1),
			"X2" => Ok(Register::X2),
			"X3" => Ok(Register::X3),
			"X4" => Ok(Register::X4),
			"X5" => Ok(Register::X5),
			"X6" => Ok(Register::X6),
			"X7" => Ok(Register::X7),
			"X8" => Ok(Register::X8),
			"X9" => Ok(Register::X9),
			"X10" => Ok(Register::X10),
			"X11" => Ok(Register::X11),
			"X12" => Ok(Register::X12),
			"X13" => Ok(Register::X13),
			"X14" => Ok(Register::X14),
			"XZERO" | "X15" => Ok(Register::XZERO),

			_ => Err(format!("Invalid reigster \"{s}\"")),
		}
	}
}

// An enum for describing arguments to instructions that can take in a value
// from either an inline immediate or a register. These are the second argument
// to the arithmatic instructions and the offset ammount in the memory (both
// normal and graphic) instructions

#[derive(Debug)]
pub enum RegImmed {
	Reg(Register),
	Immed(Immediate),
}

impl Default for RegImmed {
	fn default() -> RegImmed { RegImmed::Immed(Immediate(0)) }
}

impl FromStr for RegImmed {
	type Err = String;

	fn from_str(s : &str) -> Result<RegImmed, Self::Err> {
		s.parse::<Register>()
			.map(|r : Register| -> RegImmed { RegImmed::Reg(r) })
			.or_else(|re : String| -> Result<RegImmed, String> {
				s.parse::<Immediate>()
					.map(|i : Immediate| -> RegImmed { RegImmed::Immed(i) })
					.map_err(|ie : String| -> String {
						format!(
							"Failed to parse Register or Immediate because \"{s}\" cannot \
							 be a register because \"{re}\" and cannot be an immediate \
							 because \"{ie}\"!"
						)
					})
			})
	}
}

// Immediates carry a signed 16 bit value that's used in arithmatic operations,
// memory offsets and even data section content.
#[derive(Default, Debug)]
pub struct Immediate(pub i16);

impl FromStr for Immediate {
	type Err = String;

	// Parse immediates. Examples include 10, -746, 0B011010001, -0B01011, 0Xab
	// -0XAB
	fn from_str(mut s : &str) -> Result<Immediate, Self::Err> {
		// TODO: Enforce that all letters are uppercase, that is the BAD-APU way!

		let multiplier : i16 = if s.starts_with('-') {
			s = &s[1..];
			-1
		} else {
			1
		};

		let base : u32 = if s.len() < 2 {
			10
		} else {
			match &s[0..2] {
				"0x" => {
					return Err(format!(
						"Hexadecimal immediate {s} must begin with an uppercase \"0X\"! \
						 That is the BAD-APU way!"
					));
				},

				"0b" => {
					return Err(format!(
						"Binary immediate {s} must begin with an uppercase \"0B\"! That \
						 is the BAD-APU way!"
					));
				},

				"0B" => {
					s = &s[2..];
					2
				},
				"0X" => {
					s = &s[2..];
					16
				},
				_ => 10,
			}
		};

		let num : i16 = u16::from_str_radix(s, base)
			.map_err(|e : std::num::ParseIntError| -> String { e.to_string() })?
			.cast_signed()
			.mul(multiplier);

		Ok(Immediate(num))
	}
}

// Theres no real reason for [Register, RegImmed] pairings to have their own
// type, maybe I'll come back and do that later. Consider this the parsing
// (FromStr) for that hypothetical type
fn parse_memory_argument(arg : &str) -> Result<(Register, RegImmed), String> {
	arg
		.strip_prefix('[')
		.ok_or_else(|| -> String {
			format!(
				"Memory argument {arg} is not valid! Memory arguments must begin with \
				 a \"[\""
			)
		})?
		.strip_suffix(']')
		.ok_or_else(|| -> String {
			format!(
				"Memory argument {arg} is not valid! Memory arguments must end with a \
				 \"]\""
			)
		})?
		.split_once(',')
		.ok_or_else(|| -> String {
			format!(
				"Memory argument {arg} is not valid! Memory arguments must contain 2, \
				 comma seperated values!"
			)
		})
		.and_then(
			|(add, off) : (&str, &str)| -> Result<(Register, RegImmed), String> {
				Ok((
					add
						.trim()
						.parse::<Register>()?,
					off
						.trim()
						.parse::<RegImmed>()?,
				))
			},
		)
}
