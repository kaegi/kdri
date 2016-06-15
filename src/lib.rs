mod ffi; use ffi::*;
mod crc; use crc::*;

extern crate bluetooth;
extern crate mio;

#[macro_use] extern crate enum_primitive;

pub use bluetooth::BtAddr;
use bluetooth::{BtDevice, BtSocket, BtProtocol, BtError};
use std::io::{Read, Write};
use std::thread::JoinHandle;
use mio::*;
use enum_primitive::FromPrimitive;
use std::sync::mpsc;

macro_rules! try_msg {
	($e:expr, $m:expr) => {
		try!($e.map_err(|err| format!("{}: {:?}", $m, err)))
	}
}

fn bytes_to_string(b: &[u8]) -> String {
	b.iter().map(|b| format!("{:02X}", b)).collect::<Vec<_>>().join(":")
}

fn from_u8<T: FromPrimitive>(i: u8) -> std::result::Result<T, u8> {
	match T::from_u8(i) {
		Some(x) => Ok(x),
		None => Err(i),
	}
}

fn from_u16<T: FromPrimitive>(i: u16) -> std::result::Result<T, u16> {
	match T::from_u16(i) {
		Some(x) => Ok(x),
		None => Err(i),
	}
}

enum_from_primitive! {
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum KettlerDeviceType {
	Bike = 1,
	Crosstrainer = 2,
	Racer = 3,
	Rowing = 4,
	Treadmill = 5,
}
}

enum_from_primitive! {
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum KettlerDeviceState {
	Up = 0,
	Down = 1,
}
}

enum_from_primitive! {
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum KettlerBrakeMode {
	ConstantPower = 0,
	ConstantBrake = 1,
}
}

enum_from_primitive! {
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum KettlerPowerRange {
	Below = 0,
	In = 1,
	Above = 2,
}
}

enum_from_primitive! {
/*
   Values that can be read or set on Kettler device.
*/
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum KettlerValue {
	DeviceState = 6,
	BrakeMode = 7,

	Pulse = 8,
	Rpm = 9,

	PowerSet = 10,
	PowerGet = 11,
	PowerMin = 12,
	PowerMax = 13,

	Speed = 14,
	Distance = 15,
	Energy = 16,
	Time = 17,
	TimeMode = 18,

	DeviceName = 20,

	DeviceType = 24,
	DeviceId = 25,

	InPowerRange = 26,

	BrakeLevel = 27,
	BrakeLevelMin = 28,
	BrakeLevelMax = 19,

	InclineSet = 30,
	InclineGet = 31,
	InclineMin = 32,
	InclineMax = 33,

	SpeedSet = 34,
	SpeedGet = 35,
	SpeedMin = 36,
	SpeedMax = 37,

	Online = 38,
}
}

enum_from_primitive!{
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum KettlerInstruction {
    Read = 1,
    Write = 2,
    Answer = 3,
}
}

#[derive(Default, Clone, Debug)]
struct KettlerDeviceData {
	power:           Option<u16>,
	power_min:       Option<u16>,
	power_max:       Option<u16>,
	speed:           Option<u16>,
	speed_min:       Option<u16>,
	speed_max:       Option<u16>,
	incline:         Option<u16>,
	incline_min:     Option<u16>,
	incline_max:     Option<u16>,
	brake_level:     Option<u8>,
	brake_level_min: Option<u8>,
	brake_level_max: Option<u8>,
	online:          Option<bool>,
	pulse:           Option<u16>,
	rpm:             Option<u16>,
	distance:        Option<u16>,
	energy:          Option<u16>,
	time:            Option<u16>, // TODO: verify data type
	time_mode:       Option<u16>, // TODO: verify data type
	device_name:     Option<String>,
	device_id:       Option<String>,
	power_range:     Option<KettlerPowerRange>,
	device_type:     Option<KettlerDeviceType>,
	device_state:    Option<KettlerDeviceState>,
	brake_mode:      Option<KettlerBrakeMode>,
}

impl KettlerDeviceData {
	fn opt_to_string<T: std::fmt::Debug>(o: Option<T>) -> std::borrow::Cow<'static, str> {
		match o {
			Some(x) => std::borrow::Cow::Owned(format!("{:?}", x)),
			None => std::borrow::Cow::Borrowed("-"),
		}
	}
}

impl KettlerDeviceData {
	fn is_value_initialized(&self, value: KettlerValue) -> bool {
		use KettlerValue::*;
		match value {
			DeviceName      => self.device_name.is_some(),
			DeviceId        => self.device_id.is_some(),
			DeviceType      => self.device_type.is_some(),
			BrakeMode 		=> self.brake_mode.is_some(),
			DeviceState     => self.device_state.is_some(),
			InPowerRange    => self.power_range.is_some(),
			Pulse           => self.pulse.is_some(),
			Rpm             => self.rpm.is_some(),
			Online          => self.online.is_some(),
			SpeedGet        => self.speed.is_some(),
			SpeedMin        => self.speed_min.is_some(),
			SpeedMax        => self.speed_max.is_some(),
			InclineGet      => self.incline.is_some(),
			InclineMin      => self.incline_min.is_some(),
			InclineMax      => self.incline_max.is_some(),
			PowerGet        => self.power.is_some(),
			PowerMin        => self.power_min.is_some(),
			PowerMax        => self.power_max.is_some(),
			Distance        => self.distance.is_some(),
			Energy          => self.energy.is_some(),
			Time            => self.time.is_some(),
			TimeMode        => self.time_mode.is_some(),
			BrakeLevel      => self.brake_level.is_some(),
			BrakeLevelMin   => self.brake_level_min.is_some(),
			BrakeLevelMax   => self.brake_level_max.is_some(),
			Speed /* deprecated */ | SpeedSet | PowerSet | InclineSet => { true }
		}
	}
}

impl std::fmt::Display for KettlerDeviceData {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "speed: {}\nspeed_max: {}\n" , Self::opt_to_string(self.speed), Self::opt_to_string(self.speed_max))
    }
}

enum KettlerPackageAnalyzerResult {
	Nothing,
	Clear,
	Package(Vec<u8>),
}

struct KettlerPackageAnalyzer {
	package_data: Vec<u8>,
	unescape_next: bool,
	in_crc_bytes: bool,
	num_crc_bytes: u32,
	num_data_bytes: u32,
	is_first_byte: bool,
}

impl KettlerPackageAnalyzer {
	fn new() -> Self {
		KettlerPackageAnalyzer {
			package_data: Vec::new(),
			unescape_next: false,
			in_crc_bytes: false,
			num_crc_bytes: 0,
			num_data_bytes: 0,
			is_first_byte: true,
		}
	}

	fn error(&mut self, byte: u8) -> KettlerPackageAnalyzerResult {
		println!("E: unexpected character '0x{:02X}'", byte);
		self.reset();
		return KettlerPackageAnalyzerResult::Clear;
	}

	fn reset(&mut self) {
		self.package_data.clear();
		self.in_crc_bytes = false;
		self.unescape_next = false;
		self.num_crc_bytes = 0;
		self.num_data_bytes = 0;
		self.is_first_byte = true;
	}

	fn push_byte(&mut self, byte: u8) -> KettlerPackageAnalyzerResult {
		self.package_data.push(byte);

		if self.in_crc_bytes { self.num_crc_bytes += 1; }
		else                 { self.num_data_bytes += 1; }

		if self.num_crc_bytes == 2 {
			let vec = self.package_data.clone();
			self.reset();
			return KettlerPackageAnalyzerResult::Package(vec);
		}

		KettlerPackageAnalyzerResult::Nothing
	}

	fn next_byte(&mut self, byte: u8) -> KettlerPackageAnalyzerResult {
		if self.is_first_byte {
			self.is_first_byte = false;
			if byte != 0x02 { return self.error(byte); }
			return KettlerPackageAnalyzerResult::Nothing;
		}

		match (self.unescape_next, byte) {
			(false, 0x02)							   => { return self.error(byte); }
			(false, 0x03)							   => {
				if self.in_crc_bytes { return self.error(byte); }
				self.in_crc_bytes = true;
			}
			(false, 0x10)                              => { self.unescape_next = true; }
			(false, byte)                              => { return self.push_byte(byte); }
			(true, 0x22) | (true, 0x23) | (true, 0x30) => { self.unescape_next = false; return self.push_byte(byte ^ 0x20);}
			(true, byte)                               => { return self.error(byte); }
		}


		return KettlerPackageAnalyzerResult::Nothing;
	}
}

struct KettlerValues {
	dynamic: Vec<KettlerValue>,
	static_values: Vec<KettlerValue>,
}

struct KettlerDataManager {
    // TODO: implement VecDeque where possible
    write_channel: Vec<u8>,
    read_buffer: Vec<u8>,
    read_channel: Vec<u8>,
	read_channel_cursor: usize,
	crc: CRC,
	kdata: KettlerDeviceData,
	package_analyzer: KettlerPackageAnalyzer,
}

impl KettlerDataManager {
	fn new() -> KettlerDataManager {
		KettlerDataManager {
            read_buffer: std::vec::from_elem(0xAE, 2048),
            read_channel: Vec::new(),
			read_channel_cursor: 0,
			write_channel: Vec::new(),
			crc: CRC::new(),
			kdata: KettlerDeviceData::default(),
			package_analyzer: KettlerPackageAnalyzer::new(),
		}
	}


	fn process_package(&mut self, package: &[u8]) {
		let data = &package[0..package.len() - 2];
		let crc = &package[package.len() - 2..package.len()];
		let crc2 = Self::u16_to_2u8(self.crc.calc16(data));
		if crc[0] != crc2[0] || crc[1] != crc2[1] { println!("E: wrong CRC received:{} vs computed:{} in package:{}", bytes_to_string(crc), bytes_to_string(&crc2), bytes_to_string(package)); return; }

		self.process_instruction(data);
	}

	fn process_instruction(&mut self, b: &[u8]) {
		if b.len() < 5 { println!("E: instruction has not enough bytes {}", b.len()); return; }
		let value_int =  Self::from_2u8_to_u16(&b[0..2]);
		let instruction_int = b[2];
		let additional_data_length = Self::from_2u8_to_u16(&b[3..5]);
		let additional_data = &b[5..];

		if b.len() != additional_data_length as usize + 5 { println!("E: invalid package - additional data length is wrong"); return; }

		let value = match KettlerValue::from_u16(value_int) {
			Some(x) => x,
			None => { println!("E: unsupported value integer {}", value_int); return }
		};

		let instruction = match KettlerInstruction::from_u8(instruction_int) {
			Some(x) => x,
			None => { println!("E: unsupported instruction {}", value_int); return }
		};

		if instruction != KettlerInstruction::Answer { println!("W: expected KettlerInstruction::Answer"); return; }

		use KettlerValue::*;
		match value {
			DeviceName      => { if let Some(i) = Self::parse_string(additional_data)                { self.kdata.device_name     = Some(i); } }
			DeviceId        => { if let Some(i) = Self::parse_string(additional_data)                { self.kdata.device_id       = Some(i); } }
			DeviceType      => { if let Some(i) = Self::parse_device_type(additional_data)           { self.kdata.device_type     = Some(i); } }
			BrakeMode 		=> { if let Some(i) = Self::parse_device_brake_mode(additional_data)     { self.kdata.brake_mode      = Some(i); } }
			DeviceState     => { if let Some(i) = Self::parse_device_state(additional_data)          { self.kdata.device_state    = Some(i); } }
			InPowerRange    => { if let Some(i) = Self::parse_power_range(additional_data)           { self.kdata.power_range     = Some(i); } }
			Pulse           => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.pulse           = Some(i); } }
			Rpm             => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.rpm             = Some(i); } }
			Online          => { if let Some(i) = Self::parse_bool_u8(additional_data)               { self.kdata.online          = Some(i); } }
			SpeedGet        => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.speed           = Some(i); } }
			SpeedMin        => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.speed_min       = Some(i); } }
			SpeedMax        => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.speed_max       = Some(i); } }
			InclineGet      => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.incline         = Some(i); } }
			InclineMin      => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.incline_min     = Some(i); } }
			InclineMax      => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.incline_max     = Some(i); } }
			PowerGet        => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.power           = Some(i); } }
			PowerMin        => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.power_min       = Some(i); } }
			PowerMax        => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.power_max       = Some(i); } }
			Distance        => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.distance        = Some(i); } }
			Energy          => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.energy          = Some(i); } }
			Time            => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.time            = Some(i); } }
			TimeMode        => { if let Some(i) = Self::parse_u16(additional_data)                   { self.kdata.time_mode       = Some(i); } }
			BrakeLevel      => { if let Some(i) = Self::parse_u8(additional_data)                    { self.kdata.brake_level     = Some(i); } }
			BrakeLevelMin   => { if let Some(i) = Self::parse_u8(additional_data)                    { self.kdata.brake_level_min = Some(i); } }
			BrakeLevelMax   => { if let Some(i) = Self::parse_u8(additional_data)                    { self.kdata.brake_level_max = Some(i); } }
			Speed /* deprecated */ | SpeedSet | PowerSet | InclineSet => { println!("W: unexpected value {:?} received", value); }
		}
	}

    fn process_read_channel(&mut self) {
		//println!("Bytes received {:?}", Self::bytes_to_string(&self.read_channel));
		// ensure that the byte sequence starts with an 0x02 or is empty
		//println!("read channel {}", bytes_to_string(&self.read_channel));
		while self.read_channel_cursor < self.read_channel.len() {
			self.read_channel_cursor += 1;
			match self.package_analyzer.next_byte(self.read_channel[self.read_channel_cursor - 1]) {
				KettlerPackageAnalyzerResult::Nothing => { }
				KettlerPackageAnalyzerResult::Clear => {
					println!("W: drain package {}", bytes_to_string(&self.read_channel.drain(0..self.read_channel_cursor).collect::<Vec<u8>>()));
					self.read_channel_cursor = 0;
				}
				KettlerPackageAnalyzerResult::Package(package) => {
					self.read_channel.drain(0..self.read_channel_cursor);
					self.read_channel_cursor = 0;
					self.process_package(&package);
				}
			}
		}
    }

	fn parse_bool_u8(data: &[u8]) -> Option<bool> {
		if data.len() != 1 { println!("E: wrong additional data length ({} bytes instead of 1 bytes)", data.len()); return None; }
		Some(data[0] != 0)
	}

	fn parse_u8(data: &[u8]) -> Option<u8> {
		if data.len() != 1 { println!("E: wrong additional data length ({} bytes instead of 1 bytes)", data.len()); return None; }
		Some(data[0])
	}

	fn parse_u16(data: &[u8]) -> Option<u16> {
		if data.len() != 2 { println!("E: wrong additional data length ({} bytes instead of 2 bytes)", data.len()); return None; }
		Some(Self::from_2u8_to_u16(data))
	}

	fn parse_string(data: &[u8]) -> Option<String> {
		if data.len() == 0 { println!("E: wrong additional data length ({} bytes instead of >0 bytes)", data.len()); return None; }
		Some(String::from_utf8_lossy(data).into_owned())
	}

	fn parse_enum_u8<T: FromPrimitive>(data: &[u8], error: &'static str) -> Option<T> {
		if let Some(x) = Self::parse_u8(data) {
			match from_u8::<T>(x) {
				Ok(d) => return Some(d),
				Err(i) => println!("E: {} {} (={})", error, i as char, i),
			}
		}
		None
	}

	fn parse_enum_u16<T: FromPrimitive>(data: &[u8], error: &'static str) -> Option<T> {
		if let Some(x) = Self::parse_u16(data) {
			match from_u16::<T>(x) {
				Ok(d) => return Some(d),
				Err(i) => println!("E: {} {}", error, i),
			}
		}
		None
	}

	fn parse_char_to_device_type(c: char) -> Option<KettlerDeviceType> {
		use KettlerDeviceType::*;
		return match c as char {
			'B' => Some(Bike),
			'X' => Some(Crosstrainer),
			'T' => Some(Treadmill),
			'R' => Some(Rowing),
			'S' => Some(Racer),
			_ => { println!("E: unknown device type {}", c as u8); None }
		}
	}

	fn parse_device_type(data: &[u8]) -> Option<KettlerDeviceType> {
		if let Some(b) = Self::parse_u8(data) {
			return Self::parse_char_to_device_type(b as char);
		}
		None
	}

	fn parse_power_range(data: &[u8]) -> Option<KettlerPowerRange> {
		Self::parse_enum_u8::<KettlerPowerRange>(data, "unknown power range")
	}

	fn parse_device_state(data: &[u8]) -> Option<KettlerDeviceState> {
		Self::parse_enum_u16::<KettlerDeviceState>(data, "unknown device state")
	}

	fn parse_device_brake_mode(data: &[u8]) -> Option<KettlerBrakeMode> {
		Self::parse_enum_u16::<KettlerBrakeMode>(data, "unknown device brake mode")
	}

	/*
		Returns true if this byte has to be escaped when sending it to Kettler device.
		See send_data() for more detailed explanation.
	*/
    fn is_special_byte(b: u8) -> bool {
        let special_bytes = vec![0x02u8, 0x03, 0x10];
        special_bytes.contains(&b)
    }

	/*
		Split u16 into u8 with little endian order.
	*/
    fn u16_to_2u8(v: u16) -> [u8; 2] {
        [(v >> 8) as u8, (v >> 0) as u8]
    }
    fn from_2u8_to_u16(b: &[u8]) -> u16 {
		(b[0] as u16) << 8 | b[1] as u16
    }

	/*
		Since 0x02, 0x03 mark the beginning/end of an instruction, these bytes are
		not allowed as "normal" bytes. Therefore they are escaped with 0x10.
		Following replacements are happening:
			0x02 -> 0x10 0x22
			0x03 -> 0x10 0x23
			0x10 -> 0x10 0x30
		This function escapes these "special bytes", adds the start and end byte and
		attaches the CRC16-Checksum. The resulting byte array is then send to the
		blutooth-io-thread.
	*/
    fn send_data(&mut self, bytes: &[u8]) {
        let num_special_bytes = bytes.iter().fold(0, |acc, &b| acc + if Self::is_special_byte(b) { 1 } else { 0 });
        let new_length = bytes.len() + num_special_bytes + 4;
        let mut new_data = Vec::<u8>::with_capacity(new_length);
        new_data.push(0x02);
        for &b in bytes {
            if Self::is_special_byte(b) {
                new_data.push(0x10);
                new_data.push(b ^ 0x20);
            } else {
                new_data.push(b);
            }
        }
        new_data.push(0x03);
        let crc_bytes = Self::u16_to_2u8(self.crc.calc16(bytes));
        new_data.push(crc_bytes[0]);
        new_data.push(crc_bytes[1]);
		self.write_channel.append(&mut new_data);
    }


	/*
		Creates data with a specific layout and sends them to the blutooth device.

		Data Layout:
			2 byte: value to be read/written
			1 byte: instruction ("read"/"write")
			2 byte: additional data length "n"
			n byte: additional data (for example the targeted speed)
	*/
    fn send_instruction(&mut self, value: KettlerValue, instruction: KettlerInstruction, additional_data: &[u8]) {
        let value_bytes = Self::u16_to_2u8(value as u16);
        let add_data_length_bytes = Self::u16_to_2u8(additional_data.len() as u16);
        let mut data = Vec::<u8>::with_capacity(5 + additional_data.len());
        data.push(value_bytes[0]);
        data.push(value_bytes[1]);
        data.push(instruction as u8);
        data.push(add_data_length_bytes[0]);
        data.push(add_data_length_bytes[1]);
        data.extend_from_slice(additional_data);
        self.send_data(&data);
    }

	/* Send instruction with 0 byte additional data */
    fn send_instruction_u0(&mut self, value: KettlerValue, instruction: KettlerInstruction) {
        self.send_instruction(value, instruction, &[]);
    }

	/* Send instruction with 1 byte additional data */
    fn send_instruction_u8(&mut self, value: KettlerValue, instruction: KettlerInstruction, additional_data: u8) {
        self.send_instruction(value, instruction, &[additional_data]);
    }

	/* Send instruction with 2 byte additional data */
    fn send_instruction_u16(&mut self, value: KettlerValue, instruction: KettlerInstruction, additional_data: u16) {
        self.send_instruction(value, instruction, &Self::u16_to_2u8(additional_data));
    }
}



// KettlerConnection -> KettlerHandler/EventLoop
enum KettlerHandlerMsg {
	SendData(Vec<u8>),
	// SendInstruction(KettlerValue, KettlerInstruction, Vec<u8>),
	// SendInstruction0(KettlerValue, KettlerInstruction),
	SendInstruction8(KettlerValue, KettlerInstruction, u8),
	SendInstruction16(KettlerValue, KettlerInstruction, u16),
	SetUpdateInterval(/* in milliseconds */ u32),
	KdataRequest,
    Shutdown,
}

// KettlerHandler/EventLoop -> KettlerConnection
enum ConnectionMsg {
	Kdata(KettlerDeviceData),
}

struct KettlerHandler {
    socket: BtSocket,
	send_channel: mpsc::Sender<ConnectionMsg>,
	update_interval: u32,
	kvalues_opt: Option<KettlerValues>,
	data_manager: KettlerDataManager,
}

impl KettlerHandler {
    fn new(socket: BtSocket, send_channel: mpsc::Sender<ConnectionMsg>, update_interval: u32) -> KettlerHandler {
        KettlerHandler {
            socket: socket,
			kvalues_opt: None,
			send_channel: send_channel,
			update_interval: update_interval,
			data_manager: KettlerDataManager::new(),
        }
    }

    fn update_registration(&self, event_loop: &mut EventLoop<Self>) {
		let mut event_set = EventSet::readable();
		if self.data_manager.write_channel.len() > 0 { event_set = event_set | EventSet::writable(); }
		event_loop.reregister(&self.socket, Token(1), event_set, PollOpt::edge() | PollOpt::oneshot()).expect("Registering read event failed");
    }




	/* Get array of dynamic and static kettler values by device type */
	fn initialize_kvalues(&mut self) {
		let mut ret = KettlerValues {
			dynamic: Vec::new(),
			static_values: Vec::new(),
		};

		use KettlerValue::*;
		use KettlerDeviceType::*;

		ret.dynamic.append(&mut vec![Pulse, Distance, Energy, Time, TimeMode, Online]);
		ret.static_values.append(&mut vec![DeviceName, DeviceId, DeviceType]);

		let device_type = self.data_manager.kdata.device_type.expect("device type is not initialized");
		let (mut d, mut s) = match device_type {
			Treadmill => (
				vec![SpeedGet, InclineGet],
				vec![SpeedMin, SpeedMax, InclineMin, InclineMax]
			),
			// TODO: different profiles for crosstrainer, racer, ....
			_ => {
				println!("E: using generic profile for Kettler device {:?} in module {} file {} line {}", device_type, module_path!(), file!(), line!());
				(
					vec![DeviceState, PowerGet, SpeedGet, InclineGet, InPowerRange, BrakeLevel],
					vec![SpeedMin, SpeedMax, InclineMin, InclineMax, PowerMin, PowerMax, BrakeLevelMin, BrakeLevelMax]
				)
			}
		};

		ret.dynamic.append(&mut d);
		ret.static_values.append(&mut s);

		self.kvalues_opt = Some(ret);
	}

	fn process_kvalues(&mut self) {
		let kvalues = self.kvalues_opt.as_ref().expect("kvalues not initialized");
		for &value in &kvalues.dynamic {
			self.data_manager.send_instruction_u0(value, KettlerInstruction::Read);
		}
		for &value in &kvalues.static_values {
			if self.data_manager.kdata.is_value_initialized(value) { continue }
			self.data_manager.send_instruction_u0(value, KettlerInstruction::Read);
		}
	}
}

impl mio::Handler for KettlerHandler {
    type Timeout = ();
    type Message = KettlerHandlerMsg;

    fn ready(&mut self, event_loop: &mut EventLoop<Self>, _: Token, events: EventSet) {
        if events.is_readable() {
            match self.socket.read(&mut self.data_manager.read_buffer) {
                Ok(num_bytes) => {
                    self.data_manager.read_channel.extend_from_slice(&self.data_manager.read_buffer[0..num_bytes]);
                    self.data_manager.process_read_channel();
                }
                Err(error) => { println!("E: bluetooth read error: {}", error); }
            }

        }
        if events.is_writable() {
            match self.socket.write(&self.data_manager.write_channel) {
                Ok(bytes_written) => {
                    self.data_manager.write_channel.drain(0..bytes_written);
                    self.update_registration(event_loop);
                }
                Err(error) => { println!("E: bluetooth write error: {}", error); }
            }
        }
    }


    fn notify(&mut self, event_loop: &mut EventLoop<Self>, msg: Self::Message) {
        match msg {
            KettlerHandlerMsg::SendData(data) => { self.data_manager.send_data(data.as_slice()); }
            // KettlerHandlerMsg::SendInstruction(value, instruction, additional_data) => { self.send_instruction(value, instruction, additional_data.as_slice()); }
            // KettlerHandlerMsg::SendInstruction0(value, instruction) => { self.send_instruction_u0(value, instruction); }
            KettlerHandlerMsg::SendInstruction8(value, instruction, additional_data) => { self.data_manager.send_instruction_u8(value, instruction, additional_data); }
            KettlerHandlerMsg::SendInstruction16(value, instruction, additional_data) => { self.data_manager.send_instruction_u16(value, instruction, additional_data); }
			KettlerHandlerMsg::KdataRequest => { self.send_channel.send(ConnectionMsg::Kdata(self.data_manager.kdata.clone())).expect("sending kdata failed"); }
			KettlerHandlerMsg::SetUpdateInterval(ms) =>  { self.update_interval = ms; }
            KettlerHandlerMsg::Shutdown => { event_loop.shutdown() }
        }
		self.update_registration(event_loop);
    }

    fn timeout(&mut self, event_loop: &mut EventLoop<Self>, _: Self::Timeout) {
		if self.data_manager.kdata.device_type.is_none() {
			self.data_manager.send_instruction_u0(KettlerValue::DeviceType, KettlerInstruction::Read);
		} else {
			// device type is known -> initialize value array
			if self.kvalues_opt.is_none() {
				self.initialize_kvalues();
			}
			self.process_kvalues();
		}

		self.update_registration(event_loop);
		event_loop.timeout_ms((), self.update_interval as u64).expect("Registering timer failed");
	}
}


pub struct KettlerConnection {
    send_channel: mio::Sender<KettlerHandlerMsg>,
	recv_channel: mpsc::Receiver<ConnectionMsg>,
	kdata: KettlerDeviceData,
	join_handle: Option<JoinHandle<()>>,
}
impl KettlerConnection {

    fn new(socket: BtSocket) -> KettlerConnection {
        // start blocking event loop in different thread but retain a channel for communication
        let mut event_loop = EventLoop::<_>::new().expect("EventLoop::new() failed");
        let send_channel = event_loop.channel();
		let (tx, recv_channel): (mpsc::Sender<ConnectionMsg>, mpsc::Receiver<ConnectionMsg>) = mpsc::channel();
        let join_handle = std::thread::spawn(move || {
			event_loop.timeout_ms((), 10).expect("Registering first timer failed");
			event_loop.register(&socket, Token(1), EventSet::readable(), PollOpt::edge() | PollOpt::oneshot()).expect("Registering read event failed");
            event_loop.run(&mut KettlerHandler::new(socket, tx, 100)).expect("EventLoop::run() failed");
        });

        let connection = KettlerConnection {
            send_channel: send_channel,
			recv_channel: recv_channel,
			kdata: Default::default(),
			join_handle: Some(join_handle),
        };

        connection
    }




    fn send_handshake(&self) {
	    //let handshake_bytes = vec![0x02, 0x00, 0x01, 0x01, 0x00, 0x12, 0x2d, 0x24, 0xf2, 0x24, 0x96, 0xa4, 0xff, 0x98, 0x29, 0xf9, 0x21, 0xbe, 0x9d, 0xaa, 0x9e, 0x4d, 0x01, 0x17, 0x03, 0x86, 0x77];
        //self.write_bytes(&handshake_bytes).unwrap();
	    let handshake_bytes = vec![0x00, 0x01, 0x01, 0x00, 0x12, 0x2d, 0x24, 0xf2, 0x24, 0x96, 0xa4, 0xff, 0x98, 0x29, 0xf9, 0x21, 0xbe, 0x9d, 0xaa, 0x9e, 0x4d, 0x01, 0x17, ];
        self.send_data(handshake_bytes);
    }

	/* Send instruction with 1 byte additional data */
    fn send_data(&self, data: Vec<u8>) {
        self.send_channel.send(KettlerHandlerMsg::SendData(data)).expect("Sending data to bluetooth socket thread failed (data)");
    }

// /* Send instruction with 1 byte additional data */
// fn send_instruction(&self, value: KettlerValue, instruction: KettlerInstruction, additional_data: Vec<u8>) {
// self.channel.send(KettlerHandlerMsg::SendInstruction(value, instruction, additional_data)).expect("Sending data to bluetooth socket thread failed (Vec<u8>)");
// }
//
// /* Send instruction with 0 byte additional data */
// fn send_instruction_u0(&self, value: KettlerValue, instruction: KettlerInstruction) {
// self.channel.send(KettlerHandlerMsg::SendInstruction0(value, instruction)).expect("Sending data to bluetooth socket thread failed (u0)");
// }

	/* Send instruction with 1 byte additional data */
    fn send_instruction_u8(&self, value: KettlerValue, instruction: KettlerInstruction, additional_data: u8) {
        self.send_channel.send(KettlerHandlerMsg::SendInstruction8(value, instruction, additional_data)).expect("Sending data to bluetooth socket thread failed (u8)");
    }

	/* Send instruction with 2 byte additional data */
    fn send_instruction_u16(&self, value: KettlerValue, instruction: KettlerInstruction, additional_data: u16) {
        self.send_channel.send(KettlerHandlerMsg::SendInstruction16(value, instruction, additional_data)).expect("Sending data to bluetooth socket thread failed (u16)");
    }

	/* Send instruction with 2 byte additional data */
    fn send_message(&self, msg: KettlerHandlerMsg) {
        self.send_channel.send(msg).expect("Sending data to bluetooth socket thread failed (msg)");
    }

	fn update(&mut self) {
		self.send_message(KettlerHandlerMsg::KdataRequest);
		match self.recv_channel.recv().expect("updating/receiving kettler device data from event loop failed") {
			ConnectionMsg::Kdata(kdata) => self.kdata = kdata,
		}
	}

	/*
		Everything that has to be done before a value can be returned via "get_*()"...
	*/
	fn u(&mut self) {
		self.update();
	}

    pub fn set_power(&mut self, v: u16)	                    { self.send_instruction_u16(KettlerValue::PowerSet, KettlerInstruction::Write, v); }
    pub fn set_speed(&mut self, v: u16)		                { self.send_instruction_u16(KettlerValue::SpeedSet, KettlerInstruction::Write, v); }
    pub fn set_incline(&mut self, v: u16)	                { self.send_instruction_u16(KettlerValue::InclineSet, KettlerInstruction::Write, v); }
    pub fn set_brake_level(&mut self, v: u8)	            { self.send_instruction_u8(KettlerValue::BrakeLevel, KettlerInstruction::Write, v); }
    pub fn set_brake_mode(&mut self, v: KettlerBrakeMode)	{ self.send_instruction_u8(KettlerValue::BrakeMode, KettlerInstruction::Write, v as u8); }
    pub fn set_online(&mut self, v: bool)	                { self.send_instruction_u8(KettlerValue::Online, KettlerInstruction::Write, !v as u8); }
	pub fn set_update_interval(&mut self, v: u32)			{ self.send_message(KettlerHandlerMsg::SetUpdateInterval(v)); }

	pub fn get_power(&mut self) -> Option<u16>				                { self.u(); self.kdata.power }
	pub fn get_power_min(&mut self) -> Option<u16>		                    { self.u(); self.kdata.power_min }
	pub fn get_power_max(&mut self) -> Option<u16>		                    { self.u(); self.kdata.power_max }
	pub fn get_speed(&mut self) -> Option<u16>				                { self.u(); self.kdata.speed }
	pub fn get_speed_min(&mut self) -> Option<u16>			                { self.u(); self.kdata.speed_min }
	pub fn get_speed_max(&mut self) -> Option<u16>			                { self.u(); self.kdata.speed_max }
	pub fn get_incline(&mut self) -> Option<u16>			                { self.u(); self.kdata.incline }
	pub fn get_incline_min(&mut self) -> Option<u16>		                { self.u(); self.kdata.incline_min }
	pub fn get_incline_max(&mut self) -> Option<u16>		                { self.u(); self.kdata.incline_max }
	pub fn get_brake_level(&mut self) -> Option<u8>			                { self.u(); self.kdata.brake_level }
	pub fn get_brake_level_min(&mut self) -> Option<u8>		                { self.u(); self.kdata.brake_level_min }
	pub fn get_brake_level_max(&mut self) -> Option<u8>		                { self.u(); self.kdata.brake_level_max }
	pub fn get_online(&mut self) -> Option<bool>		                    { self.u(); self.kdata.online }
	pub fn get_pulse(&mut self) -> Option<u16>		                        { self.u(); self.kdata.pulse }
	pub fn get_rpm(&mut self) -> Option<u16>		                        { self.u(); self.kdata.rpm }
	pub fn get_distance(&mut self) -> Option<u16>		                    { self.u(); self.kdata.distance }
	pub fn get_energy(&mut self) -> Option<u16>		                        { self.u(); self.kdata.energy }
	pub fn get_time(&mut self) -> Option<u16>		                        { self.u(); self.kdata.energy }
	pub fn get_time_mode(&mut self) -> Option<u16>		                    { self.u(); self.kdata.energy }
	pub fn get_device_name(&mut self) -> Option<String>		                { self.u(); self.kdata.device_name.clone() }
	pub fn get_device_id(&mut self) -> Option<String>		                { self.u(); self.kdata.device_id.clone() }
	pub fn get_power_range(&mut self) -> Option<KettlerPowerRange>		    { self.u(); self.kdata.power_range }
	pub fn get_device_type(&mut self) -> Option<KettlerDeviceType>		    { self.u(); self.kdata.device_type }
	pub fn get_device_state(&mut self) -> Option<KettlerDeviceState>		{ self.u(); self.kdata.device_state }
	pub fn get_brake_mode(&mut self) -> Option<KettlerBrakeMode>		    { self.u(); self.kdata.brake_mode }

    pub fn close(&mut self) -> std::result::Result<(), String> {
        try_msg!(self.send_channel.send(KettlerHandlerMsg::Shutdown), "Sending shutdown signal to bluetooth socket thread failed");
		try_msg!(self.join_handle.take().unwrap().join(), "Joining threads failed");
		Ok(())
    }
}

pub struct KettlerDevice {
    name: String,
    addr: BtAddr,
}

impl KettlerDevice {
	pub fn new(name: String, addr: BtAddr) -> KettlerDevice {
		KettlerDevice {
			name: name,
			addr: addr,
		}
	}

    pub fn get_name(&self) -> String { self.name.clone() }
    pub fn get_addr(&self) -> BtAddr { self.addr }

    pub fn connect(&self) -> std::result::Result<KettlerConnection, String> {
        let mut socket = try!(BtSocket::new(BtProtocol::RFCOMM).map_err(|e| e.to_string()));

    	// Find rfcomm channel by using SDP (service discovery)
    	// Rfcomm service has class 0x1101.
        let channel = unsafe { c__get_rfcomm_channel(self.addr, 0x1101) };
        if channel < 0 { return Err("RFCOMM service of bluetooth device not found".to_string()); }
        try!(socket.connect(self.addr, channel as u32).map_err(|e| e.to_string()));

        let connection = KettlerConnection::new(socket);
        connection.send_handshake();
        Ok(connection)
    }
}

impl<'a> std::convert::From<&'a BtDevice> for KettlerDevice {

    fn from(device: &BtDevice) -> KettlerDevice {
        KettlerDevice {
            name: device.name.clone(),
            addr: device.addr,
        }
    }

}

pub fn scan_devices() -> Result<Vec<KettlerDevice>, BtError> {
	let bluetooth_devices: Vec<BtDevice> = try!(bluetooth::scan_devices());
	println!("{} devices", bluetooth_devices.len());
	let prefixes = vec!["TOUR", "RACER", "ERGO", "RECUMBENT", "UNIX", "SKYLON", "RUN", "TRACK"];
	Ok(bluetooth_devices.iter()
						.filter(|device: &&BtDevice| prefixes.iter().any(|prefix| device.name.starts_with(prefix)))
						.map(|device: &BtDevice| From::from(device))
						.collect())
}
