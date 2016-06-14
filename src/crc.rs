
pub struct CRC([u16; 256]);

static CRC16: u16 = 0x8408;

// argument has to be 256byte table which will be filled
impl CRC {

    pub fn new() -> CRC {
        let mut crc_table: CRC = CRC([0; 256]);
        for i in 0u16..256 {
    		let mut data: u16 = i;
    		let mut crc: u16 = 0;
    		for _ in 0..8 {
    			if (data ^ crc) & 1 != 0	{ crc = (crc >> 1) ^ CRC16; }
    			else						{ crc =  crc >> 1; }
    			data >>= 1;
    		}
    		crc_table.0[i as usize] = crc;
        }
        return crc_table;
    }

    pub fn calc16(&self, bytes: &[u8]) -> u16 {

    	let mut crc = 0u16;
        for &b in bytes {
    		crc = (crc >> 8) ^ self.0[((crc as u8 ^ b) & 0xFF) as usize];
    	}

    	return crc;
    }
}
