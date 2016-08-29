extern crate kdri;

use std::io::Write;
use std::str::FromStr;
use kdri::FromPrimitive;

fn main() {
    println!("Search for devices...");

    // Scan for all Kettler devices in range. This function will block for some seconds.
    let mut devices = kdri::scan_devices().expect("Scanning  devices failed");

    // Ignore all other devices, just use the last one.
    let device: kdri::KettlerDevice = devices.pop().expect("No devices found");

    // Connect to Kettler device. This function blocks for about 1-2 seconds.
    println!("Connect to \"{}\" (\"{}\")...", device.get_name(), device.get_addr().to_string());
    let mut connection = device.connect().expect("Connecting to Kettler device failed");
    println!("Connected!");

    loop {
        // shell-like IO
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        input = input.trim().to_string();

        match input.as_str() {
            "exit" => { break },

            /* Fetching values via "KettlerConnection.get_*()". The return value is a Option<u8>, Option<u16> or Option<SomeSpecificEnum>. A
               None value means, that the value isn't initialized (yet). This happens for example if you try "get_rpm()" on a treadmill.
               Values like speed and incline are in multiplied by 10. "get_speed() = Some(105)" means "10.5km/h". */
            "update_interval" => { println!("update_interval: {:?}", connection.get_update_interval());},
            "device_state"    => { println!("device_state: {:?}", connection.get_device_state());},
            "brake_mode"      => { println!("brake_mode: {:?}", connection.get_brake_mode());},
            "pulse"           => { println!("pulse: {:?}", connection.get_pulse());},
            "power_target"    => { println!("power_target: {:?}", connection.get_power_target());},
            "power"           => { println!("power: {:?}", connection.get_power());},
            "power_min"       => { println!("power_min: {:?}", connection.get_power_min());},
            "power_max"       => { println!("power_max: {:?}", connection.get_power_max());},
            "rpm"             => { println!("rpm: {:?}", connection.get_rpm());},
            "distance"        => { println!("distance: {:?}", connection.get_distance());},
            "energy"          => { println!("energy: {:?}", connection.get_energy());},
            "time"            => { println!("time: {:?}", connection.get_time());},
            "time_mode"       => { println!("time_mode: {:?}", connection.get_time_mode());},
            "device_name"     => { println!("device_name: {:?}", connection.get_device_name());},
            "device_type"     => { println!("device_type: {:?}", connection.get_device_type());},
            "device_id"       => { println!("device_id: {:?}", connection.get_device_id());},
            "power_range"     => { println!("power_range: {:?}", connection.get_power_range());},
            "brake_level"     => { println!("brake_level: {:?}", connection.get_brake_level());},
            "brake_level_min" => { println!("brake_level_min: {:?}", connection.get_brake_level_min());},
            "brake_level_max" => { println!("brake_level_max: {:?}", connection.get_brake_level_max());},
            "incline_target"  => { println!("incline_target: {:?}", connection.get_incline_target());},
            "incline"         => { println!("incline: {:?}", connection.get_incline());},
            "incline_min"     => { println!("incline_min: {:?}", connection.get_incline_min());},
            "incline_max"     => { println!("incline_max: {:?}", connection.get_incline_max());},
            "speed_target"    => { println!("speed_target: {:?}", connection.get_speed_target());},
            "speed"           => { println!("speed: {:?}", connection.get_speed());},
            "speed_min"       => { println!("speed_min: {:?}", connection.get_speed_min());},
            "speed_max"       => { println!("speed_max: {:?}", connection.get_speed_max());},
            "online"          => { println!("online: {:?}", connection.get_online());},
            _ => {

                // parse commands like "speed=100" or "incline=20"
                let substring: Vec<_> = input.split('=').collect();
                if substring.len() != 2 { println!("Input not recognized"); continue; }
                let number: i64  = match FromStr::from_str(substring[1]) { Ok(n) => n, Err(_) => { println!("Expected number after '='"); continue; } };

                match substring[0] {
                    "power" => connection.set_power(number as u16), // values are divided by 10 -> "50" means "5km/h"
                    "speed" => connection.set_speed(number as u16), // values are divided by 10 -> "50" means "5km/h"
                    "incline" => connection.set_incline(number as u16),
                    "brake_level" => connection.set_brake_level(number as u8),
                    "brake_mode" => connection.set_brake_mode(kdri::KettlerBrakeMode::from_i64(number)
                                                .unwrap_or_else(|| { println!("specified brake mode does not exist - default to ConstantBrake"); kdri::KettlerBrakeMode::ConstantBrake } )),
                    "online" => connection.set_online(number != 0), // "online=0" stops the device (tested on treadmill)
                    "update_interval" => connection.set_update_interval(number as u32),
                    _ => println!("Unknown value '{}'", substring[0]),
                }
            }

        }
    }

    // close connection
    connection.close().expect("Closing connection to Kettler device failed");
}
