Introduction
=============

`kdri` is a library that lets you control Kettler devices via
bluetooth. This includes following models:

    - TOUR
    - RACER
    - ERGO
    - RECUMBENT
    - UNIX
    - SKYLON
    - RUN
    - TRACK

This library is tested on a `RUN 7`. Because there are no platform independent
bluetooth libraries for Rust (yet), it only runs on BlueZ/Linux.


Example
---------------

```rust
extern crate kdri;

use std::io::Write;
use std::str::FromStr;

fn main() {
  println!("Search for devices...");

  // Scan for all Kettler devices in range. The first argument is the timeout. This function will block for some seconds.
  let mut devices = kdri::scan_devices(1).expect("Scanning  devices failed");

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
      None value means, that the value isn't initialized (yet). This will happen for example if you try "get_rpm()" on a treadmill.
      Values like speed and incline are in multiplied by 10. "get_speed() = Some(105)" means "10.5km/h". */
      "pulse" => { println!("pulse: {:?}", connection.get_pulse());},
      "speed" => { println!("speed: {:?}", connection.get_speed());},
      "incline" => { println!("incline: {:?}", connection.get_incline());},
      _ => {

        // parse commands like "speed=100" or "incline=20"
        let substring: Vec<_> = input.split('=').collect();
        if substring.len() != 2 { println!("Input not recognized"); continue; }
        let number: i32 = match FromStr::from_str(substring[1]) { Ok(n) => n, Err(_) => { println!("Expected number after '='"); continue; } };

        match substring[0] {
          "online" => connection.set_online(number != 0), // "online=0" stops the device (tested on treadmill)
          "speed" => connection.set_speed(number as u16), // values are divided by 10 -> "50" means "5km/h"
          "incline" => connection.set_incline(number as u16),
          _ => println!("Unknown value '{}'", substring[0]),
        }
      }

    }
  }

  // close connection
  connection.close();
}
```

Bindings for C
---------------
See [kdri-c-wrapper](https://github.com/ChangSpivey/kdri-c-wrapper).
