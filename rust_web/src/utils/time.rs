use std::time::{SystemTime, UNIX_EPOCH};

pub fn get_timestamp() -> u128 {
    let timestamp = match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(value) => value.as_millis(),
        Err(_) => panic!("Couldn't get time"),
    };
    timestamp
}
