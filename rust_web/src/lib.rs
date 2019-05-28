#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use]
extern crate rocket;
#[macro_use]
extern crate serde_derive;

mod api;

use api::resources::Resource;
use std::sync::Arc;
use std::sync::Mutex;

pub fn run() {
    rocket::ignite()
        .mount(
            "/api/resources",
            routes!(
                api::resources::get,
                api::resources::get_one,
                api::resources::create,
                api::resources::delete
            ),
        )
        .manage(Arc::new(Mutex::new(vec![Resource {
            id: 1,
            name: String::from("Hello World!"),
        }])))
        .launch();
}
