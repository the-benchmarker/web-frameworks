//! server library
//!
//! This is the main library crate for server.

pub mod apps;
pub mod config;

// Re-export commonly used items
#[cfg(feature = "management")]
pub use config::settings::get_settings;
pub use config::urls::routes;
