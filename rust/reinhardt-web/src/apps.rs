//! Application registry for server
//!
//! This file maintains the list of installed apps.
//! New apps created with `startapp` will be automatically added here.
pub mod benchmark;
pub use benchmark::BenchmarkConfig;
