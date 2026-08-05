//! Reinhardt Project Management CLI for server
//!
//! This is the project-specific management command interface (equivalent to Django's manage.py).
//!
//! This binary is intentionally native-only. The whole module body is gated
//! behind `not(target_arch = "wasm32")` so that
//! `cargo check --target wasm32-unknown-unknown` on the workspace does not
//! try to compile a tokio-based CLI for the browser target. The wasm side
//! still requires a `main` symbol for `bin` crate-types, so we keep an
//! empty stub.
//!
//! ## Router Registration
//!
//! URL patterns are automatically registered by the framework.
//! No manual registration is required - see `src/config/urls.rs` for the
//! `#[routes]` attribute macro that enables this.

#[cfg(not(target_arch = "wasm32"))]
mod native {
    // Force-link the parent library so its `#[routes]` / `#[model]`
    // `inventory::submit!` registrations survive dead-code elimination.
    // Referencing `get_settings` alone does not guarantee the whole crate
    // (and thus every inventory entry) is linked.
    use reinhardt_commands::execute_from_command_line_with_settings;
    use server as _;
    use server::config::settings::get_settings;
    use std::process;

    #[tokio::main]
    pub(super) async fn main() {
        // Set settings module environment variable
        // SAFETY: Called at program start before any spawned tasks.
        unsafe {
            std::env::set_var("REINHARDT_SETTINGS_MODULE", "server.config.settings");
        }

        // Hand the project's composed settings to the management runtime.
        // Router registration happens automatically via the #[routes]
        // attribute macro in src/config/urls.rs.
        if let Err(e) = execute_from_command_line_with_settings(get_settings()).await {
            eprintln!("Error: {}", e);
            process::exit(1);
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn main() {
    native::main();
}

#[cfg(target_arch = "wasm32")]
fn main() {}
