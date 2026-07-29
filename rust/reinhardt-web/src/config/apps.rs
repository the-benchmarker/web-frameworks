use reinhardt::installed_apps;

installed_apps! {
    benchmark: "benchmark",
}

pub fn get_installed_apps() -> Vec<String> {
    InstalledApp::all_apps()
}
