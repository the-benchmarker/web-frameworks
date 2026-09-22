use axum::{
    body::{Body, Bytes},
    extract::Request,
    http::header,
    response::Response,
    routing::{get, post},
    Router,
};
use std::net::SocketAddr;
use std::sync::Arc;
use zenocore::{parser::parse_string, Context, Node, Scope, SlotMeta};

/// Body representation for maximum execution speed:
/// - Static: Zero-allocation, uses static byte slice.
/// - DynamicParam: Extracts parameter directly from URL path without hashmap lookup.
#[derive(Clone, Debug)]
enum CompiledBody {
    Static(Bytes),
    DynamicParam,
}

#[derive(Clone, Debug)]
struct CompiledRouteHandler {
    status: u16,
    content_type: &'static str,
    body: CompiledBody,
}

impl CompiledRouteHandler {
    #[inline(always)]
    fn handle(&self, req: Request) -> Response {
        let body = match &self.body {
            CompiledBody::Static(b) => Body::from(b.clone()),
            CompiledBody::DynamicParam => {
                let path = req.uri().path();
                // Extract dynamic URL parameter directly from the path slice after the last '/'
                let param = match path.rfind('/') {
                    Some(idx) => &path[idx + 1..],
                    None => path,
                };
                Body::from(Bytes::copy_from_slice(param.as_bytes()))
            }
        };

        Response::builder()
            .status(self.status)
            .header(header::CONTENT_TYPE, self.content_type)
            .body(body)
            .unwrap()
    }
}

fn empty_slot_meta() -> SlotMeta {
    SlotMeta {
        description: String::new(),
        example: String::new(),
        inputs: std::collections::HashMap::new(),
        required_blocks: Vec::new(),
        value_type: String::new(),
    }
}

/// Compile AST node into a flat binary route handler ready for Axum
fn compile_ast_to_handler(handler_node: &Node) -> CompiledRouteHandler {
    let mut status = 200u16;
    let mut content_type: &'static str = "text/plain";
    let mut body = CompiledBody::Static(Bytes::new());

    for child in &handler_node.children {
        if child.name == "http.response" {
            for resp_child in &child.children {
                let raw_val = resp_child.value.as_deref().unwrap_or_default().trim();
                let clean_val = if (raw_val.starts_with('\'') && raw_val.ends_with('\''))
                    || (raw_val.starts_with('"') && raw_val.ends_with('"'))
                {
                    &raw_val[1..raw_val.len() - 1]
                } else {
                    raw_val
                };

                match resp_child.name.as_str() {
                    "status" => {
                        if let Ok(st) = clean_val.parse::<u16>() {
                            status = st;
                        }
                    }
                    "type" => {
                        content_type = match clean_val {
                            "application/json" => "application/json",
                            _ => "text/plain",
                        };
                    }
                    "body" => {
                        if clean_val.starts_with('$') {
                            body = CompiledBody::DynamicParam;
                        } else {
                            body = CompiledBody::Static(Bytes::copy_from_slice(clean_val.as_bytes()));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    CompiledRouteHandler {
        status,
        content_type,
        body,
    }
}

fn convert_to_axum_path(path: &str) -> String {
    // Axum 0.8 uses /{id} format for route parameters
    if path.contains('*') && !path.contains("{*") {
        path.replace('*', "{*wildcard}")
    } else {
        path.to_string()
    }
}

#[tokio::main]
async fn main() {
    let engine = zenoengine::new_engine();

    let routes = std::sync::Arc::new(std::sync::Mutex::new(Vec::<(String, String, Node)>::new()));

    let r_get = routes.clone();
    engine.register(
        "http.get",
        Arc::new(move |_, _, node, _| {
            let raw = node.value.clone().unwrap_or_default().trim().to_string();
            let clean = if raw.starts_with('\'') || raw.starts_with('"') {
                raw[1..raw.len() - 1].to_string()
            } else {
                raw
            };
            r_get.lock().unwrap().push(("GET".to_string(), clean, node.clone()));
            Ok(())
        }),
        empty_slot_meta(),
    );

    let r_post = routes.clone();
    engine.register(
        "http.post",
        Arc::new(move |_, _, node, _| {
            let raw = node.value.clone().unwrap_or_default().trim().to_string();
            let clean = if raw.starts_with('\'') || raw.starts_with('"') {
                raw[1..raw.len() - 1].to_string()
            } else {
                raw
            };
            r_post.lock().unwrap().push(("POST".to_string(), clean, node.clone()));
            Ok(())
        }),
        empty_slot_meta(),
    );

    // Load & parse app.zl
    let zl_content = std::fs::read_to_string("app.zl")
        .unwrap_or_else(|_| include_str!("../app.zl").to_string());
    let main_node = parse_string(&zl_content, "app.zl").expect("Failed to parse app.zl");

    let parent_scope = Scope::new(None);
    let mut init_ctx = Context::new();
    let _ = engine.execute(&mut init_ctx, &main_node, &parent_scope);

    // Mount compiled routes directly to native Axum Router
    let mut app = Router::new();

    for (method, path, node) in routes.lock().unwrap().drain(..) {
        let axum_path = convert_to_axum_path(&path);
        let compiled_handler = compile_ast_to_handler(&node);
        println!("📌 Mounted directly to Axum Router: {} {}", method, axum_path);

        match method.as_str() {
            "GET" => {
                app = app.route(
                    &axum_path,
                    get(move |req: Request| async move { compiled_handler.handle(req) }),
                );
            }
            "POST" => {
                app = app.route(
                    &axum_path,
                    post(move |req: Request| async move { compiled_handler.handle(req) }),
                );
            }
            _ => {}
        }
    }

    let addr = SocketAddr::from(([0, 0, 0, 0], 3000));
    println!("🚀 zeno-rs-axum (direct native routing) running on http://{}", addr);

    let listener = tokio::net::TcpListener::bind(addr).await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

