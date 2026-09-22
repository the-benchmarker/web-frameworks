use actix_web::{web, App, HttpRequest, HttpResponse, HttpServer};
use std::sync::Arc;
use zenocore::{parser::parse_string, Context, Node, Scope, SlotMeta};

#[derive(Clone, Debug)]
enum CompiledBody {
    Static(&'static [u8]),
    DynamicParam,
}

#[derive(Clone, Debug)]
struct CompiledRouteHandler {
    status: actix_web::http::StatusCode,
    content_type: &'static str,
    body: CompiledBody,
}

impl CompiledRouteHandler {
    #[inline(always)]
    fn handle(&self, req: &HttpRequest) -> HttpResponse {
        let mut builder = HttpResponse::build(self.status);
        builder.content_type(self.content_type);

        match &self.body {
            CompiledBody::Static(b) => builder.body(*b),
            CompiledBody::DynamicParam => {
                let path = req.path();
                let param = match path.rfind('/') {
                    Some(idx) => &path[idx + 1..],
                    None => path,
                };
                builder.body(param.as_bytes().to_vec())
            }
        }
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

fn compile_ast_to_handler(handler_node: &Node) -> CompiledRouteHandler {
    let mut status_code = actix_web::http::StatusCode::OK;
    let mut content_type: &'static str = "text/plain";
    let mut body = CompiledBody::Static(b"");

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
                            if let Ok(code) = actix_web::http::StatusCode::from_u16(st) {
                                status_code = code;
                            }
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
                            let leaked: &'static [u8] = Box::leak(clean_val.to_string().into_boxed_str()).as_bytes();
                            body = CompiledBody::Static(leaked);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    CompiledRouteHandler {
        status: status_code,
        content_type,
        body,
    }
}

fn convert_to_actix_path(path: &str) -> String {
    // Actix web uses /{id} format for route parameters
    path.to_string()
}

#[derive(Clone)]
struct AppRouteDef {
    method: String,
    path: String,
    handler: CompiledRouteHandler,
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
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

    // Compile into native Actix route definitions
    let mut app_routes: Vec<AppRouteDef> = Vec::new();
    for (method, path, node) in routes.lock().unwrap().drain(..) {
        let actix_path = convert_to_actix_path(&path);
        let handler = compile_ast_to_handler(&node);
        println!("📌 Mounted directly to Actix Router: {} {}", method, actix_path);
        app_routes.push(AppRouteDef {
            method,
            path: actix_path,
            handler,
        });
    }

    let shared_routes = Arc::new(app_routes);

    println!("🚀 zeno-rs-actix (direct native routing) running on http://0.0.0.0:3000");

    HttpServer::new(move || {
        let mut app = App::new();
        for r in shared_routes.iter() {
            let handler = r.handler.clone();
            match r.method.as_str() {
                "GET" => {
                    app = app.route(
                        &r.path,
                        web::get().to(move |req: HttpRequest| {
                            let h = handler.clone();
                            async move { h.handle(&req) }
                        }),
                    );
                }
                "POST" => {
                    app = app.route(
                        &r.path,
                        web::post().to(move |req: HttpRequest| {
                            let h = handler.clone();
                            async move { h.handle(&req) }
                        }),
                    );
                }
                _ => {}
            }
        }
        app
    })
    .bind(("0.0.0.0", 3000))?
    .run()
    .await
}
