use axum::{
    extract::{Request, State},
    response::Response,
    routing::any,
    Router,
};
use matchit::Router as MatchitRouter;
use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};
use zenocore::{parser::parse_string, Context, Engine, Node, Scope, SlotMeta, Value};

#[derive(Clone)]
struct HttpResponseData {
    status: u16,
    content_type: String,
    body: String,
}

impl Default for HttpResponseData {
    fn default() -> Self {
        Self {
            status: 200,
            content_type: "text/plain".to_string(),
            body: String::new(),
        }
    }
}

struct MethodHandler {
    get: Option<Node>,
    post: Option<Node>,
}

#[derive(Clone)]
struct AppState {
    engine: Arc<Engine>,
    router: Arc<MatchitRouter<MethodHandler>>,
    parent_scope: Arc<Scope>,
}

fn empty_slot_meta() -> SlotMeta {
    SlotMeta {
        description: String::new(),
        example: String::new(),
        inputs: HashMap::new(),
        required_blocks: Vec::new(),
        value_type: String::new(),
    }
}

fn convert_path_to_matchit(path: &str) -> String {
    // matchit 0.8 uses {id} natively for parameters and {*path} for wildcards
    if path.contains('*') && !path.contains("{*") {
        path.replace('*', "{*wildcard}")
    } else {
        path.to_string()
    }
}

#[tokio::main]
async fn main() {
    let engine = zenoengine::new_engine();

    // Register http.response slot handler for ZenoLang
    engine.register(
        "http.response",
        Arc::new(|engine, ctx, node, scope| {
            let mut status = 200u16;
            let mut content_type = "text/plain".to_string();
            let mut body = String::new();

            for child in &node.children {
                let val = engine.resolve_shorthand_value(child, scope);
                if child.name == "status" {
                    status = val.to_int() as u16;
                } else if child.name == "type" {
                    content_type = val.to_string_coerce();
                } else if child.name == "body" {
                    body = val.to_string_coerce();
                }
            }

            if let Some(resp_store) = ctx.get::<Arc<Mutex<HttpResponseData>>>("http_response_data") {
                let mut store = resp_store.lock().unwrap();
                store.status = status;
                store.content_type = content_type;
                store.body = body;
            }
            Ok(())
        }),
        empty_slot_meta(),
    );

    // Dynamic Route Collector from app.zl
    let routes = Arc::new(Mutex::new(Vec::<(String, String, Node)>::new()));

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

    // Load & Parse app.zl (with compile-time fallback for container runtime)
    let zl_content = std::fs::read_to_string("app.zl")
        .unwrap_or_else(|_| include_str!("../app.zl").to_string());
    let main_node = parse_string(&zl_content, "app.zl").expect("Failed to parse app.zl");

    let parent_scope = Scope::new(None);
    let mut init_ctx = Context::new();
    let _ = engine.execute(&mut init_ctx, &main_node, &parent_scope);

    // Build Matchit Router
    let mut route_map: HashMap<String, MethodHandler> = HashMap::new();
    for (method, path, node) in routes.lock().unwrap().drain(..) {
        let matchit_path = convert_path_to_matchit(&path);
        println!("📌 Registered route: {} {} -> matchit: {}", method, path, matchit_path);
        let entry = route_map
            .entry(matchit_path)
            .or_insert(MethodHandler { get: None, post: None });
        if method == "GET" {
            entry.get = Some(node);
        } else if method == "POST" {
            entry.post = Some(node);
        }
    }

    let mut matchit_router = MatchitRouter::new();
    for (path, handler) in route_map {
        let _ = matchit_router.insert(&path, handler);
    }

    let state = AppState {
        engine: Arc::new(engine),
        router: Arc::new(matchit_router),
        parent_scope,
    };

    let app = Router::new()
        .fallback(any(zeno_route_handler))
        .with_state(state);

    let addr = SocketAddr::from(([0, 0, 0, 0], 3000));
    println!("🚀 zeno-rs-axum benchmark server running on http://{}", addr);

    let listener = tokio::net::TcpListener::bind(addr).await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

async fn zeno_route_handler(State(state): State<AppState>, req: Request) -> Response {
    let path = req.uri().path();
    let method = req.method().as_str();

    let matched = match state.router.at(path) {
        Ok(m) => m,
        Err(_) => return Response::builder().status(404).body(axum::body::Body::from("Not Found")).unwrap(),
    };

    let node = match method {
        "GET" => matched.value.get.as_ref(),
        "POST" => matched.value.post.as_ref(),
        _ => None,
    };

    let handler_node = match node {
        Some(n) => n,
        None => return Response::builder().status(405).body(axum::body::Body::from("Method Not Allowed")).unwrap(),
    };

    let mut ctx = Context::new();
    let req_scope = Scope::new(Some(state.parent_scope.clone()));

    // Inject URL params ($id, etc.) into ZenoLang Scope
    for (k, v) in matched.params.iter() {
        req_scope.set(k, Value::String(v.to_string()));
    }

    let resp_store = Arc::new(Mutex::new(HttpResponseData::default()));
    ctx.set("http_response_data", resp_store.clone());

    // Execute the statements inside the route block
    for child in &handler_node.children {
        let _ = state.engine.execute(&mut ctx, child, &req_scope);
    }

    let resp_data = resp_store.lock().unwrap().clone();
    Response::builder()
        .status(resp_data.status)
        .header("Content-Type", resp_data.content_type)
        .body(axum::body::Body::from(resp_data.body))
        .unwrap()
}
