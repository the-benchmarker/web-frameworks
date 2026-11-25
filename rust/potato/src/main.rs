// potato 参赛实现：The Benchmarker web-frameworks v2 六路由契约
//
// 契约规范见仓库根 SCENARIO.md，验收用例见 .spec/v2/*.feature：
//   GET  /                 空响应体（HTTP 200）
//   GET  /user/{id}        恰 8 位 ASCII 数字 ID，原样回显 {"id":"..."}，非法 400
//   POST /user             空响应体（HTTP 200）
//   GET  /serialization    ?seed= 8 位小写字母，返回 100 个 {"seed":"..."} 的数组
//   POST /deserialization  解析 {"items":[...100 项整数 id...]}，
//                          返回 {"count":..,"checksum":..}；媒体类型不符 415
//   POST /upload           恰一个名为 file 的上传部件，
//                          返回 {"first_byte":..,"last_byte":..,"checksum":"sha256"}
//
// v2 运行时要求：纯 HTTP 监听 3000、无日志、响应不压缩（不发送 Content-Encoding），
// 故 configure 中开启 set_disable_compress。

use potato::{HttpRequest, HttpResponse};
use sha2::{Digest, Sha256};

// 本地调试可传端口参数（基准容器内无参数，固定 3000）
fn port() -> String {
    std::env::args().nth(1).unwrap_or_else(|| "3000".into())
}

// ---------- 1. 静态路由 / 3. 方法分发 ----------

#[potato::http_get("/")]
fn index() -> HttpResponse {
    HttpResponse::empty()
}

#[potato::http_post("/user")]
fn create_user() -> HttpResponse {
    HttpResponse::empty()
}

// ---------- 2. 动态路由 ----------

/// ID 合法性：解码后恰 8 个 ASCII 数字（保留前导零，按字符串处理）
#[potato::http_get("/user/{id}")]
fn get_user(id: String) -> HttpResponse {
    if id.len() == 8 && id.bytes().all(|b| b.is_ascii_digit()) {
        HttpResponse::json(format!(r#"{{"id":"{id}"}}"#))
    } else {
        HttpResponse::bad_request("invalid user id")
    }
}

// ---------- 4. JSON 序列化 ----------

/// 查询参数段解码：%xx 十六进制转义 + '+' 转空格（查询串语义）
fn decode_query_component(raw: &str) -> String {
    let bytes = raw.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'+' => {
                out.push(b' ');
                i += 1;
            }
            b'%' if i + 2 < bytes.len() => {
                let hi = (bytes[i + 1] as char).to_digit(16);
                let lo = (bytes[i + 2] as char).to_digit(16);
                if let (Some(hi), Some(lo)) = (hi, lo) {
                    out.push((hi << 4 | lo) as u8);
                    i += 3;
                } else {
                    out.push(b'%');
                    i += 1;
                }
            }
            b => {
                out.push(b);
                i += 1;
            }
        }
    }
    String::from_utf8_lossy(&out).into_owned()
}

/// seed 合法性：恰 8 个小写 ASCII 字母
fn is_valid_seed(s: &str) -> bool {
    s.len() == 8 && s.bytes().all(|b| b.is_ascii_lowercase())
}

#[potato::http_get("/serialization")]
fn serialization(req: &mut HttpRequest) -> HttpResponse {
    // 框架的 url_query 已按参数名折叠（%xx 转义后同名参数会合并），
    // 重复参数名检测必须基于原始查询串手工解析
    let Some(raw) = req.raw_query() else {
        return HttpResponse::bad_request("missing query");
    };
    let mut seed: Option<String> = None;
    for pair in raw.split('&') {
        let (name, value) = pair.split_once('=').unwrap_or((pair, ""));
        if decode_query_component(name) == "seed" {
            if seed.is_some() {
                // 重复的 seed 参数（含 %73eed 形式）直接拒绝
                return HttpResponse::bad_request("repeated seed");
            }
            seed = Some(decode_query_component(value));
        } else {
            return HttpResponse::bad_request("unexpected query parameter");
        }
    }
    let Some(seed) = seed.filter(|s| is_valid_seed(s)) else {
        return HttpResponse::bad_request("invalid seed");
    };
    // 每请求构建 100 个对象的数组
    let mut body = String::with_capacity(18 * 100 + 1);
    body.push('[');
    for _ in 0..100 {
        body.push_str(r#"{"seed":""#);
        body.push_str(&seed);
        body.push_str(r#""},"#);
    }
    body.pop();
    body.push(']');
    HttpResponse::json(body)
}

// ---------- 5. JSON 反序列化 ----------

/// JSON 媒体类型白名单：application/json、+json 结构化后缀别名、
/// text/x-json、application/jsonrequest（charset 等参数忽略）
fn is_json_media_type(content_type: &str) -> bool {
    let media = content_type.split(';').next().unwrap_or("").trim();
    let media = media.to_ascii_lowercase();
    media == "application/json"
        || media == "text/x-json"
        || media == "application/jsonrequest"
        || (media.starts_with("application/") && media.ends_with("+json"))
}

/// ID 合法性：整数值且落在 0..=1_000_000（2、2.0、2e0 均视为整数 2）
fn integral_id(n: &serde_json::Number) -> Option<u64> {
    let v = if let Some(v) = n.as_u64() {
        v
    } else if let Some(v) = n.as_i64() {
        if v < 0 {
            return None;
        }
        v as u64
    } else {
        let v = n.as_f64()?;
        if !(0.0..=1_000_000.0).contains(&v) || v.fract() != 0.0 {
            return None;
        }
        v as u64
    };
    (v <= 1_000_000).then_some(v)
}

#[potato::http_post("/deserialization")]
fn deserialization(req: &mut HttpRequest) -> HttpResponse {
    // 先校验媒体类型再解析：缺失/不支持一律 415，与 body 是否合法无关
    let content_type_ok = req
        .get_header("Content-Type")
        .map(is_json_media_type)
        .unwrap_or(false);
    if !content_type_ok {
        let mut res = HttpResponse::error("unsupported media type");
        res.http_code = 415;
        return res;
    }
    // 携带任何查询参数都拒绝
    if req.raw_query().is_some_and(|q| !q.is_empty()) {
        return HttpResponse::bad_request("unexpected query parameter");
    }
    // 顶层对象 + 恰 100 个 items，逐项校验数值型整数 ID
    let parsed: serde_json::Value = match serde_json::from_slice(&req.body[..]) {
        Ok(v) => v,
        Err(_) => return HttpResponse::bad_request("malformed JSON"),
    };
    let Some(items) = parsed.get("items").and_then(|v| v.as_array()) else {
        return HttpResponse::bad_request("missing items array");
    };
    if items.len() != 100 {
        return HttpResponse::bad_request("items length must be 100");
    }
    let mut checksum: u64 = 0;
    for item in items {
        let Some(id) = item
            .get("id")
            .and_then(|v| v.as_number())
            .and_then(integral_id)
        else {
            return HttpResponse::bad_request("invalid item id");
        };
        checksum += id;
    }
    HttpResponse::json(format!(
        r#"{{"count":{},"checksum":{checksum}}}"#,
        items.len()
    ))
}

// ---------- 6. multipart 上传 ----------

fn strip_quotes(v: &str) -> &str {
    v.strip_prefix('"')
        .and_then(|rest| rest.strip_suffix('"'))
        .unwrap_or(v)
}

#[potato::http_post("/upload")]
fn upload(req: &mut HttpRequest) -> HttpResponse {
    // 媒体类型检查：非 multipart 一律 415；multipart 但 boundary 缺失/为空为 400
    let unsupported = || {
        let mut res = HttpResponse::error("unsupported media type");
        res.http_code = 415;
        res
    };
    let Some(content_type) = req.get_header("Content-Type") else {
        return unsupported();
    };
    let media = content_type.split(';').next().unwrap_or("").trim();
    if !media.eq_ignore_ascii_case("multipart/form-data") {
        return unsupported();
    }
    let mut boundary = None;
    for param in content_type.split(';').skip(1) {
        if let Some((k, v)) = param.split_once('=') {
            if k.trim().eq_ignore_ascii_case("boundary") {
                let v = strip_quotes(v.trim());
                if v.is_empty() {
                    return HttpResponse::bad_request("empty boundary");
                }
                boundary = Some(v);
            }
        }
    }
    if boundary.is_none() {
        return HttpResponse::bad_request("missing boundary");
    }
    // 头残缺部件（缺 \r\n\r\n 结束符）属畸形 multipart 请求
    if req.body_malformed_multipart {
        return HttpResponse::bad_request("malformed multipart body");
    }
    // file 部件判定：
    //   - filename="" 的未选中输入不计入重复统计（框架记入 body_unselected_files）
    //   - 真实上传段重复（框架在 HashMap 折叠前记入 body_dup_files）拒绝
    //   - 与无 filename 的文本字段同名共存（"file then text"）拒绝
    let unselected = req.body_unselected_files.iter().any(|n| &n[..] == "file");
    let dup = req.body_dup_files.iter().any(|n| &n[..] == "file");
    let text_field = req.body_pairs.keys().any(|k| &k[..] == "file") && !unselected;
    if dup || text_field {
        return HttpResponse::bad_request("repeated file part");
    }
    let Some(file) = req.body_files.iter().find(|(k, _)| &k[..] == "file") else {
        return HttpResponse::bad_request("missing file part");
    };
    let bytes = &file.1.data[..];
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let mut checksum = String::with_capacity(64);
    for b in hasher.finalize() {
        checksum.push_str(&format!("{b:02x}"));
    }
    let (first, last) = match (bytes.first(), bytes.last()) {
        (Some(f), Some(l)) => (f.to_string(), l.to_string()),
        _ => ("null".to_string(), "null".to_string()),
    };
    HttpResponse::json(format!(
        r#"{{"first_byte":{first},"last_byte":{last},"checksum":"{checksum}"}}"#
    ))
}

#[tokio::main]
async fn main() {
    let mut server = potato::HttpServer::new(format!("0.0.0.0:{}", port()));
    server
        .configure(|ctx| {
            ctx.use_handlers();
            // v2 运行时要求：响应保持未压缩，不发送 Content-Encoding
            ctx.set_disable_compress(true);
        })
        .expect("configure server");
    _ = server.serve_http().await;
}
