module main

@[heap]
struct RadixNode {
mut:
	children   map[string]&RadixNode
	handler    fn (params map[string]string) ![]u8 = unsafe { nil } // evita o uso de opcional
	is_param   bool
	param_name string
}

// Radix Trie router with parameterized route support
struct Router {
mut:
	root   RadixNode
	params map[string]string // reuso de mapa para evitar nova alocação em cada requisição
}

// Adds a route to the Radix Trie with support for parameters
fn (mut router Router) add_route(method string, path string, handler fn (params map[string]string) ![]u8) {
	segments := path.split('/').filter(it.len > 0)
	mut node := &router.root

	for segment in segments {
		is_param := segment.starts_with(':')
		segment_key := if is_param { ':' } else { segment }
		if segment_key !in node.children {
			node.children[segment_key] = &RadixNode{
				children:   map[string]&RadixNode{}
				is_param:   is_param
				param_name: if is_param { segment[1..] } else { '' }
			}
		}
		node = node.children[segment_key] or { panic('Unexpected radix trie error') }
	}
	node.handler = handler
}

// Finds and executes the handler for a given route
fn (mut router Router) handle_request(req HttpRequest) ![]u8 {
	path := req.buffer[req.path.start..req.path.start + req.path.len]
	segments := path.bytestr().split('/').filter(it.len > 0)
	mut node := &router.root
	// router.params.clear()
	router.params = map[string]string{}

	for segment in segments {
		mut matched := false

		for key, child in node.children {
			if child.is_param {
				router.params[child.param_name] = segment
				node = child
				matched = true
				break
			} else if key == segment {
				node = child
				matched = true
				break
			}
		}

		if !matched {
			dump(req.buffer.bytestr())
			return error('Route not matched. segment: ${segment}, path ${path.bytestr()}')
		}
	}

	if node.handler == unsafe { nil } {
		dump(req.buffer.bytestr())
		return error('Route not have handler for path ${path.bytestr()}')
	}
	handler := node.handler

	return handler(router.params)!
}

// Initialize the router and add the routes
fn setup_router() Router {
	mut router := Router{
		root: RadixNode{
			children: map[string]&RadixNode{}
		}
	}

	// Adding routes with handler functions
	router.add_route('GET', '/', home_controller)
	router.add_route('GET', '/user', get_users_controller)
	router.add_route('GET', '/user/:id', get_user_controller)
	router.add_route('POST', '/user', create_user_controller)

	return router
}
