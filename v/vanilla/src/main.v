module main

const port = 3000

fn main() {
	mut server := Server{
		router: setup_router()
	}

	server.server_socket = create_server_socket(port)
	if server.server_socket < 0 {
		return
	}
	server.epoll_fd = C.epoll_create1(0)
	if server.epoll_fd < 0 {
		C.perror('epoll_create1 failed'.str)
		C.close(server.server_socket)
		return
	}

	server.lock_flag.lock()
	if add_fd_to_epoll(server.epoll_fd, server.server_socket, u32(C.EPOLLIN)) == -1 {
		C.close(server.server_socket)
		C.close(server.epoll_fd)

		server.lock_flag.unlock()
		return
	}

	server.lock_flag.unlock()

	server.lock_flag.init()
	for i := 0; i < 16; i++ {
		server.threads[i] = spawn worker_thread(&server)
	}
	println('listening on http://localhost:${port}/')
	event_loop(&server)
}
