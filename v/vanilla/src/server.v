// This module implements a basic HTTP server using epoll for handling multiple client connections efficiently.
// The server is designed to be non-blocking and uses multiple threads to handle incoming requests concurrently.
//
// Performance Considerations:
// - Non-blocking I/O: The server uses non-blocking sockets to ensure that it can handle multiple connections without being blocked by any single connection.
// - Epoll: The use of epoll allows the server to efficiently monitor multiple file descriptors to see if I/O is possible on any of them.
// - Threading: The server spawns multiple threads to handle client requests, which can improve performance on multi-core systems.
// - Memory Management: Care is taken to allocate and free memory appropriately to avoid memory leaks and ensure efficient memory usage.
// - Error Handling: The server includes error handling to manage and log errors without crashing, ensuring robustness and reliability.
// - SO_REUSEPORT: The server sets the SO_REUSEPORT socket option to allow multiple sockets on the same host and port, which can improve performance in certain scenarios.
// - Connection Handling: The server efficiently handles client connections, including accepting new connections, reading requests, and sending responses.
// - Mutex Locking: The server uses mutex locks to manage access to shared resources, ensuring thread safety while minimizing contention.
module main

import sync

const tiny_bad_request_response = 'HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()

#include <arpa/inet.h>
#include <fcntl.h>
#include <sys/epoll.h>
#include <errno.h>
#include <stdio.h>
#include <netinet/in.h>

fn C.socket(__domain int, __type int, __protocol int) int

fn C.bind(sockfd int, addr &Addr, addrlen u32) int

fn C.send(__fd int, __buf voidptr, __n usize, __flags int) int

fn C.recv(__fd int, __buf voidptr, __n usize, __flags int) int

fn C.setsockopt(__fd int, __level int, __optname int, __optval voidptr, __optlen u32) int

fn C.listen(__fd int, __n int) int

struct In_addr {
	s_addr int
}

enum In_port_t as u16 {
	ipproto_hopopts  = 0
	ipproto_routing  = 43
	ipproto_fragment = 44
	ipproto_icmpv6   = 58
	ipproto_none     = 59
	ipproto_dstopts  = 60
	ipproto_mh       = 135
}

struct Sockaddr_in {
	sin_family u16
	sin_port   u16
	sin_addr   In_addr
	sin_zero   [8]u8
}

fn C.htons(__hostshort u16) u16

@[typedef]
union C.epoll_data {
mut:
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

pub struct C.epoll_event {
	events u32
	data   C.epoll_data
}

fn C.epoll_create1(__flags int) int

fn C.epoll_ctl(__epfd int, __op int, __fd int, __event &C.epoll_event) int

fn C.epoll_wait(__epfd int, __events &C.epoll_event, __maxevents int, __timeout int) int

struct Server {
mut:
	server_socket int
	epoll_fd      int
	lock_flag     sync.Mutex
	has_clients   int
	threads       [16]thread
	router        Router
}

fn C.fcntl(fd int, cmd int, arg int) int
fn C.perror(s &u8) voidptr
fn C.close(fd int) int
fn C.accept(sockfd int, address &C.sockaddr_in, addrlen &u32) int

const sock_stream = C.SOCK_STREAM
const sock_nonblock = C.SOCK_NONBLOCK

const max_unix_path = 108

@[_pack: '1']
pub struct Ip6 {
	port      u16
	flow_info u32
	addr      [16]u8
	scope_id  u32
}

@[_pack: '1']
pub struct Ip {
	port    u16
	addr    [4]u8
	sin_pad [8]u8
}

pub struct Unix {
	path [max_unix_path]u8
}

union AddrData {
	Unix
	Ip
	Ip6
}

@[_pack: '1']
pub struct Addr {
pub:
	f    u16
	addr AddrData
}

fn set_blocking(fd int, blocking bool) {
	flags := C.fcntl(fd, 3, 0)
	if flags == -1 {
		return
	}
	if blocking {
		C.fcntl(fd, 4, flags & ~2048)
	} else {
		C.fcntl(fd, 4, flags | 2048)
	}
}

fn create_server_socket(port int) int {
	server_fd := C.socket(2, sock_stream | sock_nonblock, 0)
	if server_fd < 0 {
		eprintln(@LOCATION)
		C.perror('Socket creation failed'.str)
		return -1
	}

	// Enable SO_REUSEPORT
	opt := 1
	if C.setsockopt(server_fd, C.SOL_SOCKET, C.SO_REUSEPORT, &opt, sizeof(opt)) < 0 {
		eprintln(@LOCATION)
		C.perror('setsockopt SO_REUSEPORT failed'.str)
		C.close(server_fd)
		return -1
	}

	server_addr := Sockaddr_in{
		sin_family: 2
		sin_port:   C.htons(port)
		sin_addr:   In_addr{C.INADDR_ANY}
		sin_zero:   [8]u8{}
	}

	if C.bind(server_fd, voidptr(&server_addr), sizeof(server_addr)) < 0 {
		eprintln(@LOCATION)
		C.perror('Bind failed'.str)
		C.close(server_fd)
		return -1
	}
	if C.listen(server_fd, 512) < 0 {
		eprintln(@LOCATION)
		C.perror('Listen failed'.str)
		C.close(server_fd)
		return -1
	}
	return server_fd
}

fn add_fd_to_epoll(epoll_fd int, fd int, events u32) int {
	mut ev := C.epoll_event{}
	ev.events = events
	ev.data.fd = fd
	if C.epoll_ctl(epoll_fd, 1, fd, &ev) == -1 {
		C.perror(c'epoll_ctl: fd')
		return -1
	}
	return 0
}

fn remove_fd_from_epoll(epoll_fd int, fd int) {
	C.epoll_ctl(epoll_fd, 2, fd, C.NULL)
}

fn handle_accept(server &Server) {
	for {
		client_fd := C.accept(server.server_socket, C.NULL, C.NULL)
		if client_fd < 0 {
			if C.errno == 11 || C.errno == 11 {
				break
			}
			C.perror(c'Accept failed')
			return
		}
		set_blocking(client_fd, false)
		unsafe {
			server.lock_flag.lock()
			if add_fd_to_epoll(server.epoll_fd, client_fd, u32(C.EPOLLIN | C.EPOLLET)) == -1 {
				C.close(client_fd)
			}
			server.lock_flag.unlock()
		}
	}
}

fn handle_client_closure(server &Server, client_fd int) {
	unsafe {
		server.lock_flag.lock()
		remove_fd_from_epoll(client_fd, client_fd)
		server.lock_flag.unlock()
	}
}

fn process_events(server &Server) {
	events := [512]C.epoll_event{}
	num_events := C.epoll_wait(server.epoll_fd, &events[0], 512, -1)
	for i := 0; i < num_events; i++ {
		if events[i].events & (C.EPOLLHUP | C.EPOLLERR) != 0 {
			handle_client_closure(server, unsafe { events[i].data.fd })
			continue
		}
		if events[i].events & C.EPOLLIN != 0 {
			request_buffer := unsafe { C.malloc(140) }
			if request_buffer == C.NULL {
				C.perror('malloc failed'.str)
				continue
			}
			unsafe { C.memset(request_buffer, 0, 140) }
			bytes_read := C.recv(unsafe { events[i].data.fd }, request_buffer, 140 - 1,
				0)
			if bytes_read > 0 {
				mut readed_request_buffer := []u8{cap: bytes_read}

				unsafe {
					readed_request_buffer.push_many(request_buffer, bytes_read)
				}

				decoded_http_request := decode_http_request(readed_request_buffer) or {
					eprintln('Error decoding request ${err}')
					C.send(unsafe { events[i].data.fd }, tiny_bad_request_response.data,
						tiny_bad_request_response.len, 0)
					handle_client_closure(server, unsafe { events[i].data.fd })
					continue
				}

				response_buffer := (*server).router.handle_request(decoded_http_request) or {
					eprintln('Error handling request ${err}')
					C.send(unsafe { events[i].data.fd }, tiny_bad_request_response.data,
						tiny_bad_request_response.len, 0)
					handle_client_closure(server, unsafe { events[i].data.fd })
					continue
				}

				C.send(unsafe { events[i].data.fd }, response_buffer.data, response_buffer.len,
					0)
				handle_client_closure(server, unsafe { events[i].data.fd })

				C.free(request_buffer)
			} else if bytes_read == 0 || (bytes_read < 0 && C.errno != 11 && C.errno != 11) {
				handle_client_closure(server, unsafe { events[i].data.fd })
			}
		}
	}
}

fn worker_thread(server &Server) {
	for {
		process_events(server)
	}
	return
}

fn event_loop(server &Server) {
	for {
		handle_accept(server)
	}
}
