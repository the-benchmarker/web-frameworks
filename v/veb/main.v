import veb
import os
import log

// App represents the veb benchmark application
pub struct App {}

// Context represents the HTTP request context for veb
pub struct Context {
	veb.Context
}

fn main() {
	// Configure logging
	log.set_format(log.Format{
		timestamp: true
		level:     true
		message:   true
	})
	log.set_level(.debug)

	// Get port from environment or use default
	port := int(os.getenv('PORT') or { '3000' })

	log.info('Starting veb benchmark server on port ${port}')

	mut app := &App{}
	veb.run[App, Context](mut app, port)
}

// index handles the root endpoint
// GET / -> returns empty string with 200 OK
@['/']
fn (mut app App) index(mut ctx Context) veb.Result {
	log.debug('Root endpoint accessed')
	ctx.set_header(.connection, 'keep-alive')
	ctx.set_header(.content_length, '0')
	return ctx.text('')
}

// get_user_id handles GET /user/:id requests
// Returns the user ID as plain text with 200 OK
// 
// Args:
//   ctx: HTTP context
//   id: User identifier from URL parameter
// 
// Returns:
//   veb.Result with user ID as response body
@['/user/:id']
fn (mut app App) get_user_id(mut ctx Context, id string) veb.Result {
	log.debug('User endpoint accessed with ID: ${id}')
	ctx.set_header(.connection, 'keep-alive')
	ctx.set_header(.content_length, id.len.str())
	return ctx.text(id)
}

// post_user handles POST /user requests
// Creates a new user and returns empty string with 201 Created
// 
// Args:
//   ctx: HTTP context
// 
// Returns:
//   veb.Result with empty response body
@['/user'; post]
fn (mut app App) post_user(mut ctx Context) veb.Result {
	log.debug('Create user endpoint accessed')
	ctx.res.set_status(.created)
	ctx.set_header(.connection, 'keep-alive')
	ctx.set_header(.content_length, '0')
	return ctx.text('')
}

// health_check handles GET /health requests
// Returns "OK" with 200 OK for health monitoring
// 
// Args:
//   ctx: HTTP context
// 
// Returns:
//   veb.Result with "OK" response body
@['/health']
fn (mut app App) health_check(mut ctx Context) veb.Result {
	ctx.set_header(.connection, 'keep-alive')
	ctx.set_header(.content_length, '2')
	return ctx.text('OK')
}
