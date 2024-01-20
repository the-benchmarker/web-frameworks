module main

import nedpals.vex.router
import nedpals.vex.server
import nedpals.vex.ctx

fn main() {
	mut app := router.new()

	app.route(.get, '/', fn (req &ctx.Req, mut res ctx.Resp) {
		res.send('', 200)
	})

	app.route(.get, '/user', fn (req &ctx.Req, mut res ctx.Resp) {
		res.send('', 200)
	})

	app.route(.get, '/user/:id', fn (req &ctx.Req, mut res ctx.Resp) {
		res.send(req.params['id'], 200)
	})

	app.route(.post, '/user', fn (req &ctx.Req, mut res ctx.Resp) {
		res.send('', 200)
	})

	server.serve(app, 3000)
}
