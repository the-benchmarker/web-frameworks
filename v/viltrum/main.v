module main

import viltrum

fn main() {
	mut app := viltrum.new()
	app.get('/', fn (mut c viltrum.Ctx) {
		c.no_content(200)
	})
	app.get('/user/:id', fn (mut c viltrum.Ctx) {
		c.text(200, c.param('id') or { '' })
	})
	app.post('/user', fn (mut c viltrum.Ctx) {
		c.no_content(200)
	})
	app.listen('0.0.0.0:3000') or { panic(err) }
}
