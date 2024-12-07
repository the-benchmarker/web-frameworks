import vweb

struct App {
	vweb.Context
}

fn main() {
	vweb.run(&App{}, 3000)
}

@['/']
fn (mut app App) index() vweb.Result {
	return app.text('')
}

@['/user/:id']
fn (mut app App) get_user_id(id string) vweb.Result {
	return app.text(id)
}

@['/user'; post]
fn (mut app App) post_user() vweb.Result {
	return app.text('')
}
