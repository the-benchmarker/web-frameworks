import veb

pub struct App {}

pub struct Context {
	veb.Context
}

fn main() {
	mut app := &App{}
	veb.run[App, Context](mut app, 3000)
}

@['/']
fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text('')
}

@['/user/:id']
fn (mut app App) get_user_id(mut ctx Context, id string) veb.Result {
	return ctx.text(id)
}

@['/user'; post]
fn (mut app App) post_user(mut ctx Context) veb.Result {
	return ctx.text('')
}
