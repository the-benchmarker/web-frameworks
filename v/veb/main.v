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
	ctx.set_header(.connection, 'keep-alive')
	ctx.set_header(.content_length, '0')
	return ctx.text('')
}

@['/user/:id']
fn (mut app App) get_user_id(mut ctx Context, id string) veb.Result {
	ctx.set_header(.connection, 'keep-alive')
	ctx.set_header(.content_length, id.len.str())
	return ctx.text(id)
}

@['/user'; post]
fn (mut app App) post_user(mut ctx Context) veb.Result {
	ctx.set_header(.connection, 'keep-alive')
	ctx.set_header(.content_length, '0')
	return ctx.text('')
}
