module main

import watchmen123456.valval

fn index(req valval.Request) valval.Response {
	return valval.response_ok('')
}

fn user(req valval.Request) valval.Response {
	if req.method == 'POST' {
		return valval.response_ok('')
	}
	id := req.get('id', '-1')
	return valval.response_ok(id)
}

fn main() {
	mut app := valval.new_app(true)
	app.route('/', index)
	app.route('/user', user)
	valval.runserver(app, 3000)
}

