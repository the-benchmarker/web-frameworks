module main

import nedpals.vex.server
import nedpals.vex.ctx

fn main() {
    mut s := server.new()
    s.get('/', fn (req ctx.Req, res mut ctx.Resp) {
        res.send_file('')
    })
    
    s.get('/public/*path', fn (req ctx.Req, res mut ctx.Resp) {
        res.send_file('public/' + req.params['path'], 200)
    })

    s.get('/user/:id', fn (req ctx.Req, res mut ctx.Resp) {
        res.send(req.params['id'])
    })

    s.post('/user/:name', fn (req ctx.Req, res mut ctx.Resp) {
        res.send('')
    })

    s.serve(3000)
}