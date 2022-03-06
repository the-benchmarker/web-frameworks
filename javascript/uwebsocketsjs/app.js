const uWS = require('uWebSockets.js');
const port = 3000;

uWS.App().get('/', (res, req) => {
    res.end('');
}).get('/user/:id', (res, req) => {
    res.end(req.getParameter(0));
}).get('/user', (res, req) => {
    res.end('');
}).listen(port, (token) => {
    if (token) {
        console.log('Listening to port ' + port);
    } else {
        console.log('Failed to listen to port ' + port);
    }
});