use v5.45;
use Mojolicious::Lite -signatures;

get '/' => sub ($c) {
    $c->render(text => '');
};

get '/user/:id' => sub ($c) {
    $c->render(text => $c->param('id'));
};

post '/user' => sub ($c) {
    $c->render(text => '');
};

app->start;
