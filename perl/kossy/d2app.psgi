use v5.38;

use Kossy;

get '/' => sub ($self, $c) {
    $c->halt_text(200, '');
};

get '/user/:id' => sub ($self, $c) {
    $c->halt_text(200, $c->args->{'id'});
};

post '/user' => sub ($self, $c) {
    $c->halt_text(200, '');
};

__PACKAGE__->psgi;
