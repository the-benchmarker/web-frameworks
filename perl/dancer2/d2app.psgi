use warnings;
use strict;

{
    package Web::Frameworks::D2App;
    use Dancer2;

    get '/'      => sub { '' };
    get '/user/:id'   => sub { route_parameters->{'id'} };
    post '/user' => sub { '' };
}

Web::Frameworks::D2App->to_app;
