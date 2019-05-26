"""Middleware Configuration Settings."""

"""HTTP Middleware
HTTP middleware is middleware that will be ran on every request. Middleware
is only ran when a HTTP call is successful (a 200 response). This list
should contain a simple aggregate of middleware classes.
"""

HTTP_MIDDLEWARE = []


"""Route Middleware
Specify a dictionary of middleware to be used on a per route basis here. The key will 
be the alias to use on routes and the value can be any middleware class or a list
of middleware (middleware stacks).
"""

ROUTE_MIDDLEWARE = {}
