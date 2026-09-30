<h1 class="title">Congratulations, your <a class="alert-link" href="http://luminusweb.net">Luminus</a> site is ready!</h1>

This page will help guide you through the first steps of building your site.

<p class="title is-5">Why are you seeing this page?</p>

The `home-routes` handler in the `luminus.routes.home` namespace defines the
route that invokes the `home-page` function whenever an HTTP request is made to
the `/` URI using the `GET` method.

```
(defroutes home-routes
  (GET "/" [] (home-page))
  (GET "/about" [] (about-page)))
```

The `home-page` function will in turn call the `luminus.layout/render` function
to render the HTML content:

```
(defn home-page []
  (layout/render
    "home.html" {:docs (-> "docs/docs.md" io/resource slurp)}))
```

The `render` function will render the `home.html` template found in the
`resources/templates` folder using a parameter map containing the `:docs` key.
This key points to the contents of the `resources/docs/docs.md` file containing
these instructions.

The HTML templates are written using [Selmer](https://github.com/yogthos/Selmer)
templating engine.

```
<div class="content">
  {{docs|markdown}}
</div>
```

<a class="level-item button" href="http://www.luminusweb.net/docs/html_templating.md">learn
more about HTML templating »</a>

<p class="title is-5">Organizing the routes</p>

The routes are aggregated and wrapped with middleware in the `luminus.handler`
namespace:

```
(defstate app
  :start
  (middleware/wrap-base
    (routes
      (-> #'home-routes
          (wrap-routes middleware/wrap-csrf)
          (wrap-routes middleware/wrap-formats))
      (route/not-found
        (:body
          (error-page {:status 404
                       :title "page not found"}))))))
```

The `app` definition groups all the routes in the application into a single
handler. A default route group is added to handle the `404` case.

<a class="level-item button" href="http://www.luminusweb.net/docs/routes.md">learn
more about routing »</a>

The `home-routes` are wrapped with two middleware functions. The first enables
CSRF protection. The second takes care of serializing and deserializing various
encoding formats, such as JSON.

<p class="title is-5">Managing your middleware</p>

Request middleware functions are located under the `luminus.middleware`
namespace.

This namespace is reserved for any custom middleware for the application. Some
default middleware is already defined here. The middleware is assembled in the
`wrap-base` function.

Middleware used for development is placed in the `luminus.dev-middleware`
namespace found in the `env/dev/clj/` source path.

<a class="level-item button" href="http://www.luminusweb.net/docs/middleware.md">learn
more about middleware »</a>

<p class="title is-5">Need some help?</p>

Visit the [official documentation](http://www.luminusweb.net/docs) for examples
on how to accomplish common tasks with Luminus. The `#luminus` channel on the
[Clojurians Slack](http://clojurians.net/) and
[Google Group](https://groups.google.com/forum/#!forum/luminusweb) are both
great places to seek help and discuss projects with other users.
