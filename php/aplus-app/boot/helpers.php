<?php declare(strict_types=1);
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * @package app
 */

use Framework\Helpers\ArraySimple;
use Framework\HTTP\Response;
use Framework\MVC\App;
use Framework\MVC\Model;
use Framework\Routing\Route;
use Framework\Session\Session;
use JetBrains\PhpStorm\Pure;

/**
 * Load helper files.
 *
 * @param array<int,string>|string $helper A list of helper names as array
 * or a helper name as string
 *
 * @return array<int,string> A list of all loaded files
 */
function helpers(array | string $helper) : array
{
    if (is_array($helper)) {
        $files = [];
        foreach ($helper as $item) {
            $files[] = helpers($item);
        }
        return array_merge(...$files);
    }
    $files = App::locator()->findFiles('Helpers/' . $helper);
    foreach ($files as $file) {
        require_once $file;
    }
    return $files;
}

/**
 * Escape special characters to HTML entities.
 *
 * @param string|null $text The text to be escaped
 * @param string $encoding The escaped text encoding
 *
 * @return string The escaped text
 */
#[Pure]
function esc(?string $text, string $encoding = 'UTF-8') : string
{
    $text = (string) $text;
    return empty($text)
        ? $text
        : htmlspecialchars($text, \ENT_QUOTES | \ENT_HTML5, $encoding);
}

/**
 * Renders a view.
 *
 * @param string $path View path
 * @param array<string,mixed> $variables Variables passed to the view
 * @param string $instance The View instance name
 *
 * @return string The rendered view contents
 */
function view(string $path, array $variables = [], string $instance = 'default') : string
{
    return App::view($instance)->render($path, $variables);
}

/**
 * Get the current URL.
 *
 * @return string
 */
function current_url() : string
{
    return App::request()->getUrl()->toString();
}

/**
 * Get the current Route.
 *
 * @return Framework\Routing\Route
 */
function current_route() : Route
{
    return App::router()->getMatchedRoute();
}

/**
 * Get a URL based in a Route name.
 *
 * @param string $name Route name
 * @param array<mixed> $pathArgs Route path arguments
 * @param array<mixed> $originArgs Route origin arguments
 *
 * @return string The Route URL
 */
function route_url(string $name, array $pathArgs = [], array $originArgs = []) : string
{
    $route = App::router()->getNamedRoute($name);
    $matched = App::router()->getMatchedRoute();
    if (empty($originArgs)
        && $matched
        && $route->getOrigin() === $matched->getOrigin()
    ) {
        $originArgs = App::router()->getMatchedOriginArguments();
    }
    return $route->getUrl($originArgs, $pathArgs);
}

/**
 * Renders a language file line with dot notation format.
 *
 * e.g. home.hello matches 'home' for file and 'hello' for line.
 *
 * @param string $line The dot notation file line
 * @param array<int|string,string> $args The arguments to be used in the
 * formatted text
 * @param string|null $locale A custom locale or null to use the current
 *
 * @return string|null The rendered text or null if not found
 */
function lang(string $line, array $args = [], string $locale = null) : ?string
{
    return App::language()->lang($line, $args, $locale);
}

/**
 * Get the Session instance.
 *
 * @return Framework\Session\Session
 */
function session() : Session
{
    return App::session();
}

/**
 * Get data from old redirect.
 *
 * @param string|null $key Set null to return all data
 * @param bool $escape
 *
 * @see Framework\HTTP\Request::getRedirectData()
 * @see Framework\HTTP\Response::redirect()
 * @see redirect()
 *
 * @return mixed The old value. If $escape is true and the value is not
 * stringable, an empty string will return
 */
function old(?string $key, bool $escape = true) : mixed
{
    App::session()->activate();
    $data = App::request()->getRedirectData($key);
    if ($data !== null && $escape) {
        $data = is_scalar($data) || (is_object($data) && method_exists($data, '__toString'))
            ? esc((string) $data)
            : '';
    }
    return $data;
}

/**
 * Tells if session has old data.
 *
 * @param string|null $key null to check all data or a specific key in the
 * array simple format
 *
 * @see old()
 *
 * @return bool
 */
function has_old(string $key = null) : bool
{
    App::session()->activate();
    return App::request()->getRedirectData($key) !== null;
}

/**
 * Renders the AntiCSRF input.
 *
 * @param string $instance The antiCsrf service instance name
 *
 * @return string An HTML hidden input if antiCsrf service is enabled or an
 * empty string if it is disabled
 */
function csrf_input(string $instance = 'default') : string
{
    return App::antiCsrf($instance)->input();
}

/**
 * Set Response status as "404 Not Found" and auto set body as
 * JSON or HTML page based on Request Content-Type header.
 *
 * @param array<string,mixed> $variables
 *
 * @return Framework\HTTP\Response
 */
function respond_not_found(array $variables = []) : Response
{
    $request = App::request();
    $response = App::response();
    $response->setStatus(404);
    if ($request->isJson() || $request->negotiateAccept([
            'text/html',
            'application/json',
        ]) === 'application/json') {
        return $response->setJson([
            'error' => [
                'code' => 404,
                'reason' => 'Not Found',
            ],
        ]);
    }
    $variables['title'] ??= lang('routing.error404');
    $variables['message'] ??= lang('routing.pageNotFound');
    return $response->setBody(
        view('errors/404', $variables)
    );
}

/**
 * Sets the HTTP Redirect Response with data accessible in the next HTTP
 * Request.
 *
 * @param string $location Location Header value
 * @param array<int|string,mixed> $data Session data available on next
 * Request
 * @param int|null $code HTTP Redirect status code. Leave null to determine
 * based on the current HTTP method.
 *
 * @see http://en.wikipedia.org/wiki/Post/Redirect/Get
 * @see Framework\HTTP\Request::getRedirectData()
 * @see old()
 *
 * @throws InvalidArgumentException for invalid Redirection code
 *
 * @return Framework\HTTP\Response
 */
function redirect(string $location, array $data = [], int $code = null) : Response
{
    if ($data) {
        App::session()->activate();
    }
    return App::response()->redirect($location, $data, $code);
}

/**
 * Redirect to a named route.
 *
 * @param array<mixed>|string $route route name as string or an array with the
 * route name, an array with path args and other array with origin args
 * @param array<mixed> $data Session data available on next
 * Request
 * @param int|null $code HTTP Redirect status code. Leave null to determine
 * based on the current HTTP method.
 *
 * @see http://en.wikipedia.org/wiki/Post/Redirect/Get
 * @see Framework\HTTP\Request::getRedirectData()
 * @see old()
 * @see redirect()
 *
 * @throws InvalidArgumentException for invalid Redirection code
 *
 * @return Framework\HTTP\Response
 */
function redirect_to(
    array | string $route,
    array $data = [],
    int $code = null
) : Response {
    $route = (array) $route;
    $route = route_url(...$route);
    return redirect($route, $data, $code);
}

/**
 * Get configs from a service.
 *
 * @param string $name The service name
 * @param string $key The instance name and, optionally, with keys in the
 * ArraySimple keys format
 *
 * @return mixed The key value
 */
function config(string $name, string $key = 'default') : mixed
{
    [$instance, $keys] = array_pad(explode('[', $key, 2), 2, null);
    $config = App::config()->get($name, $instance);
    if ($keys === null) {
        return $config;
    }
    $pos = strpos($keys, ']');
    if ($pos === false) {
        $pos = strlen($key);
    }
    $parent = substr($keys, 0, $pos);
    $keys = substr($keys, $pos + 1);
    $key = $parent . $keys;
    return ArraySimple::value($key, $config);
}

/**
 * Get same Model instance.
 *
 * @template T of Model
 *
 * @param class-string<T> $class
 *
 * @return T
 */
function model(string $class) : Model
{
    static $models;
    if ( ! isset($models[$class])) {
        $models[$class] = new $class();
    }
    return $models[$class];
}
