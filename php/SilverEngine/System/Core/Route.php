<?php

/**
 * SilverEngine  - PHP MVC framework
 *
 * @package   SilverEngine
 * @author    SilverEngine Team
 * @copyright 2015-2017
 * @license   MIT
 * @link      https://github.com/SilverEngine/Framework
 */

namespace Silver\Core;

// Rewrite: remove $_type

class Route
{
    private $_method;
    private $_route;
    private $_action;
    private $_jails;
    private $_name;
    private $_middleware;
    private $_type;
    private $_variables = [];

    private static $jails = [];
    private static $_prefix = '';
    private static $_routes = [];
    private static $_route_index = [];

    private static $_types = [
        'int'    => '/^[0-9]+$/',
        'string' => '/^[a-zA-Z]+/$',
        'hash'   => 'md5',
    ];

    /**
     * Route constructor.
     *
     * @param $method
     * @param $route
     * @param $action
     * @param null   $name
     * @param null   $type
     */
    public function __construct($method, $route, $action, $name = null, $middleware = 'public', $type = null)
    {
        $this->_method = strtolower($method);
        $this->_route = $route;
        $this->_action = $action;
        $this->_jails = self::$jails;
        $this->_name = $name;
        $this->_middleware   = $middleware;
        $this->_type = $type;
    }

    /**
     * @return mixed
     */
    public function action()
    {
        return $this->_action;
    }

    /**
     * @return null
     */
    public function type()
    {
        return $this->_type;
    }

    /**
     * @return null
     */
    public function name()
    {
        return $this->_name;
    }

    /**
     * @return null
     */
    public function middleware()
    {
        return $this->_middleware;
    }

    /**
     * @return string
     */
    public function method()
    {
        return $this->_method;
    }



    /**
     * @param array $vars
     * @return string
     * @throws \Exception
     */
    public function url($vars = [])
    {
        $parts = explode('/', $this->_route);
        $url = [];

        $i_variable = 0;
        foreach ($parts as $part) {
            if (empty($part)) {
                continue;
            }

            if ($part[0] == '{') { // is variable?
                $key = trim($part, '{}');

                if (isset($vars[0])) {
                    if (!isset($vars[ $i_variable ])) {
                        throw new \Exception("Route {$this->_route} has no variable $key.");
                    }

                    $value = $vars[ $i_variable++ ];
                } else {
                    if (!isset($vars[ $key ])) {
                        throw new \Exception("Route {$this->_route} has no variable $key.");
                    }

                    $value = $vars[ $key ];
                }
                $url[] = $value;
            } else {
                $url[] = $part;
            }
        }

        return BASEPATH . '/' . implode('/', $url);
    }

    /**
     * @return array
     */
    public function variables()
    {
        return $this->_variables;
    }

    /**
     * @param $index
     * @return mixed|string
     */
    public function segment($index)
    {
        $segments = explode('/', $this->_route);
        $seg = $segments[ $index ];

        if ($seg[0] == '{') {
            $seg = substr($seg, 1, -1);
            if ($seg[ strlen($seg) - 1 ] == '?') {
                $seg = substr($seg, 0, -1);
            }

            return $this->_variables[ $seg ];
        } else {
            return $seg;
        }
    }

    /**
     * @param $method
     * @param $url
     * @return bool
     * @throws \Exception
     */
    public function check($method, $url)
    {
        if ($this->_method != 'ANY') {
            if ($method != $this->_method) {
                return false;
            }
        }

        $route = explode('/', rtrim($this->_route, '/'));
        $url = explode('/', rtrim($url, '/'));

        while ($route) {
            if (strlen($route[0]) and $route[0][0] == '{') { // variable
                $required = true;

                $varname = substr($route[0], 1, -1);

                if ($varname[ strlen($varname) - 1 ] == '?') {
                    $required = false;
                    $varname = substr($varname, 0, -1);
                }

                if (strpos($varname, ':') !== false) {
                    list($varname, $type) = explode(':', $varname);
                    $rule = self::$_types[ $type ];

                    if (!$rule) {
                        throw new \Exception("Invalid route variable type $type.");
                    }

                    if ($rule[0] == '/') {
                        if (!preg_match($rule, $url[0])) {
                            throw new \Exception("Invalid variable type.");
                        }
                    } elseif (function_exists($rule)) {
                        $url[0] = $rule($url[0]);
                    } else {
                        throw new \Exception("Invalid rule for type $type.");
                    }
                }

                if ($url) {
                    $this->_variables[ $varname ] = $url[0];
                } else {
                    if ($required) {
                        return false;
                    } else {
                        $this->_variables[ $varname ] = null;
                    }
                }
            } else {
                if (!$url) {
                    return false;
                }
                if ($route[0] != $url[0]) {
                    return false;
                }
            }

            array_shift($route);
            array_shift($url);
        }

        if ($url) {
            return false;
        }


        // Check jails
        //TODO: deprecated!
        foreach ($this->_jails as $jail) {
            $jail = explode('@', $jail);

            $class = $jail[0];
            $method = isset($jail[1]) ? $jail[1] : 'protect';
            if ($class[0] != '\\') {
                $class = '\\App\\Jail\\' . $class;
            }

            if (!$class::$method()) {
                return false;
            }
        }

        return true;
    }

    /**
     * @param $args
     * @param $fn
     */
    public static function group($args, $fn)
    {
        // Prepare
        //TODO: deprecated use MW
        if (isset($args['jail'])) {
            self::$jails[] = $args['jail'];
        }

        if (!isset($args['prefix'])) {
            $args['prefix'] = '';
        }

        $old_prefix = self::$_prefix;
        self::$_prefix = self::$_prefix . '/'.$args['prefix'];

        // Execute
        $fn();

        // Restore
        self::$_prefix = $old_prefix;

        // TODO: deprecated use MW
        if (isset($args['jail'])) {
            array_pop(self::$jails);
        }
    }

    /**
     * @param $requestUrl
     * @param $requestMethod
     * @return mixed|null
     */
    public static function find($requestUrl, $requestMethod)
    {
        foreach (self::$_routes as $route) {
            if ($route->check($requestMethod, $requestUrl)) {
                return $route;
            }
        }

        return null;
    }

    /**
     * @param $method
     * @param $route
     * @param $action
     * @param null   $name
     * @param string $type
     */
    public static function register($method, $route, $action, $name = null, $middleware = 'public', $type = '')
    {
        $route = self::$_prefix . $route;
        foreach (explode("|", $method) as $m) {
            $r = new Route($m, $route, $action, $name, $middleware,  $type);
            self::$_routes[] = $r;
            self::$_route_index[ $name ] = $r;
        }
    }

    /**
     * @param $route
     * @param $action
     * @param null   $name
     */
    public static function get($route, $action, $name = null, $middleware ='public')
    {
        return self::register("get", $route, $action, $name, $middleware, 'get');
    }

    /**
     * @param $route
     * @param $action
     * @param null   $name
     */
    public static function post($route, $action, $name = null, $middleware ='public')
    {
        return self::register("post", $route, $action, $name, $middleware, 'post');
    }


    // need to work on POST too
    /**
     * @param $route
     * @param $action
     * @param null   $name
     * @param string $req
     */
    public static function put($route, $action, $name = null, $middleware ='public', $req = 'put')
    {
        return self::register("put", $route, $action, $name, $middleware,  $req);
    }

    // need to work on POST too
    /**
     * @param $route
     * @param $action
     * @param null   $name
     * @param string $req
     */
    public static function delete($route, $action, $name = null, $middleware ='public',  $req = 'delete')
    {
        return self::register("delete", $route, $action, $name, $middleware, $req);
    }


    /**
     * @param $route
     * @param $action
     * @param null   $name
     */
    public static function resource($route, $action, $name = null, $middleware ='public')
    {
        // Remove possible method name
        if (($pos = strpos($action, '@')) !== false) {
            $action = substr($action, 0, $pos);
        }

        $route = rtrim($route, '/');

        self::register(
            'get',
            $route,
            $action . '@get',
            $name,
            $middleware,
            'resources'
        );

        self::register(
            'post',
            $route,
            $action . '@post',
            $name,
            $middleware,
            'resources'
        );

        foreach (['get', 'put', 'patch', 'delete'] as $method) {
            self::register(
                $method,
                $route . '/{id}',
                $action . '@' . $method,
                $name,
                $middleware,
                'resources'
            );
        }
    }

    /**
     * @param $route
     * @param $action
     * @param null   $name
     */
    public static function any($route, $action, $name = null, $middleware ='public')
    {
        return self::register("any", $route, $action, $name, $middleware, 'any');
    }

    /**
     * @param $name
     * @return mixed
     * @throws \Exception
     */
    public static function getRoute($name)
    {
        // ndd($name);
        if (isset(self::$_route_index[ $name ])) {
            return self::$_route_index[ $name ];
        }
        throw new \Exception("Route $name not found.");
    }
}
