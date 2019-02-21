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

namespace Silver\App\Middlewares;

use Silver\Core\Blueprints\MiddlewareInterface;
use Silver\Http\Request;
use Silver\Http\Response;
use Closure;

class AccessLog implements MiddlewareInterface
{
    public function execute(Request $req, Response $res, Closure $next) 
    {
        $path = ROOT . "Storage/Logs/" . date("Y-m-d") . "-access.log";
        $fp = fopen($path, 'a+');
        $line = sprintf("%20s [%s] %s: %s\n", $req->ip(), Date("Y-m-d H:i:s"), $req->method(), $req->getUri());
        if($fp) {
            fwrite($fp, $line);
            fclose($fp);
        } else {
            throw new \Exception("Unable to write to file $path.");
        }

        return $next();
    }
}