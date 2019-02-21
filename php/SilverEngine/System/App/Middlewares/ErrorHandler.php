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
use Silver\Exception\NotFoundException;
use Silver\Core\ErrorHandler as Handler;
use Closure;

class ErrorHandler implements MiddlewareInterface
{
    public function execute(Request $req, Response $res, Closure $next) 
    {
        try {
            return $next();
        } catch(NotFoundException $e) {
            $res->setCode(404);
            return Handler::render($e);
        } catch(\Exception $e) {
            $res->setCode(500);
            return Handler::render($e);
        }
    }
}