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

class ApiTransform implements MiddlewareInterface
{
    public function execute(Request $req, Response $res, Closure $next) 
    {
        $data = $next();

        //       if (Request::header('accept', 'application/json')) {
        //            $data = $data->data();
        //
        //
        //            return [
        //                'data' => $data,
        //                'status' => \Silver\Http\Response::instance()->getCode(),
        //                'msg' => 'not found',
        //            ];
        //        }

        return $data;
    }
}