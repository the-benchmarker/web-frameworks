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

namespace Silver\Core\Blueprints;

use Silver\Http\Request;
use Silver\Http\Response;
use Closure;

interface MiddlewareInterface
{
    public function execute(Request $req, Response $res, Closure $next);
}