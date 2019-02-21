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

namespace Silver\Core\Blueprints\Http;

interface RequestInterface
{
    /**
     * getUri is a method that returns the HTTP request URI
     * the expected result could be string or null
     *
     * @return mixed
     */
    public function getUri();

    /**
     * method is a method that returns the HTTP request Method
     * available methods are 'GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'OPTIONS'
     *
     * @return mixed
     */
    public function method();

    /**
     * header is a method that returns all the information of the HTTP header
     * header information :
     * HOST, CONNECTION, CACHE_CONTROL, USER_AGENT, DTN, COOKIE,ENCODING and so on
     *
     * @return mixed
     */
    public function header();
}