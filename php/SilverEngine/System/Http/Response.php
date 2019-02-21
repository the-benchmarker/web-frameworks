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

namespace Silver\Http;

use Silver\Core\AppInstanceTrait;
use Silver\Core\Blueprints\Http\ResponseInterface;

/**
 * Class Response
 *
 * @package Silver\Http
 */
class Response implements ResponseInterface
{

    /**
     * success response code
     *
     * @var int
     */
    private $code = 200;

    /**
     * headers
     *
     * @var array
     */
    private $headers = [];

    /**
     * cookies
     *
     * @var array
     */
    private $cookies = [];

    /**
     * response body
     *
     * @var null
     */
    private $body = null;

    /**
     * App instance Trait
     */
    use AppInstanceTrait;

    /**
     * set header method
     *
     * @param $key
     * @param $value
     */
    public function setHeader($key, $value)
    {
        $this->headers[$key] = $value;
    }

    /**
     * @param $payload
     * @return mixed
     */
    public function json($payload)
    {
        header('Content-type: Application/json');
        return json_decode($payload);
    }

    /**
     * @param $payload
     * @return string
     */
    public function xml($payload)
    {
        return ' $payload ';
    }

    /**
     * get header method
     *
     * @param  $key
     * @param  null $default
     * @return mixed|null
     */
    public function getHeader($key, $default = null)
    {
        return isset($this->headers[$key]) ? $this->headers[$key] : $default;
    }

    /**
     * set response code method
     *
     * @param $code
     */
    public function setCode($code)
    {
        $this->code = $code;
    }

    /**
     * get response code method
     *
     * @return int
     */
    public function getCode()
    {
        return $this->code;
    }


    /**
     * set response body method
     *
     * @param  $body
     * @throws \Exception
     */
    public function setBody($body)
    {
        if (!is_string($body)
            && !($body instanceof \Silver\Core\Render)
            && !(is_array($body) or is_object($body))
        ) {
            throw new \Exception("Unknown body type.");
        }

        $this->body = $body;
    }

    /**
     * send esponse method
     *
     * @throws \Exception
     */
    public function send()
    {

        // Robust Accept header checking
        $types = preg_split('/[,;] */', $_SERVER['HTTP_ACCEPT']); // XXX: Request::header()
        $content_type = false;

        foreach ($types as $type) {
            if (in_array($type, ['application/json', 'text/html', 'text/*', '*/*'])) {
                $content_type = $type;
                break;
            }
        }

        if ($content_type === false) {
            http_response_code(406);
            return;
        }

        // Override accept header with application/json
        // for development purpose
        if (\Silver\Core\Env::get('debug') && \Silver\Http\Request::instance()->input('__override_json__')) {
            $content_type = 'application/json';
            $this->setHeader('Content-Type', 'application/json');
        }

        // XXX: if not setted
        $this->setHeader('Content-Type', $content_type);

        http_response_code($this->code);
        foreach ($this->headers as $key => $value) {
            header($key . ': ' . $value);
        }

        foreach ($this->cookies as $name => $value) {
            list ($value, $expiration) = $value;
            setcookie($name, $value, $expiration);
        }

        if ($this->body !== null) {
            $body = $this->body;

            switch ($content_type) {
            case '*/*':
            case 'text/*':
            case 'text/html':
                if (is_string($body) || is_numeric($body)) {
                    echo $body;
                } elseif ($body instanceof \Silver\Core\Blueprints\RenderInterface) {
                    echo $body->render();
                } elseif (is_array($body) or is_object($body)) {
                    print json_encode($body);
                } else {
                    // Impossible
                    throw new \Exception("FATAL: Unknown body type.");
                }
                break;
            case 'application/json':
                if ($body instanceof \Silver\Core\Blueprints\RenderInterface) {
                    $body = $body->data();
                }
                print json_encode($body);
                break;
            default:
                throw new \Exception('Not possible');
            }
        }
        else{
            //            dd(112222);
            //when you dont use body
            return false;
            //            dd('check from kernel in file response on line 156');
        }
    }
}
