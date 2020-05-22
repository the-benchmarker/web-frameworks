<?php

namespace App\Server;

use App\GlobalData\Client;
use One\Http\Router;
use One\Swoole\Server\HttpServer;

class AppHttpServer extends HttpServer
{
//    /**
//     * @var Client
//     */
//    public $client;
//
//    public function __construct(\swoole_server $server, array $conf)
//    {
//        parent::__construct($server, $conf);
//        $this->client = new Client();
//    }

    public function onRequest(\swoole_http_request $request, \swoole_http_response $response)
    {
        $this->httpRouter($request, $response);
    }

    public function onWorkerStart(\swoole_server $server, $worker_id)
    {
        parent::onWorkerStart($server, $worker_id);
        Router::clearCache();
        require _APP_PATH_ . '/config.php';
    }
}
