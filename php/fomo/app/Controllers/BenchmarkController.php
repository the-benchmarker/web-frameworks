<?php

namespace App\Controllers;

use Fomo\Request\Request;

class BenchmarkController
{
    public function index(): string
    {
        return "HTTP/1.1 200 OK\r\nContent-Type: text/html;charset=utf-8\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n";
    }

    public function getUser(Request $request, $id): string
    {
        $length = strlen($id);
        return "HTTP/1.1 200 OK\r\nContent-Type: text/html;charset=utf-8\r\nContent-Length: $length\r\nConnection: keep-alive\r\n\r\n$id";
    }

    public function postUser(): string
    {
        return "HTTP/1.1 200 OK\r\nContent-Type: text/html;charset=utf-8\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n";
    }
}

