<?php
  return [
   'host' => '0.0.0.0',
   'port' => 3000,
   'mode' => SWOOLE_PROCESS,
   'sockType' => SWOOLE_SOCK_TCP,
   'app' => require __DIR__ . '/swoole.php', 
   'options' => [
       'worker_num' => swoole_cpu_num(),
       'daemonize' => 0,
   ]
];