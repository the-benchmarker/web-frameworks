<?php
/**
 * Use this for the parent process before pcntl_fork
 */
/** @var \App\Application $app */
$app = require_once __DIR__ . '/app.php';

$app->preRegisterAllAvailableBindings();

/**
 * SOCKET SANITIZATION
 */
// Disconnect ALL active DB connections (mysql, pgsql, read/write replicas, tenants)
if ($app->resolved('db')) {
    $dbManager = $app->make('db');

    foreach (($dbManager->getConnections() ?? []) as $name => $connection) {
        /** @var \MacropaySolutions\Kernel\Database\Connection $connection */
        $dbManager->purge($name);
        \fwrite(STDERR, "Disconnected db connection {$name};\n");
    }
}

// Disconnect ALL active Redis connections (default, cache, publisher, etc.)
if ($app->resolved('redis')) {
    $redisManager = $app->make('redis');

    foreach (($redisManager->connections() ?? []) as $name => $connection) {
        /** @var \MacropaySolutions\Kernel\Redis\Connections\Connection $connection */
        $connection->disconnect();
        $redisManager->purge($name);
        \fwrite(STDERR, "Disconnected redis connection {$name};\n");
    }
}

return $app;
