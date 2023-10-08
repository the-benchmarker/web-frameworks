<?php

define('DIR', dirname(__DIR__));
chdir(DIR);
require 'main/conf.php';
new SKY;

Plan::gate_p('main-default_c.php', Gate::instance()->parse('main', 'mvc/default_c.php', false));
