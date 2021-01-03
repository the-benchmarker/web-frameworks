<?php
return array(
		"siteUrl"=>"http://0.0.0.0:3000",
		"sessionName"=>"ubiquity",
		"namespaces"=>[],
		"test"=>false,
		"debug"=>false,
		"logger"=>function(){return new \Ubiquity\log\libraries\UMonolog("ubiquity",\Monolog\Logger::INFO);},
		"cache"=>["directory"=>"cache/","system"=>"Ubiquity\\cache\\system\\ArrayCache","params"=>[]],
		"mvcNS"=>["models"=>"models","controllers"=>"controllers","rest"=>""],
		"isRest"=>function(){
			return \Ubiquity\utils\http\URequest::getUrlParts()[0]==="rest";
		}
);
