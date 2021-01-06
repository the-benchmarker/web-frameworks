<?php
return array(
	"classes-files" => array(),
	"classes" => array(),
	"paths" => array(),
	"excludeds" => array(),
	"libraries-parts" => array(),
	"callback" => function (\Ubiquity\cache\Preloader $preloader) {
		$preloader->addUbiquityBasics();
	}
);