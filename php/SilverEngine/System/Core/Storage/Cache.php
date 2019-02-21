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

namespace Silver\Core\Storage;

use Silver\Core\Env;

class Cache
{
    private $ext = '.cache';
    private $path = ROOT . 'Storage/Caches/';
    private $expire_time = null;
    private $enabled = false;

    public function __construct($config = null)
    {
        if(!$config) {
            $config = (array) Env::get('caches');
        }
        
        if(isset($config['ext'])) {
            $this->ext = $config['ext'];
        }

        if(isset($config['path'])) {
            $this->path = $config['path'];
        }

        if(isset($config['expire_time'])) {
            $this->expire_time = $config['expire_time'];
        }

        if(isset($config['enabled'])) {
            $this->enabled = $config['enabled'];
        }
    }

    public function set($key, $data, $time = null) 
    {
        $path = $this->path . 'Session/' . md5($key) . $this->ext;
        
        if($time === null) {
            $time = $this->expire_time;
        }
        
        file_put_contents(
            $path, serialize(
                [
                'expire' => $time ? time() + $time : $time,
                'data' => $data,
                ]
            )
        );
    }

    public function file($key, $content) 
    {
        $path = $this->path . 'Views/' . md5($key) . $this->ext;
        file_put_contents($path, $content);
    }

    public function get($key) 
    {
        if($this->enabled) {
            $path = $this->path . 'Session/' . md5($key) . $this->ext;

            if(file_exists($path)) {
                $data = unserialize(file_get_contents($path));

                // Check time restrition
                if(isset($data['expire']) and $data['expire']) {
                    if($data['expire'] < time()) {
                        unlink($path);
                        return null;
                    }
                }

                return $data['data'];
            }

        }
        return null;
    }

    public function getFile($key, $time = null) 
    {
        if($this->enabled) {
            $path = $this->path . 'Views/' . md5($key) . $this->ext;
        
            if(file_exists($path)) {
                // Check time restriction
                if($time !== null) {
                    if(filemtime($path) + $time < time()) {
                        unlink($path);
                        return null;
                    }
                }

                return file_get_contents($path);
            }
        }
        return null;
    }

    public function cacheFile($key, $fn, $time = null) 
    {
        $data = $this->getFile($key, $time);

        if($data) {
            return $data;
        }

        $data = $fn();
        $this->file($key, $data);
        return $data;
    }

    public function cache($key, $fn, $time) 
    {
        $data = $this->get($key);

        if($data) {
            return $data;
        }

        $data = $fn();
        $this->set($key, $data, $time_or_predictor);
        return $data;
    }
}
