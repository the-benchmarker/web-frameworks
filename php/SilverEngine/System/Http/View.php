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

use Silver\Core\Blueprints\RenderInterface;
use Silver\Core\App;
use Silver\Core\ErrorHandler;
use Silver\Engine\Ghost\Template;
use Silver\Exception;
use Silver\Helpers\String as Str;
use Silver\Support\Git;


class View implements RenderInterface
{

    private $template;
    private $data = [];

    public static function make($template, $data = [])
    {
        return new static($template, $data);
    }

    public static function error($template, $data = [])
    {
        return new static('errors' . DS . $template, $data);
    }

    public static function demo()
    {
        $branch = Git::test();
        // dd($branch);
        return new static('demo.default', [
            '_branch_' => $branch
        ]);
    }

    public function __construct($template, $data = [])
    {
        $this->template = $template;
        foreach ($data as $key => $value) {
            $this->with($key, $value);
        }
    }

    public function with($key, $value = true)
    {
        $this->data[$key] = $value;
        return $this;
    }

    public function withComponent($value = true, $key = false)
    {

        if ($key) {
            $key = 'component_' . $key;
        } else {
            $key = 'component_payload';
        }

        $this->data[$key] = $value;
        return $this;
    }

    public function data()
    {
        return $this->data;
    }

    public function render()
    {
        $self = $this;

        return ErrorHandler::withFilter(
            E_ALL ^ E_NOTICE,
            function () use ($self) {
                $name = str_replace('.', '/', $self->template);

                if ($target = App::instance()->find('Views/' . $name . '.ghost.php')) {
                    $template = new Template($target, $self->data);
                    return $template->render();
                } else if ($target = App::instance()->find('Views/' . $name . '.ghost.tpl')) {
                    $template = new Template($target, $self->data);
                    return $template->render();
                } else if ($target = App::instance()->find('Views/' . $name . '.php')) {
                    try {
                        foreach ($self->data as $key => $value) {
                            $$key = $value;
                        }

                        ob_start();
                        include $target;
                        $content = ob_get_contents();
                    }
                    finally {
                        ob_end_clean();
                    }
                    return $content;
                } else if ($target = App::instance()->find('Views/' . $name . '.html')) {
                    return file_get_contents($target);
                } else {
                    throw new Exception("Template $name not found.");
                }
            }
        );
    }

    public function __call($key, $args)
    {
        if (Str::startsWith($key, 'with')) {
            $key = lcfirst(substr($key, 4));
            return $this->with($key, count($args) > 0 ? $args[0] : true);
        } else {
            throw new Exception("Call undefined method " . static::class . "::" . $key . '()');
        }
    }
}
