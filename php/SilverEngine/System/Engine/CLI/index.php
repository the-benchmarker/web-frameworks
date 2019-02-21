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

namespace Silver\Engine;

use Silver\Engine\Ghost\Template;

class CLI
{
    private $cmd = [];
    private $args = [];

    public function __construct($command)
    {

        if (!isset($command[1])) {
            echo "\n Welcome to SilverEngine framework \n\n";

            echo " - try use 'php silver help' \n";
            echo "\n ----- \n";
            echo " - Create CRUD resource: php silver g resource {name} \n";
            echo " - Create Controller: php silver g controller {name} \n";
            echo " - Create Model: php silver g model {name} \n";
            echo " - Create View: php silver g view {name} \n";
            echo " ----- \n";
            echo " - Create Facade: php silver g facade {name} \n";
            echo " - Create Helper: php silver g helper {name} \n";
            echo " ----- \n";
            echo " - Delete CRUD resource: php silver g resource {name} \n";
            exit;
        }

        $this->cmd = $command[1];
        $this->args = $command;


        return $this->run();
    }

    private function run()
    {
        switch ($this->cmd) {
            case "g":
            case "c":
                return $this->make();
                break;

            case "d":
                return $this->delete();
                break;

            case "migrate":
                return $this->migrate();
                break;


            default:
                echo 'Comand not exits';
                break;
        }
    }

    private function migrate()
    {
        //todo:  need to redone this is not oK!
        if (empty($this->args[2])) {
            $path = ROOT . 'Database/Migrations/';;
            $files = array_diff(scandir($path), array('.', '..'));

            foreach ($files as $key) {
                $key = preg_replace('/.php/', '', $key);
                // exit($path.$key.'.php');
                include_once $path . $key . '.php';
                $namespace = "\\Database\\Migrations\\" . $key;
                // exit($namespace);
                $namespace::up();
                return 'test1';
            }
        } else {

        }
    }

    private function seed()
    {

    }

    private function make()
    {
        // var_dump($this);
        // exit;

        if ($this->args[2] == 'resource') {
            foreach (['controller', 'view', 'model'] as $type) {
                $this->generate($type, $this->args[3]);
            }
        } elseif ($this->args[2] == 'controller'
            || $this->args[2] == 'model'
            || $this->args[2] == 'view'
            || $this->args[2] == 'helper'
            || $this->args[2] == 'facade') {
            $this->generate($this->args[2], $this->args[3]);
        } else {
            $this->error(
                'Please enter complete command',
                [
                    'resource',
                    'controller',
                    'model',
                    'view',
                    'helper',
                    'facade',
                ]
            );
        }


    }

    private function delete()
    {

        if ($this->args[2] == 'resource') {
            foreach (['model', 'view', 'controller'] as $type) {
                $this->deleteResources($type, $this->args[3]);
            }
        } else {
            $this->error(
                'Please enter complete command',
                [
                    'resource',
                    'controller',
                    'model',
                    'view',
                    'helper',
                    'facade',
                ]
            );
        }


    }

    private function generate($type, $name, $force = false)
    {
        $template = null;
        $destination = null;

        switch ($type) {
            case 'model':
            case 'controller':
                $template = ROOT . 'App/Templates/' . ucfirst($type) . '.ghost.tpl';
                $destination = ROOT . 'App/' . ucfirst($type) . 's/' . ucfirst($name) . ucfirst($type) . EXT;
                break;
            case 'view':
                $name = strtolower($name);
                $name = str_replace('.', '/', $name);
              // exit(ROOT);
                $template = ROOT . 'App/Templates/View.ghost.tpl';
                $destination = ROOT . 'App/Views/' . $name . '.ghost.tpl';
            // exit($destination);
                break;
            case 'event':
                $this->createDirIfNorExists('Events', ROOT . 'App/');
                $this->createDirIfNorExists('Listeners', ROOT . 'App/');
                $name = strtolower($name);
                $name = str_replace('.', '/', $name);
                $template = ROOT . 'App/Templates/Event.ghost.tpl';
                $destination = ROOT . 'App/Events/' . $name . EXT;

                $template2 = ROOT . 'App/Templates/Listeners.ghost.tpl';
                $destination2 = ROOT . 'App/Listeners/' . $name . EXT;
                break;
            case 'helper':
                $this->createDirIfNorExists('Helpers', ROOT . 'App/');
                $name = strtolower($name);
                $name = str_replace('.', '/', $name);
                $template = ROOT . 'App/Templates/Helper.ghost.tpl';
                $destination = ROOT . 'App/Helpers/' . $name . EXT;
                break;
            case 'facade':
                $this->createDirIfNorExists('Facades', ROOT . 'App/');
                $name = strtolower($name);
                $name = str_replace('.', '/', $name);
                $template = ROOT . 'App/Templates/Facade.ghost.tpl';
                $destination = ROOT . 'App/Facades/' . ucfirst($name) . EXT;
                break;
            default:
                $this->error(
                    'Please enter complete command',
                    [
                        'resource',
                        'controller',
                        'model',
                        'view',
                        'helper',
                        'facade',
                    ]
                );
                break;
        }

        if (!file_exists($template)) {
            $this->error('Template is missing');
        }

        if ($type == 'view' || $type == 'v') {
            return $this->generateView('y', $template, $destination, $type, $name);
            // return $this->generateFile('y', $template, $destination, $type, $name);
        }

        if (file_exists($destination) and $force === false) {
            return $this->error('File exists!');
        } else {
            return $this->generateFile('y', $template, $destination, $type, $name);
        }
    }

    private function deleteResources($type, $name, $force = false)
    {
        $template = null;
        $destination = null;


        switch ($type) {
            case 'model':
            case 'controller':
                $destination = ROOT . 'App/' . ucfirst($type) . 's/' . ucfirst($name) . ucfirst($type) . EXT;
                $this->fix_routes($name, false);
                break;
            case 'view':
                $name = strtolower($name);
                $name = str_replace('.', '/', $name);
                $destination = ROOT . 'App/Views/' . $name . '.ghost.tpl';
                break;
            case 'event':
                $this->createDirIfNorExists('Events', ROOT . 'App/');
                $this->createDirIfNorExists('Listeners', ROOT . 'App/');
                $name = strtolower($name);
                $name = str_replace('.', '/', $name);
                $destination = ROOT . 'App/Events/' . $name . EXT;

                $template2 = ROOT . 'App/Templates/Listeners.ghost.tpl';
                $destination2 = ROOT . 'App/Listeners/' . $name . EXT;
                break;
            case 'helper':
                $this->createDirIfNorExists('Helpers', ROOT . 'App/');
                $name = strtolower($name);
                $name = str_replace('.', '/', $name);
                $destination = ROOT . 'App/Helpers/' . $name . EXT;
                break;
            case 'facade':
                $this->createDirIfNorExists('Facades', ROOT . 'App/');
                $name = strtolower($name);
                $name = str_replace('.', '/', $name);
                $destination = ROOT . 'App/Facades/' . ucfirst($name) . EXT;
                break;
            default:
                $this->error(
                    'Please enter complete command',
                    [
                        'resource',
                        'controller',
                        'model',
                        'view',
                        'helper',
                        'facade',
                    ]
                );
                break;
        }


        if (! file_exists($destination) and $force !== true) {
            return $this->error('File exists!');
        } else {
            return $this->deleteFile('y', $destination, $type, $name);
        }
    }

    private function generateFile($yes, $template, $destination, $type, $name)
    {
        // RenderInterface template
        $ghost = new Template($template);
        $ghost->set('type', $type);
        $ghost->set('name', $name);

        file_put_contents($destination, $ghost->render());

        if ($type == 'controller') {
            $this->fix_routes($name);
        }

        $this->success("{$type} {$name} successfully created. ({$destination})");
    }

    private function deleteFile($yes, $destination, $type, $name)
    {
        unlink($destination);
        $this->success("{$type} {$name} successfully deleted. ({$destination})");
    }

    private function generateView($yes, $template, $destination, $type, $name)
    {
        // RenderInterface template
        $ghost = new Template($template);
        $ghost->set('type', $type);
        $ghost->set('name', $name);

        // var_dump($template,$destination);
        // die();
        $temp = "{{ extends('layouts.master') }}

#set[content]
    <p>Welcome to <b> @routeName()</b> page</p>
    <p>This file you can find in App/Views/@routeName().ghost.tpl</p>
    <p>Also check out  App/Views/layouts/master.ghost.tpl<p>
#end";

        file_put_contents($destination, $temp);

        $this->success("{$type} {$name} successfully created. ({$destination})");
    }

    private function fix_routes($name, $add = true)
    {
        $name = ucfirst($name);

        if (is_file(ROOT . 'App' . DS . 'Routes' . EXT))
            $routes_path = ROOT . 'App' . DS . 'Routes' . EXT;
        else
            $routes_path = ROOT . 'App' . DS . 'Routes' . DS . 'Web' . EXT;

        $content = FILE($routes_path);
        $fh = fopen($routes_path, 'w');

        if ($fh) {
            foreach ($content as $row) {
                if (strpos($row, "{$name}@") === false or strpos(trim($row), "//") !== false) {
                    fwrite($fh, $row);
                } else {
                    fwrite($fh, "// " . trim($row) . "    -- removed by resource manager\n");
                }
            }

            if ($add === true) {
                fwrite($fh, "\n");
                fwrite($fh, "// Route for {$name} controller.\n");
                fwrite($fh, "Route::get('/" . lcfirst($name) . "', '{$name}@get', '" . lcfirst($name) . "', 'public');\n");
                fclose($fh);
                $this->success('Route created!');
            }
        } else {
            if (is_file(ROOT . 'App' . DS . 'Routes' . EXT))
                $this->error('Can not open App/Routes.php.');
            else
                $this->error('Can not open App/Routes/Web.php.');
        }
    }

    private function error($message = null, $help = false)
    {
        if ($message) {
            echo "ERROR: {$message}\n";
            print_r(json_encode($help));
        } else {
            echo "ERROR! \n";
            print_r(json_encode($help));
        }
    }

    private function success($message)
    {
        echo "{$message}\n";
    }

    private function createDirIfNorExists($name, $path)
    {
        if (is_dir($path . $name)) {
            return false;
        } else {
            mkdir($path . ucfirst($name), 0766);
        }
    }

}
