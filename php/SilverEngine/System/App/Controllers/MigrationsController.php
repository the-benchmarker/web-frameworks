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

namespace System\App\Controllers;

use Silver\Core\Controller;
use Silver\Database\Query as DB;

class MigrationsController extends Controller
{
    public function up($modelName = false)
    {
        if($modelName) {
            if($modelName == 'migrations') {
                return 'This is GLOBAL SYSTEM Name!';
            }

            if(! is_file(ROOT.'Database'.DS.'Migrations'.DS.$modelName.'Migrate'.EXT)) {
                return "File or namespace in: ".ROOT."Database".DS."Migrations".DS.ucfirst($modelName)."Migrate".EXT." not exists!";
            }

            $namespace = "Database\\Migrations\\";
            $migrate = $namespace . ucfirst($modelName).'Migrate';
            $migrate::up();
            $list[] = $modelName;

            $model = ucfirst($modelName).'Migrate';
            $check = DB::count()
            ->from('migrations')->where('model_name', $model)->fetch();

            if($check->count == 0) {
                DB::insert('migrations', ['model_name'=> ucfirst($modelName).'Migrate'])->execute();
            }
        }
        else{
            $models = ['UsersMigrate','Migrations'];
            $path    = ROOT. 'Database/Migrations/';
            $files = array_diff(scandir($path), array('.', '..'));

            foreach ($files as $row) {
                $row = preg_replace('/.php/', '', $row);

                if($row != 'Migrations') {
                    DB::insert('Migrations', ['model_name'=> $row])->execute();
                }

                $namespace = "Database\\Migrations\\";
                $migrate = $namespace . ucfirst($row);
                $migrate::up();
                $list[] = $row;
            }
        }
        print_r($list);
        return '- Migrations created!';
    }

    public function down($modelName = false)
    {
        if($modelName) {
            if($modelName == 'migrations') {
                return 'This is GLOBAL SYSTEM Name!';
            }

            if(! is_file(ROOT.'Database'.DS.'Migrations'.DS.$modelName.'Migrate'.EXT)) {
                return "File or namespace in: ".ROOT."Database".DS."Migrations".DS.ucfirst($modelName)."Migrate".EXT." not exists!";
            }

            $namespace = "Database\\Migrations\\";
            $migrate = $namespace . ucfirst($modelName).'Migrate';
            $migrate::down();
            $list[] = $modelName;
        }
        else{
            $path    = ROOT. 'Database/Migrations/';
            $files = array_diff(scandir($path), array('.', '..'));

            foreach ($files as $row) {
                $row = preg_replace('/.php/', '', $row);

                $namespace = "Database\\Migrations\\";
                $migrate = $namespace . ucfirst($row);
                $migrate::down();
                $list[] = $row;
            }
        }
        print_r($list);
        return '- Migrations created!';
    }

    public function all()
    {

        $pathMigrations    = ROOT. 'Database/Migrations/';
        $filesMigrations = array_diff(scandir($pathMigrations), array('.', '..'));

        foreach ($filesMigrations as $row) {
            $row = preg_replace('/.php/', '', $row);
            $namespace = "Database\\Migrations\\";
            $migrate = $namespace . ucfirst($row);
            $migrate::up();
            $list[] = $row;
        }

        $pathSeeds    = ROOT. 'Database/Seeds/';
        $filesSeeds   = array_diff(scandir($pathSeeds), array('.', '..'));

        foreach ($filesSeeds as $row) {
            $row = preg_replace('/.php/', '', $row);
            $namespace = "Database\\Seeds\\";
            $seed = $namespace . ucfirst($row);
            $seed::run();
            $listSeeds[] = $row;
        }

        print_r($list);
        print_r($listSeeds);
        return '- Migrations created!';
    }
}
