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

namespace Silver\Core;


/**
 *
 */
class Controller
{
    public $model = null;
    public $modelNamespace = null;
    private $modelPath = null;

    public function __construct()
    {
        if (isset($this->controllerName)) {
            $modelName = $this->controllerName;

            $this->modelPath = ROOT . 'App' . DS . 'Models' . DS . ucfirst($modelName) . 'Model' . EXT;

            if ($modelName and file_exists($this->modelPath)) {
                $this->modelNamespace = $model = "\\App\\Models\\" . ucfirst($modelName) . 'Model';
            } else {
                throw new \Exception(sprintf('%s model file not found', $modelName));
            }
        }
    }


    /**
     *    Access to model
     *
     * @return mixed
     */
    protected function model($model = false)
    {
        if ($model) {
            $this->model = $model;
            $modelName = $model;
        } else {
            $modelName = $this->controllerName;
        }
    }
}
