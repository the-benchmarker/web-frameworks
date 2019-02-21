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

use PDO;
use Silver\Core\Env;


/**
 * Model
 */
class Model extends Controller
{
    private $db;
    private $primaryKey = 'id';
    private $maxLimit = 20;
    private $result = [];

    /**
     * Model constructor.
     */
    public function __construct()
    {
        $env = (object)Env::get('database');
        $this->db = new PDO($env->driver . ':host=' . $env->hostname . ';dbname=' . $env->basename, $env->username, $env->password, [PDO::MYSQL_ATTR_INIT_COMMAND => 'SET NAMES \'UTF8\'']);
        $this->db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
    }

    /**
     * @param $table
     * @param null  $filter
     * @return array
     */
    protected function select($table, $filter = null)
    {
        /*echo "<pre>";
                var_dump($this->filterable);
                echo "</pre>";
        exit;*/

        $filter = implode(", ", $this->filterable);
        if (!empty($filter)) {
            $sql = "SELECT $filter FROM $table";
        } else {
            $sql = "SELECT * FROM $table";
        }


        $statement = $this->db->prepare($sql);
        //$statement->bindValue(':table', $table, PDO::PARAM_STR);
        $statement->execute();

        return $statement->fetchAll(PDO::FETCH_OBJ);
    }


    public function getPrimaryKey()
    {
        return $this->primaryKey;
    }

    public function getTable()
    {
        return $this->table;
    }

    /**
     * @return mixed
     */
    public function getFilterable()
    {
        return $this->filterable;
    }

    /**
     * @return mixed
     */
    public function getIncludable()
    {
        return $this->includable;
    }

    /**
     * @return mixed
     */
    public function getSearchable()
    {
        return $this->searchable;
    }

    /**
     * @return mixed
     */
    public function getHidden()
    {
        return $this->hidden;
    }

    /**
     * @return mixed
     */
    public function getFillable()
    {
        return $this->fillable;
    }

    /**
     * @return mixed
     */
    public function getSelectable()
    {
        return $this->selectable;
    }

    public function isSelectable($column)
    {
        return (array_search($column, $this->getSelectable()) !== false)
            and (array_search($column, $this->getHidden()) === false);
    }

}
