<?php

namespace Silver\Database;


class Relation
{
    private $local_model;
    private $alias;
    private $local_id;
    private $remote_model;
    private $remote_id;
    private $through;
    private $wheres = [];

    public function __construct($local) 
    {
        $this->localModel($local);
    }

    public function hasOne($model, $local_id, $remote_id = null) 
    {
        return $this->remoteModel($model)
            ->local($local_id)
            ->remote($remote_id);
    }

    public function hasMany($model, $remote_id, $local_id = null) 
    {
        return $this->remoteModel($model)
            ->remote($remote_id)
            ->local($local_id);
    }

    public function through($local_id, $through_table, $remote_id) 
    {
        $this->through = [$local_id, $through_table, $remote_id];
        return $this;
    }

    public function local($id) 
    {
        $this->local_id = $id;
        return $this;
    }

    public function remote($id) 
    {
        $this->remote_id = $id;
        return $this;
    }

    public function localModel($model) 
    {
        $this->local_model = $model;
        return $this;
    }

    public function remoteModel($model) 
    {
        $this->remote_model = $model;
        return $this;
    }

    public function alias($alias) 
    {
        $this->alias = $alias;
        return $this;
    }

    // XXX: unused
    public function getReferences() 
    {
        // Incomplete reference exception
        if ($this->through) {
            // return 2 references
        } else {
            // return one reference
        }
    }

    // Output:
    
    public function getTable() 
    {
        $rm = $this->remote_model;
        $alias = Parts\Name::ensure($this->alias);
        return new Parts\Table($rm::tableName(), $alias);
    }

    public function getJoinCondition() 
    {
        $lm = $this->local_model;
        $rm = $this->remote_model;

        $lt =    Parts\Name::ensure($lm::tableName());
        $lid =   Parts\Name::ensure($this->local_id ?: $lm::primaryKey());
        $rid =   Parts\Name::ensure($this->remote_id ?: $rm::primaryKey());
        $alias = Parts\Name::ensure($this->alias);

        return new Parts\JoinCondition([$lt, $lid], '=', [$alias, $rid]);
    }

    public function makeXXXJoin($alias) 
    {


        $rt =    Parts\Name::ensure($rm::tableName());


        return "LEFT JOIN $rt as $alias ON ($lt.$lid = $alias.$rid)";
    }
}