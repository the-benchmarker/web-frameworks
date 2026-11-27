<?php

namespace App\Models;

use MacropaySolutions\Framework\Auth\Authorizable;
use MacropaySolutions\Kernel\Auth\Authenticatable;
use MacropaySolutions\Kernel\Contracts\Auth\Access\Authorizable as AuthorizableContract;
use MacropaySolutions\Kernel\Contracts\Auth\Authenticatable as AuthenticatableContract;
use MacropaySolutions\Kernel\Database\Obvious\Model;

class User extends Model implements AuthenticatableContract, AuthorizableContract
{
    use Authenticatable;
    use Authorizable;

    /**
     * The attributes that are mass assignable.
     *
     * @var string[]
     */
    protected array $fillable = [
        'name',
        'email',
    ];

    /**
     * The attributes excluded from the model's JSON form.
     *
     * @var string[]
     */
    protected array $hidden = [
        'password',
    ];
}
