<?php declare(strict_types=1);

namespace App\Validator;

use App\Annotation\Mapping\AlphaDash;
use Swoft\Validator\Annotation\Mapping\IsInt;
use Swoft\Validator\Annotation\Mapping\IsString;
use Swoft\Validator\Annotation\Mapping\Validator;

/**
 * Class TestValidator
 *
 * @since 2.0
 *
 * @Validator(name="TestValidator")
 */
class TestValidator
{
    /**
     * @IsString()
     *
     * @var string
     */
    protected $name = 'defualtName';

    /**
     * @IsInt(message="type must Integer")
     *
     * @var int
     */
    protected $type;

    /**
     * @IsString()
     * @AlphaDash(message="Passwords can only be alphabet, numbers, dashes, underscores")
     *
     * @var string
     */
    protected $password;
}
