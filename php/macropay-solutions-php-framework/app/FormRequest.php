<?php

namespace App;

/**
 * Use this by uncommenting \App\Applicaton::fireResolvingCallbacks
 */
class FormRequest extends \MacropaySolutions\Kernel\Http\FormRequest
{
    use \MacropaySolutions\Framework\Http\RequestTrait;
    use RequestTrait;
}
