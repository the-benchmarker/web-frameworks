<?php echo '<?php'; ?>


namespace App\Facades;

use Silver\Support\Facade;


/**
 * {{{$name}}} event provider
 */
class {{{$name}}} extends Facade
{

    protected static function getClass()
    {
        return 'App\Helpers\{{{$name}}}';
    }

}
