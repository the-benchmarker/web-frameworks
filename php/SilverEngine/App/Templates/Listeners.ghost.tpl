<?php echo '<?php'; ?>


namespace App\Listneners;

use App\Events\{{{$name}}};

/**
 * {{{$name}}} listnener
 */
class {{{$name}}}
{
    public function fire()
    {
        return "your code";
    }
}
