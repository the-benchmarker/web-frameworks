<?php echo '<?php'; ?>

namespace App\Controllers;

use Silver\Core\Controller;
use Silver\Http\View;

/**
* {{{$name}}} controller
*/
class {{{ucfirst($name)}}}Controller extends Controller
{

    protected $name = "{{$name}}";

    public function get()
    {
        //echo "Welcome in {{$name}} controller. This file is on App/Controllers/";
        return View::make('{{$name}}')->with('name', $this->name);
    }

    public function post()
    {
        echo 'Method: post';
    }

    public function put()
    {
        echo 'Method: put';
    }

    public function delete()
    {
        echo 'Method: delete';
    }
}
