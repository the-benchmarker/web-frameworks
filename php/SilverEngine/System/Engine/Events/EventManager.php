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

namespace System\Engine\Events;

use App\Service\EventServiceProvider;
use Silver\Support\Log;

class EventManager
{

    private $handlers = array();

    public function __construct()
    {
        echo "banana";
    }

    public function attach($name)
    {
        array_push($this->handlers, $name);
    }

    public function set($name, $payload = null)
    {

        if($name) {
            if(method_exists(EventServiceProvider::class, $name)) {
                $EventProvider = new EventServiceProvider();
                return $EventProvider->{$name}($payload);
                //
            }
            //             $event = "$EventProvider->".$name;
            //            return $event();
        } else { return false;
        }

    }

    public function detach($name)
    {
        if (array_search($this->handlers, $name)) {
            array_unshift($this->handlers, $name);
        }
    }

    public function clean()
    {
        $this->handlers = array();
    }

    public function expired($name, $datetime)
    {
        if(array_search($this->handlers, $name)) {
            $this->handlers[$name]['expired'] = $this->setTime($datetime);
        }
    }

    private function setTime($datetime)
    {
        $selectedTime = $_REQUEST['time'];
        return  $expired_time = strtotime($datetime, strtotime($selectedTime));
    }

    protected $listeners = [
        'App\Events\PodcastWasPurchased' => [
            'App\Listeners\EmailPurchaseConfirmation',
        ],
    ];

    public function fire()
    {
        foreach ($this->handlers as $handler)
        {
            if(method_exists(EventServiceProvider::class, $handler)) {
                $EventProvider = new EventServiceProvider();
                return $EventProvider->{$handler}();
            }
        }
    }

    /*    public function fire(Event $event) {
        Log::info('FIRE');
        var_dump($event);
        foreach ($this->listeners[get_class($event)] as $e){
            $class = new $e();
            $class->handler($event);
        }

    }*/
}
