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

namespace System\App\Controllers;

use Silver\Core\Controller;
use Silver\init\Image;

class SystemController extends Controller
{
    public function pull()
    {
        echo " - System update framework start<br>";
        Image::archive();
        sleep(5);
        echo " - Archive complete<br>";
        Image::pull();
        sleep(2);
        echo " - Pulled 100% complete<br>";
        sleep(2);
        Image::unzip();
        echo " - Update framework complete";
    }
}
