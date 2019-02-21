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

namespace Silver\init;


class Silver
{

    public function run()
    {
        $result = Image::pull();
        if (!$result) {
            throw new Exception('Download error...');
        }
    }

}