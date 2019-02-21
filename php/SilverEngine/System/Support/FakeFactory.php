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

namespace Silver\Support;

use Silver\Database\DB;
use Silver\Http\Response;

class FakeFactory
{

    private $firstnames = array(
        'Christopher',
        'Ryan',
        'Ethan',
        'John',
        'Zoey',
        'Sarah',
        'Michelle',
        'Samantha',
        'Stive',
        'Arthur',
        'George',
        'Max',
        'Graham',
        'Peter',
        'Michel',
        'Richard',
        'Glan',
        'John',
        'Mike',
    );

    private $lastnames = array(
        'Walker',
        'Thompson',
        'Anderson',
        'Johnson',
        'Tremblay',
        'Peltier',
        'Cunningham',
        'Simpson',
        'Mercado',
        'Sellers',
        'Smith',
        'Bloom',
        'Gaddis',
        'Miller',
        'Racham',
        'Gates',
        'Jobs',
    );

    private $domain = array(
        'freshc',
        'local',
        'localhost',
        'develop',
        'bingox',
        'fmd',
        'atr32',
    );

    private $domain_ext = array(
        'com',
        'net',
        'io',
        'org',
        'me',
        'co.uk',
        'si',
        'us',
    );

    private $data_ = [];

    /**
     * @param int    $num
     * @param string $users
     *
     * @return bool
     */
    public function users($num = 1, $users = 'users')
    {
        for ($i = 1; $i <= $num; $i++) {
            $data = [
                'username' => $this->fullname(),
                'email' => $this->email(),
                'password' => $this->password(),
            ];

            DB::insert($users, $data);

        }
        return false;
    }

    /**
     * @return mixed
     */
    public function firstname()
    {
        return $this->firstnames[mt_rand(0, sizeof($this->firstnames) - 1)];
    }

    /**
     * @return mixed
     */
    public function lastname()
    {
        return $this->lastnames[mt_rand(0, sizeof($this->lastnames) - 1)];
    }

    /**
     * @return string
     */
    public function fullname()
    {
        return $this->firstnames[mt_rand(0, sizeof($this->firstnames) - 1)] . ' ' . $this->lastnames[mt_rand(0, sizeof($this->lastnames) - 1)];
    }

    /**
     * @return string
     */
    public function domain_gen()
    {
        return $this->domain[mt_rand(0, sizeof($this->domain) - 1)] . '.' . $this->domain_ext[mt_rand(0, sizeof($this->domain_ext) - 1)];
    }

    /**
     * @return string
     */
    public function email()
    {
        return $this->firstnames[mt_rand(0, sizeof($this->firstnames) - 1)] . '.'
        . $this->lastnames[mt_rand(0, sizeof($this->lastnames) - 1)] . '@' . $this->domain_gen();
    }

    public function password()
    {
        return md5($this->firstnames[mt_rand(0, sizeof($this->firstnames) - 1)]);
    }

    /**
     * @return string
     */
    public function number()
    {
        return $this->generateRandomNumber(3) . '-' . $this->generateRandomNumber(3) . '-' . $this->generateRandomNumber(3);
    }

    /**
     * @param int $length
     *
     * @return string
     */
    private function generateRandomNumber($length = 9)
    {
        return substr(str_shuffle(str_repeat($x = '0123456789', ceil($length / strlen($x)))), 1, $length);
    }

    /**
     * @param int $length
     *
     * @return string
     */
    private function generateRandomString($length = 10)
    {
        return substr(str_shuffle(str_repeat($x = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ', ceil($length / strlen($x)))), 1, $length);
    }
}