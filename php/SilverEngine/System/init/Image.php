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

class Image
{

    public static function pull()
    {
        $ch = curl_init();
        curl_setopt($ch, CURLOPT_URL, URL.'/public/images.zip');
        $fp = fopen('System/init/images/se_images.zip', 'w+');
        curl_setopt($ch, CURLOPT_FILE, $fp);
        curl_exec($ch);
        curl_close($ch);
        fclose($fp);

        return false;
    }

    public static function unzip()
    {
        $zip = new \ZipArchive;
        if ($zip->open(getcwd() ."/System/init/images/se_images.zip")) {
            $path = getcwd() . "/System/";
            $path = str_replace("\\", "/", $path);
            $zip->extractTo($path);
            $zip->close();
            return false;
        } else {
            echo "Error";
        }
    }

    public static function archive()
    {

        $rootPath = realpath('System');
        $date =  date('Y_m_d_H_i_s');

        $zip = new \ZipArchive();
        $zip->open('System/init/backup/'.$date.'-image.zip', \ZipArchive::CREATE | \ZipArchive::OVERWRITE);

        /**
 * @var SplFileInfo[] $files 
*/
        $files = new \RecursiveIteratorIterator(
            new \RecursiveDirectoryIterator($rootPath),
            \RecursiveIteratorIterator::LEAVES_ONLY
        );

        foreach ($files as $name => $file) {

            if (!$file->isDir()) {
                $filePath = $file->getRealPath();
                $relativePath = substr($filePath, strlen($rootPath) + 1);

                $zip->addFile($filePath, $relativePath);
            }
        }

        // Zip archive will be created only after closing object
        $zip->close();
    }

}