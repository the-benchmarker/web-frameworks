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

namespace Silver\Core;

use Silver\Core\Bootstrap\Facades\Log;
use Silver\Core\Bootstrap\Facades\Request;
use Silver\Exception\NotFoundException;
use Silver\Exception\ErrorException;
use Silver\Exception\Exception;
use Silver\Http\Response;
use Silver\Http\View;
use Silver\Core\Env;

class ErrorHandler
{
    // Filter for php errors
    private static $filter = E_ALL;

    public static function setFilter($filter)
    {
        self::$filter = $filter;
    }

    public static function getFilter()
    {
        return self::$filter;
    }

    public static function withFilter($filter, $cb)
    {
        $old = self::getFilter();
        self::setFilter($filter);
        $rv = $cb();
        self::setFilter($old);

        return $rv;
    }

    public static function handle_error($code, $message, $file, $line)
    {
        self::resetCWD();
        if ($code & self::$filter) {
            $ex = new ErrorException($message, $code);
            $ex->setFile($file);
            $ex->setLine($line);
            throw $ex;
        }
    }

    public static function handle_fatal()
    {
        self::resetCWD();
        if ($fatal = error_get_last()) {
            $ex = new Exception($fatal['message']);
            $ex->setFile($fatal['file']);
            $ex->setLine($fatal['line']);
            // throw $ex;
            self::handle_ex($ex);
        }
    }

    public static function handle_ex($ex)
    {
        self::resetCWD();
        self::render($ex, true);
    }

    private static function code_around(Exception $ex, $around = 3)
    {
        $file = $ex->getFile();
        $line = $ex->getLine();

        if (file_exists($file)) {
            return implode("\n", array_slice(FILE($file), $line - $around, $around * 2 + 1));
        } else {
            return "Not a file: '" . print_r($file, true) . "'";
        }
    }

    public static function render(Exception $e, $finalize = false)
    {
        $view = null;

        if (Request::segment(1) == 'api' || Request::segment(1) == 'public' && Request::segment(2) == 'api') {
            try {
                //                dd(1111);

                if ($e instanceof NotFoundException) {
                    //                    dd(1111);

                    //                    dd(2222);
                    $payload = [
                        'data' => [
                            'message' => $e->getMessage(),
                            'code'    => $e->getCode() ?: '404',
                            'debug'   => [
                                'file' => $e->getFile(),
                                'line' => $e->getLine(),
                            ]
                        ],
                    ];
                    header('Content-type: Application/json');
                    echo json_encode($payload);
                    exit();
                } else if($e instanceof Exception) {
                    //                    dd(1111);

                    header('Content-type: Application/json');

                    $payload = [
                        'data' => [
                            'message' => $e->getMessage(),
                            'code' => 500,
                            'file' => $e->getFile(),
                            'on line' => $e->getLine(),
                            'trace' => $e->getTrace(),
                    //                            'trace2' => $e->getTraceAsString(),
                        ],
                    ];

                    //                    dd($payload);

                    echo json_encode($payload);
                    exit();
                }
            } catch (Exception $e) {
                //                dd(66666);
                self::finalize("Fatal error: " . $e->getMessage());
            }
        } else {
            try {
                // FIXMEE: API - isDebug() -> hide details
                if ($e instanceof NotFoundException) {
                    $view = View::make('errors.404')
                        ->with('message', $e->getMessage())
                        ->with('debug', self::isDebug());
                } else {
                    $view = View::make('errors.500')
                        ->with('message', $e->getMessage())
                        ->with('file', $e->getFile())
                        ->with('line', $e->getLine())
                        ->with('code_around', self::code_around($e))
                        ->with('back_trace', $e->getTrace())
                        ->with('debug', self::isDebug());
                }

                if ($finalize) {
                    self::finalize($view);
                } else {
                    return $view;
                }
            } catch (Exception $e) {
                self::finalize("Fatal error: " . $e->getMessage());
            }
        }

        //        try {
        //            // FIXMEE: API - isDebug() -> hide details
        //            if ($e instanceof NotFoundException) {
        //                $view = View::make('errors.404')
        //                    ->with('message', $e->getMessage())
        //                    ->with('debug', self::isDebug());
        //            } else {
        //                $view = View::make('errors.500')
        //                    ->with('message', $e->getMessage())
        //                    ->with('file', $e->getFile())
        //                    ->with('line', $e->getLine())
        //                    ->with('code_around', self::code_around($e))
        //                    ->with('back_trace', $e->getTrace())
        //                    ->with('debug', self::isDebug());
        //            }
        //
        //            if ($finalize) {
        //                self::finalize($view);
        //            } else {
        //                return $view;
        //            }
        //        } catch (Exception $e) {
        //            self::finalize("Fatal error: " . $e->getMessage());
        //        }
    }

    private static function finalize($content)
    {
        http_response_code(500);

        if ($content instanceof View) {
            echo $content->render();
        } else {
            echo $content;
        }
        exit;
    }

    private static function isDebug()
    {
        return Env::get('debug', false);
    }

    private static function resetCWD()
    {
        chdir(ROOT);
    }
}

set_error_handler("Silver\\Core\\ErrorHandler::handle_error", E_ALL);
set_exception_handler("Silver\\Core\\ErrorHandler::handle_ex");
register_shutdown_function("Silver\\Core\\ErrorHandler::handle_fatal");
ErrorHandler::setFilter(E_ALL);
error_reporting(~E_ALL);
