<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * @package app
 */
/**
 * The current environment name.
 */
define('ENVIRONMENT', $_SERVER['ENVIRONMENT'] ?? 'production');
/**
 * True if it is in development environment, otherwise false.
 */
define('IS_DEV', ENVIRONMENT === 'development');
/**
 * Path to the root directory.
 */
define('ROOT_DIR', dirname(__DIR__) . \DIRECTORY_SEPARATOR);
/**
 * Path to the app directory.
 */
define('APP_DIR', ROOT_DIR . 'app' . \DIRECTORY_SEPARATOR);
/**
 * Path to the bin directory.
 */
define('BIN_DIR', ROOT_DIR . 'bin' . \DIRECTORY_SEPARATOR);
/**
 * Path to the boot directory.
 */
define('BOOT_DIR', ROOT_DIR . 'boot' . \DIRECTORY_SEPARATOR);
/**
 * Path to the config directory.
 */
define('CONFIG_DIR', ROOT_DIR . 'config' . \DIRECTORY_SEPARATOR);
/**
 * Path to the public directory.
 */
define('PUBLIC_DIR', ROOT_DIR . 'public' . \DIRECTORY_SEPARATOR);
/**
 * Path to the storage directory.
 */
define('STORAGE_DIR', ROOT_DIR . 'storage' . \DIRECTORY_SEPARATOR);
/**
 * Path to the vendor directory.
 */
define('VENDOR_DIR', ROOT_DIR . 'vendor' . \DIRECTORY_SEPARATOR);
/**
 * Path to the aplus directory.
 */
define('APLUS_DIR', VENDOR_DIR . 'aplus' . \DIRECTORY_SEPARATOR);
