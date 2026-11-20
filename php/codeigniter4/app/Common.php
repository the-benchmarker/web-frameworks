<?php

/**
 * The goal of this file is to allow developers a location
 * where they can overwrite core procedural functions and
 * replace them with their own. This file is loaded during
 * the bootstrap process and is called during the frameworks
 * execution.
 *
 * This can be looked at as a `master helper` file that is
 * loaded early on, and may also contain additional functions
 * that you'd like to use throughout your entire application
 *
 * @link: https://codeigniter4.github.io/CodeIgniter4/
 */
function is_cli(): bool
{
    if (PHP_SAPI === 'cli') {
        return true;
    }

    if (defined('STDIN')) {
        return true;
    }

    if (! isset($_SERVER['REMOTE_ADDR'], $_SERVER['HTTP_USER_AGENT']) && isset($_SERVER['argv']) && count($_SERVER['argv']) > 0) {
        return true;
    }

    // if source of request is from CLI, the `$_SERVER` array will not populate this key
    return ! isset($_SERVER['REQUEST_METHOD']);
}
