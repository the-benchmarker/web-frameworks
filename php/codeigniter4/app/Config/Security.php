<?php

namespace Config;

use CodeIgniter\Config\BaseConfig;

class Security extends BaseConfig
{
	/**
	 * --------------------------------------------------------------------------
	 * CSRF Token Name
	 * --------------------------------------------------------------------------
	 *
	 * Token name for Cross Site Request Forgery protection cookie.
	 *
	 * @var string
	 */
	public $tokenName = 'csrf_test_name';

	/**
	 * --------------------------------------------------------------------------
	 * CSRF Header Name
	 * --------------------------------------------------------------------------
	 *
	 * Token name for Cross Site Request Forgery protection cookie.
	 *
	 * @var string
	 */
	public $headerName = 'X-CSRF-TOKEN';

	/**
	 * --------------------------------------------------------------------------
	 * CSRF Cookie Name
	 * --------------------------------------------------------------------------
	 *
	 * Cookie name for Cross Site Request Forgery protection cookie.
	 *
	 * @var string
	 */
	 public $cookieName = 'csrf_cookie_name';

	/**
	 * --------------------------------------------------------------------------
	 * CSRF Expires
	 * --------------------------------------------------------------------------
	 *
	 * Expiration time for Cross Site Request Forgery protection cookie.
	 *
	 * Defaults to two hours (in seconds).
	 *
	 * @var integer
	 */
	public $expires = 7200;

	/**
	 * --------------------------------------------------------------------------
	 * CSRF Regenerate
	 * --------------------------------------------------------------------------
	 *
	 * Regenerate CSRF Token on every request.
	 *
	 * @var boolean
	 */
	public $regenerate = true;

	/**
	 * --------------------------------------------------------------------------
	 * CSRF Redirect
	 * --------------------------------------------------------------------------
	 *
	 * Redirect to previous page with error on failure.
	 *
	 * @var boolean
	 */
	public $redirect = true;

	/**
	 * --------------------------------------------------------------------------
	 * CSRF SameSite
	 * --------------------------------------------------------------------------
	 *
	 * Setting for CSRF SameSite cookie token.
	 *
	 * Allowed values are: None - Lax - Strict - ''.
	 *
	 * Defaults to `Lax` as recommended in this link:
	 *
	 * @see https://portswigger.net/web-security/csrf/samesite-cookies
	 *
	 * @var string
	 *
	 * @deprecated
	 */
	public $samesite = 'Lax';
}
