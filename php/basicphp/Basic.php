<?php

/**
 * BasicPHP - A frameworkless library-based approach for building web applications
 *          - and application programming interfaces or API's.
 *          - The aim of the project is for developers to build applications that
 *          - are framework-independent using native PHP functions and API's.
 *          -
 *          - To embed the application to any framework, copy BasicPHP class library
 *          - (Basic.php), and the 'classes', 'models', 'views' and 'controllers'
 *          - folders one (1) folder above the front controller (index.php) of the
 *          - chosen framework. In the controller file, at the top of the script,
 *          - include/require Basic.php.
 *
 * @package  BasicPHP
 * @version  v0.9.9
 * @author   Raymund John Ang <raymund@open-nis.org>
 * @license  MIT License
 */

class Basic
{

	/*
	|--------------------------------------------------------------------------
	| FUNCTIONS
	|--------------------------------------------------------------------------
	*/

	/**
	 * Get URI segment value
	 *
	 * @param int $order    - URI segment position from base URL
	 *                      - Basic::segment(1) as first URI segment
	 * @return string|false - URI segment string or error
	 */

	public static function segment($order)
	{
		$uri = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
		$uri = explode('/', $uri);

		// Number of subdirectories from hostname to index.php
		$sub_dir = substr_count($_SERVER['SCRIPT_NAME'], '/') - 1;

		if (! isset($uri[$order+$sub_dir])) return FALSE;

		return $uri[$order+$sub_dir];
	}

	/**
	 * Controller or callable-based endpoint routing
	 *
	 * @param string $http_method           - HTTP method (e.g. 'ANY', 'GET', 'POST', 'PUT', 'DELETE')
	 * @param string $path                  - URL path in the format '/url/path'
	 *                                      - Wildcard convention from CodeIgniter
	 *                                      - (:num) for number and (:any) for string
	 * @param string|callable $class_method - 'ClassController@method' format or callable function
	 */

	public static function route($http_method, $path, $class_method)
	{
		if ($http_method === 'ANY') $http_method = $_SERVER['REQUEST_METHOD']; // Any HTTP Method

		if ($_SERVER['REQUEST_METHOD'] === $http_method) {

			// Convert '/' and wilcards (:num) and (:any) to RegEx
			$pattern = str_ireplace('/', '\/', $path);
			$pattern = str_ireplace('(:num)', '[0-9]+', $pattern);
			$pattern = str_ireplace('(:any)', '[^\/]+', $pattern);
					
			// Check for subfolders from DocumentRoot and include in endpoint
			$sub = explode('/', dirname($_SERVER['SCRIPT_NAME']));
			$subfolder = (! empty($sub[1])) ? implode('\/', $sub) : '';

			$uri = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
			if ( preg_match('/^' . $subfolder . $pattern . '+$/i', $uri) )  {
				if (is_string($class_method)) {
					if (strstr($class_method, '@')) {
						list($class, $method) = explode('@', $class_method);

						$object = new $class();
						$object->$method();
						exit;
					}
				} elseif (is_callable($class_method)) {
					$class_method();
					exit;
				}

			}

		}
	}

	/**
	 * Render view with data
	 *
	 * @param string $view - View file inside 'views' folder (exclude .php extension)
	 * @param array $data  - Data in array format
	 */

	public static function view($view, $data=NULL)
	{
		$file = '../views/' . $view . '.php';
		if (! empty($data)) extract($data); // Convert array keys to variables
		if (file_exists($file) && is_readable($file) && pathinfo($file)['extension'] === 'php') require_once $file; // Render page view
	}

	/**
	 * HTTP API request call using cURL
	 *
	 * @param string $http_method - HTTP request method (e.g. 'GET', 'POST')
	 * @param string $url         - URL of API endpoint
	 * @param array $data         - Request body in array format
	 * @param string $user_token  - Basic 'username:password' or Bearer token
	 *
	 * @return (int|string)[]     - HTTP response code and result of cURL execution
	 */

	public static function apiCall($http_method, $url, $data=NULL, $user_token=NULL)
	{
		if ( substr( strtolower( trim($url) ), 0, 16) !== 'http://localhost' && substr( strtolower( trim($url) ), 0, 8) !== 'https://' ) self::apiResponse(400, 'API URL should start with "https://".'); // Require HTTPS API URL

		$auth_scheme = ( stristr($user_token, ':') ) ? 'Basic' : 'Bearer'; // Authorization scheme
		$auth_cred = ( $auth_scheme === 'Basic' ) ? base64_encode($user_token) : $user_token; // Credentials
		$content_type = ( is_array($data) ) ? 'application/json' : 'text/plain'; // Content Type
		$data = ( is_array($data) ) ? json_encode($data) : $data; // Data array to JSON
		$data = ( is_object($data) ) ? json_encode($data) : $data; // Data object to JSON

		$ch = curl_init(); // Initialize cURL

		// Set cURL options
		curl_setopt($ch, CURLOPT_URL, $url);
		curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $http_method);
		curl_setopt($ch, CURLOPT_POSTFIELDS, $data);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
		curl_setopt($ch, CURLOPT_SSL_VERIFYHOST, FALSE);
		curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, FALSE);
		curl_setopt($ch, CURLOPT_HTTPHEADER,
			array(
				"Authorization: $auth_scheme $auth_cred",
				"Content-Type: $content_type",
				'Content-Length: ' . strlen($data)
			)
		);

		$result = curl_exec($ch); // Execute cURL
		$http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE); // HTTP response code
		curl_close ($ch); // Close cURL connection

		return ['code' => $http_code, 'data' => $result];
	}

	/**
	 * Handle HTTP API response
	 *
	 * @param integer $code        - HTTP response code
	 * @param string $data         - Data to transmit
	 * @param string $content_type - Header: Content-Type
	 */

	public static function apiResponse($code, $data=NULL, $content_type='text/plain')
	{
		$data = ( is_array($data) ) ? json_encode($data) : $data; // Data array to JSON
		$data = ( is_object($data) ) ? json_encode($data) : $data; // Data object to JSON
		
		if ($code > 199 && $code < 300) $message = 'OK'; // OK response
		if ($code < 200 || $code > 299) $message = $data; // If no data, $data = $message

		header($_SERVER['SERVER_PROTOCOL'] . ' ' . $code . ' ' . $message); // Set HTTP response code and message
		header('Content-Type: ' . $content_type);
		exit($data); // Data in string format
	}

	/**
	 * Base URL - Templating
	 *
	 * @return string - Base URL
	 */

	public static function baseUrl()
	{
		$http_protocol = (isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] === 'on') ? 'https://' : 'http://';
		$subfolder = (! empty(dirname($_SERVER['SCRIPT_NAME']))) ? dirname($_SERVER['SCRIPT_NAME']) : '';

		return $http_protocol . $_SERVER['SERVER_NAME'] . $subfolder . '/';
	}

	/**
	 * Prevent Cross-Site Request Forgery (CSRF)
	 * Create a per request token to handle CSRF using sessions
	 * Basic::setFirewall() should be executed. $verify_csrf_token = TRUE (default)
	 */

	public static function csrfToken()
	{
		if (defined('VERIFY_CSRF_TOKEN') && VERIFY_CSRF_TOKEN) {
			$_SESSION['csrf-token'] = bin2hex( random_bytes(32) );
			return $_SESSION['csrf-token'];
		}
	}

	/**
	 * Encrypt data using AES GCM, CTR-HMAC or CBC-HMAC
	 *
	 * @param string $plaintext   - Plaintext to be encrypted
	 * @param string $pass_phrase - Passphrase or encryption API URL
	 * @param string $header      - Encryption token version or JWE header
	 * @param string $cipher      - Cipher method
	 * @param string $hmac_algo   - HMAC algorithm
	 *
	 * @return string             - Encryption token with base64-encoded ciphertext
	 */

	public static function encrypt($plaintext=NULL, $pass_phrase=NULL, $header='encv1', $cipher='aes-256-gcm', $hmac_algo='sha512')
	{
		if (! isset($plaintext)) self::apiResponse(500, 'Set plaintext for encryption.');
		if (! isset($pass_phrase)) self::apiResponse(500, 'Set passphrase as a constant.');

		if ($cipher !== 'aes-256-gcm' && $cipher !== 'aes-256-ctr' && $cipher !== 'aes-256-cbc' && $cipher !== 'aes-128-gcm' && $cipher !== 'aes-128-ctr' && $cipher !== 'aes-128-cbc') self::apiResponse(500, "Encryption cipher method should either be 'aes-256-gcm', 'aes-256-ctr', 'aes-256-cbc', 'aes-128-gcm', 'aes-128-ctr' or 'aes-128-cbc'.");

		// Encryption - Version 1
		if (! function_exists('encrypt_v1')) {

			function encrypt_v1($plaintext, $pass_phrase, $header, $cipher, $hmac_algo) {

				$iv = random_bytes( openssl_cipher_iv_length($cipher) ); // Initialization Vector
				$salt = $iv; // Salt

				if ( filter_var($pass_phrase, FILTER_VALIDATE_URL) ) {
					$api = $pass_phrase . '?action=encrypt';
					$response = Basic::apiCall('POST', $api, ['key' => $pass_phrase]);

					if ($response['code'] !== 200) Basic::apiResponse($response['code']);
					
					$pass_phrase = bin2hex( random_bytes(32) ); // Random password
				}

				// Derive keys
				$masterKey = hash_pbkdf2('sha256', $pass_phrase, $salt, 10000); // Master key
				$encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Data Encryption key
				$hmacKey = hash_hkdf('sha256', $masterKey, 32, 'sha-256-authentication', $salt); // HMAC key

				if ($cipher === 'aes-256-gcm' || $cipher === 'aes-128-gcm') {

					$ciphertext = openssl_encrypt($plaintext, $cipher, $encKey, $options=0, $iv, $tag);
					$encrypted = $header . '.' . base64_encode($ciphertext) . '.' . base64_encode($tag) . '.' . base64_encode($salt);

					if ( isset($api) && $response['code'] === 200 ) {
						$response = Basic::apiCall('POST', $api, ['key' => $pass_phrase]);
						$data = json_decode($response['data'], TRUE);
						$dek_token = $data['key']; // Encrypted passphrase token

						return str_replace('=', '', $encrypted . '.' . $dek_token); // Strip off '='
					} else {
						return str_replace('=', '', $encrypted); // Strip off '='
					}

				} else {

					$ciphertext = openssl_encrypt($plaintext, $cipher, $encKey, $options=0, $iv);
					$hash = hash_hmac($hmac_algo, $ciphertext, $hmacKey);
					$encrypted = $header . '.' . base64_encode($ciphertext) . '.' . base64_encode($hash) . '.' . base64_encode($salt);

					if ( isset($api) && $response['code'] === 200 ) {
						$response = Basic::apiCall('POST', $api, ['key' => $pass_phrase]);
						$data = json_decode($response['data'], TRUE);
						$dek_token = $data['key'];

						return str_replace('=', '', $encrypted . '.' . $dek_token); // Strip off '='
					} else {
						return str_replace('=', '', $encrypted); // Strip off '='
					}

				}

			}

		}

		/** Version-based encryption */
		if ( substr( ltrim($plaintext), 0, 5 ) !== $header ) return encrypt_v1($plaintext, $pass_phrase, $header, $cipher, $hmac_algo);
		return $plaintext;
	}

	/**
	 * Decrypt data using AES GCM, CTR-HMAC or CBC-HMAC
	 *
	 * @param string $encrypted   - Encryption token with base64-encoded ciphertext
	 * @param string $pass_phrase - Passphrase or encryption API URL
	 * @param string $header      - Encryption token version or JWE header
	 * @param string $cipher      - Cipher method
	 * @param string $hmac_algo   - HMAC algorithm
	 *
	 * @return string             - Decrypted plaintext
	 */

	public static function decrypt($encrypted=NULL, $pass_phrase=NULL, $header='encv1', $cipher='aes-256-gcm', $hmac_algo='sha512')
	{
		if (! isset($encrypted)) self::apiResponse(500, 'Set encryption token for decryption.');
		if (! isset($pass_phrase)) self::apiResponse(500, 'Set passphrase as a constant.');

		if ($cipher !== 'aes-256-gcm' && $cipher !== 'aes-256-ctr' && $cipher !== 'aes-256-cbc' && $cipher !== 'aes-128-gcm' && $cipher !== 'aes-128-ctr' && $cipher !== 'aes-128-cbc') self::apiResponse(500, "Encryption cipher method should either be 'aes-256-gcm', 'aes-256-ctr', 'aes-256-cbc', 'aes-128-gcm', 'aes-128-ctr' or 'aes-128-cbc'.");

		// Decryption - Version 1
		if (! function_exists('decrypt_v1')) {

			function decrypt_v1($encrypted, $pass_phrase, $header, $cipher, $hmac_algo) {

				if ($cipher === 'aes-256-gcm' || $cipher === 'aes-128-gcm') {

					if ( filter_var($pass_phrase, FILTER_VALIDATE_URL) ) {
						$api = $pass_phrase . '?action=decrypt';
						$response = Basic::apiCall('POST', $api, ['key' => $pass_phrase]);

						if ($response['code'] !== 200) Basic::apiResponse($response['code']);

						list($header, $ciphertext, $tag, $salt, $header_dek, $ciphertext_dek, $tag_dek, $salt_dek) = explode('.', $encrypted);

						$ciphertext = base64_decode($ciphertext);
						$tag = base64_decode($tag);
						$salt = base64_decode($salt);
						$iv = $salt; // Initialization Vector
					} else {
						list($header, $ciphertext, $tag, $salt) = explode('.', $encrypted);

						$ciphertext = base64_decode($ciphertext);
						$tag = base64_decode($tag);
						$salt = base64_decode($salt);
						$iv = $salt; // Initialization Vector
					}

					if ( isset($api) && $response['code'] === 200 ) {
						$response = Basic::apiCall('POST', $api, ['key' => $header_dek . '.' . $ciphertext_dek . '.' . $tag_dek . '.' . $salt_dek]);
						$data = json_decode($response['data'], TRUE);
						$pass_phrase = $data['key']; // Decrypted random password
					}

					// Derive keys
					$masterKey = hash_pbkdf2('sha256', $pass_phrase, $salt, 10000); // Master key
					$encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Data Encryption key
					$hmacKey = hash_hkdf('sha256', $masterKey, 32, 'sha-256-authentication', $salt); // HMAC key

					$plaintext = openssl_decrypt($ciphertext, $cipher, $encKey, $options=0, $iv, $tag);

					// GCM authentication
					if ($plaintext !== FALSE) {
						return $plaintext;
					} else {
						exit ('Please verify authenticity of ciphertext.');
					}

				} else {

					if ( filter_var($pass_phrase, FILTER_VALIDATE_URL) ) {
						$api = $pass_phrase . '?action=decrypt';
						$response = Basic::apiCall('POST', $api, ['key' => $pass_phrase]);

						if ($response['code'] !== 200) Basic::apiResponse($response['code']);

						list($header, $ciphertext, $hash, $salt, $header_dek, $ciphertext_dek, $hash_dek, $salt_dek) = explode('.', $encrypted);

						$ciphertext = base64_decode($ciphertext);
						$hash = base64_decode($hash);
						$salt = base64_decode($salt);
						$iv = $salt; // Initialization Vector
					} else {
						list($header, $ciphertext, $hash, $salt) = explode('.', $encrypted);

						$ciphertext = base64_decode($ciphertext);
						$hash = base64_decode($hash);
						$salt = base64_decode($salt);
						$iv = $salt; // Initialization Vector
					}

					if ( isset($api) && $response['code'] === 200 ) {
						$response = Basic::apiCall('POST', $api, ['key' => $header_dek . '.' . $ciphertext_dek . '.' . $hash_dek . '.' . $salt_dek]);
						$data = json_decode($response['data'], TRUE);
						$pass_phrase = $data['key']; // Decrypted passphrase
					}

					// Derive keys
					$masterKey = hash_pbkdf2('sha256', $pass_phrase, $salt, 10000); // Master key
					$encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Encryption key
					$hmacKey = hash_hkdf('sha256', $masterKey, 32, 'sha-256-authentication', $salt); // HMAC key

					$digest = hash_hmac($hmac_algo, $ciphertext, $hmacKey);

					// HMAC authentication
					if  ( hash_equals($hash, $digest) ) {
						return openssl_decrypt($ciphertext, $cipher, $encKey, $options=0, $iv);
						}
					else {
						exit ('Please verify authenticity of ciphertext.');
					}

				}

			}

		}

		/** Version-based decryption */
		if ( substr( ltrim($encrypted), 0, 5 ) === $header ) return decrypt_v1($encrypted, $pass_phrase, $header, $cipher, $hmac_algo);
		if (! isset($encrypted) || empty($encrypted)) { return ''; } // Return empty if $encrypted is not set or empty.
		return $encrypted;
	}

	/*
	|--------------------------------------------------------------------------
	| MIDDLEWARE
	|--------------------------------------------------------------------------
	*/ 

	/**
	 * Error Reporting
	 * 
	 * @param boolean $boolean - TRUE or FALSE
	 */

	public static function setErrorReporting($boolean=TRUE)
	{
		if ($boolean === TRUE) {
			error_reporting(E_ALL);
		} elseif ($boolean === FALSE) {
			error_reporting(0);
		} else {
			self::apiResponse(500, 'Boolean parameter for Basic::setErrorReporting() can only be TRUE or FALSE.');
		}
	}

	/**
	 * JSON Request Body as $_POST - API Access
	 */

	public static function setJsonBodyAsPOST() {
		$body = file_get_contents('php://input');
		if ( ! empty($body) && is_array(json_decode($body, TRUE)) ) $_POST = json_decode($body, TRUE);
	}

	/**
	 * Web Application Firewall
	 * 
	 * @param array $ip_blacklist          - Blacklisted IP addresses
	 * @param boolean $verify_csrf_token   - Verify CSRF token
	 * @param boolean $post_auto_escape    - Automatically escape $_POST
	 * @param string $uri_whitelist        - Whitelisted URI RegEx characters
	 */

	public static function setFirewall($ip_blacklist=[], $verify_csrf_token=TRUE, $post_auto_escape=TRUE, $uri_whitelist='\w\/\.\-\_\?\=\&\:\$')
	{
		// Deny access from blacklisted IP addresses
		if (isset($_SERVER['REMOTE_ADDR']) && in_array($_SERVER['REMOTE_ADDR'], $ip_blacklist)) {
			self::apiResponse(403, 'You are not allowed to access the application using your IP address.');
		}

		// Verify CSRF token
		if ($verify_csrf_token === TRUE) {
			define('VERIFY_CSRF_TOKEN', TRUE); // Used for Basic::csrfToken()
			session_set_cookie_params(NULL, NULL, NULL, TRUE, TRUE); // Secure and Httponly
			session_start(); // Require sessions

			if (isset($_POST['csrf-token']) && isset($_SESSION['csrf-token']) && ! hash_equals($_POST['csrf-token'], $_SESSION['csrf-token'])) {
				self::apiResponse(400, 'Please check authenticity of CSRF token.');
			}
		}

		// Automatically escape $_POST values using htmlspecialchars()
		if ($post_auto_escape === TRUE && isset($_POST)) {
			foreach ($_POST as $key => $value) {
				$_POST[$key] = htmlspecialchars($value, ENT_QUOTES, 'UTF-8');
			}
		}

		// Allow only whitelisted URI characters
		if (! empty($uri_whitelist)) {

			$regex_array = str_replace('w', 'alphanumeric', $uri_whitelist);
			$regex_array = explode('\\', $regex_array);

			if (isset($_SERVER['REQUEST_URI']) && preg_match('/[^' . $uri_whitelist . ']/i', $_SERVER['REQUEST_URI'])) {
				header($_SERVER["SERVER_PROTOCOL"]." 400 Bad Request");
				exit('<p>The URI should only contain alphanumeric and GET request characters:</p><p><ul>' . implode('<li>', $regex_array) . '</ul></p>');
			}

		}

		// // Deny blacklisted $_POST characters. '\' is blacklisted by default.
		// if (! empty($post_blacklist)) {
		// 	$regex_array = explode('\\', $post_blacklist);

		// 	if (isset($_POST) && preg_match('/[' . $post_blacklist . '\\\]/i', implode('/', $_POST)) ) {
		// 		header($_SERVER["SERVER_PROTOCOL"] . ' 400 Bad Request');
		// 		exit('<p>Submitted data should NOT contain the following characters:</p><p><ul>' . implode('<li>', $regex_array) . '<li>\</ul></p>');
		// 	}
		// }
	}

	/**
	 * Force application to use TLS/HTTPS
	 */

	public static function setHttps()
	{
		if (! isset($_SERVER['HTTPS']) || $_SERVER['HTTPS'] !== 'on') {
			header('Location: https://' . $_SERVER['SERVER_NAME'] . $_SERVER['REQUEST_URI']);
			exit;
		}
	}

	/**
	 * Autoload Classes
	 * 
	 * @param array $classes - Array of folders to autoload classes
	 */

	public static function setAutoloadClass($classes)
	{
		define('AUTOLOADED_FOLDERS', $classes);
		spl_autoload_register(function ($class_name) {
			foreach (AUTOLOADED_FOLDERS as $folder) {
				if (file_exists('../' . $folder . '/' . $class_name . '.php') && is_readable('../' . $folder . '/' . $class_name . '.php')) {
					require_once '../' . $folder . '/' . $class_name . '.php';
				}
			}
		});
	}

	/**
	 * Automatic routing of Basic::segment(1) and (2) as class and method
	 *
	 * @param string $controller - Default controller suffix
	 * @param string $method     - Default method name
	 */

	public static function setAutoRoute($controller='Controller', $method='index')
	{
		$class = ucfirst(strtolower(self::segment(1))) . $controller;
		if (self::segment(2)) $method = strtolower(self::segment(2));

		if (class_exists($class)) {
			$object = new $class();
			if (method_exists($object, $method)) {
				$object->$method();
				exit;
			} else {
				self::apiResponse(404);
				exit;
			}
		}
	}

	/**
	 * Encryption API - Key-Encryption-Key (KEK)
	 * Credits: https://github.com/ray-ang/encryption-api
	 *
	 * @param string $pass_phrase	- KEK master key
	 */

	public static function apiEncrypt($pass_phrase) {
		/* Require POST method */
		if ($_SERVER['REQUEST_METHOD'] !== 'POST') {
			self::apiResponse(405, "Method should be 'POST'.");
			exit();
		}

		$body = file_get_contents('php://input'); // Request body

		/* Require request body (not enctype="multipart/form-data") */
		if ( empty($body) ) {
			self::apiResponse(400, 'The request should have a body, and must not be enctype="multipart/form-data".');
			exit();
		}

		/* Require request body to be in JSON format */
		$body_array = json_decode($body, TRUE); // Convert JSON body string into array

		if (! is_array($body_array)) {
			self::apiResponse(400, 'The request body should be in JSON format.');
			exit();
		}

		/* Require parameter "action" */
		if (! isset($_GET['action']) || empty($_GET['action'])) {
			self::apiResponse(400, 'Please set "action" parameter to either "encrypt" or "decrypt".');
			exit();
		}

		/* Execute Function */
		switch ($_GET['action']) {
			case 'encrypt':
				$data = array();
				foreach($body_array as $key => $value) {
					$data[$key] = self::encrypt($value, $pass_phrase);
				}
				echo json_encode($data);
				break;
			case 'decrypt':
				$data = array();
				foreach($body_array as $key => $value) {
					$data[$key] = self::decrypt($value, $pass_phrase);
				}
				echo json_encode($data);
				break;
			default:
				Basic::apiResponse(400, 'Please set "action" parameter to either "encrypt" or "decrypt".');
				exit();
		}
	}

	/**
	 * Remote Procedure Call (RPC) over HTTP
	 *
	 * @param string $action     - RPC GET parameter
	 * @param string $controller - Default controller suffix
	 */

	public static function setHttpRpc($action='action', $controller='controller')
	{
		if (empty($_GET[$action])) self::apiResponse(400, "GET parameter '$action' should be set.");
		if ( substr(trim($_GET[$action]), 0, 1) === '.' ) self::apiResponse(400, "GET parameter '$action' should not start with a period (.) .");
		if (substr_count($_GET[$action], '.') < 1) self::apiResponse(400, "GET parameter '$action' should contain a period (.) to separate class and method.");
		if (substr_count($_GET[$action], '.') > 1) self::apiResponse(400, "GET parameter '$action' should only contain one period (.) .");

		list($class, $method) = explode('.', $_GET[$action]);
		$class = ucfirst(strtolower($class)) . $controller;
		$method = strtolower($method);

		if (class_exists($class)) {
			$object = new $class();
			if (method_exists($object, $method)) {
				$object->$method();
				exit;
			} else {
				self::apiResponse(404);
				exit;
			}
		} else {
			self::apiResponse(404);
			exit;
		}
	}

	/**
	 * JSON-RPC v2.0 middleware with request Method member as 'class.method'
	 * 
	 * @param string $controller - Default controller suffix
	 */

	public static function setJsonRpc($controller='Controller')
	{
		$body = file_get_contents('php://input'); // Request body
		$array = json_decode($body, TRUE); // JSON body to array

		header('Content-Type: application/json'); // Set content type as JSON

		if ( $_SERVER['REQUEST_METHOD'] !== 'GET' && $_SERVER['REQUEST_METHOD'] !== 'POST' ) exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32601, 'message' => 'Only GET and POST methods allowed.'], 'id' => NULL])); // Only GET and POST

		if ( $_SERVER['HTTP_CONTENT_TYPE'] !== 'application/json' ) exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32700, 'message' => "Request content type should be 'application/json'."], 'id' => NULL])); // Accept only JSON request content type

		if (! $body) exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32700, 'message' => 'Request should have a request body.'], 'id' => NULL])); // Require request body

		if ($body && ! $array) exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32700, 'message' => 'Provide request body data in valid JSON format.'], 'id' => NULL])); // Require valid JSON

		if ( strpos(ltrim($body), '[') === 0 ) exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32700, 'message' => 'Batch processing not supported at this time.'], 'id' => NULL])); // No batch processing

		if (! isset($array['jsonrpc']) || $array['jsonrpc'] !== '2.0') exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32600, 'message' => "JSON-RPC 'version' member should be set, and assigned a value of '2.0'."], 'id' => NULL])); // JSON-RPC (version) member

		if (! isset($array['method']) || ! strstr($array['method'], '.')) exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32600, 'message' => "JSON-RPC 'method' member should be set with the format 'class.method'."], 'id' => NULL])); // Method member

		list($class, $method) = explode('.', $array['method']); // Method member as 'class.method'
		$class = $class . $controller; // Default controller suffix

		// If class exists
		if (class_exists($class)) {
			if (! isset($array['id'])) exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32600, 'message' => "JSON-RPC 'id' member should be set."], 'id' => NULL])); // Require ID member

			$object = new $class();
			if (method_exists($object, $method)) {
				$object->$method();
				exit;
			} else {
				exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32601, 'message' => 'Method not found.'], 'id' => NULL]));
			}
		} else {
			exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32601, 'message' => 'Class not found.'], 'id' => NULL]));
		}
	}

}