<?php

class Basic
{

	/*
	|--------------------------------------------------------------------------
	| BasicPHP Library
	|--------------------------------------------------------------------------
	|
	| segment()          - retrieves the URL path substring separated by '/'
	| homepage()         - render hompage
	| error404()         - Handle Error 404 - Page Not Found - Invalid URI
	| json_rpc()         - Configure application for JSON-RPC v2.0 protocol.
	| route_auto()       - automatic routing of URL path to Class and method
	| route()            - routes URL path request to closure or Controller
	| view()             - passes data and renders the View
	| api_response()     - handles API response
	| api_call()         - handles API call
	| firewall()         - web application firewall
	| force_ssl()        - force application to use SSL
	| esc()              - uses htmlspecialchars() to prevent XSS
	| csrf_token()       - uses sessions to create per request CSRF token
	| encrypt()          - encrypt data using AES-256 and HMAC
	| decrypt()          - decrypt data using AES-256 and HMAC
	|
	*/

	/**
	 * Get URL path string value after the BASE_URL.
	 *
	 * @param integer $order - URL substring position from the BASE_URL
	 *                       - segment(1) as first string after BASE_URL
	 */

	public static function segment($order)
	{
		if (isset($_SERVER['REQUEST_URI'])) {
			$url_path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
			$url = explode('/', $url_path);
		}

		// Number of subdirectories from hostname to index.php
		$sub_dir = substr_count($_SERVER['SCRIPT_NAME'], '/') - 1;

		if ( isset($url[$order+$sub_dir]) || ! empty($url[$order+$sub_dir]) ) {
			return $url[$order+$sub_dir];
		} else {
			return FALSE;
		}
	}

	/**
	 * Render Homepage
	 */

	public static function homepage()
	{
		if ( empty(self::segment(1)) ) {
			list($class, $method) = explode('@', HOME_PAGE);
			$object = new $class();

			$object->$method();
			exit();
		}
	}

	/**
	 * Handle Error 404 - Page Not Found - Invalid URI
	 * A valid page has $valid_page set to TRUE.
	 */

	public static function error404()
	{
		header($_SERVER["SERVER_PROTOCOL"] . " 404 Not Found");
		exit();
	}

	/**
	 * Configure application for JSON-RPC v2.0 protocol.
	 * JSON-RPC v2.0 compatibility layer with 'method' member as 'class.method'
	 */

	public static function json_rpc()
	{
		// Check if there is POSTed data.
		if (file_get_contents('php://input') !== FALSE) {

			// If POSTed data is in JSON format.
			if (json_decode(file_get_contents('php://input'), TRUE) !== NULL) {

				$json_rpc = json_decode(file_get_contents('php://input'), TRUE);

				// Send error message if server request method is not 'POST'.
				if (isset($_SERVER['REQUEST_METHOD']) && $_SERVER['REQUEST_METHOD'] !== 'POST') { exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32600, 'message' => "Server request method should be 'POST'."]])); }
				// Send error message if 'jsonrpc' and 'method' members are not set.
				if (! isset($json_rpc['jsonrpc']) || ! isset($json_rpc['method']) ) { exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32600, 'message' => "JSON-RPC 'version' and 'method' members should be set."]])); }
				// Send error message if JSON-RPC version is not '2.0'.
				if (isset($json_rpc['jsonrpc']) && $json_rpc['jsonrpc'] !== '2.0') { exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32600, 'message' => "JSON-RPC version should be a string set to '2.0'."]])); }
				// Send error message if 'method' member is not in the format 'class.method'.
				if (isset($json_rpc['method']) && substr_count($json_rpc['method'], '.') !== 1) { exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32602, 'message' => "The JSON-RPC 'method' member should have the format 'class.method'."]])); }

				// Require 'jsonrpc' and 'method' members as minimum for the request object.
				if (isset($json_rpc['jsonrpc']) && isset($json_rpc['method'])) {

					list($class, $method) = explode('.', $json_rpc['method']);
					$class = $class . CONTROLLER_SUFFIX;

					// Respond if class exists and 'id' member is set.
					if (class_exists($class) && isset($json_rpc['id'])) {
						$object = new $class();
						if (method_exists($object, $method)) {
							$object->$method();
							exit();
						} else { exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32601, 'message' => "Method not found."], 'id' => $json_rpc['id']])); }
					} else { exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32601, 'message' => "Class not found."], 'id' => $json_rpc['id']])); }
				}

			} else {
				
				// If POSTed data is not in JSON format.
				exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32700, 'message' => "Please provide data in valid JSON format."]]));
			
			}
		
		}
	}

	/**
	 * Automatic routing of segment(1) and (2) as Class and method
	 */

	public static function route_auto()
	{
		if (self::segment(1) !== FALSE) { $class = self::segment(1) . CONTROLLER_SUFFIX; }
		if (self::segment(2) !== FALSE) { $method = self::segment(2); } else { $method = METHOD_DEFAULT; }

		if (class_exists($class)) {
			$object = new $class();
			if (method_exists($object, $method)) {
				$object->$method();
				exit();
			} else {
				header($_SERVER["SERVER_PROTOCOL"]." 404 Not Found");
				exit();
			}
		}
	}

	/**
	 * Load Controller based on URL path string and HTTP method
	 *
	 * @param string $http_method - HTTP method (e.g. GET, POST, PUT, DELETE)
	 * @param string $string - URL path in the format '/url/string'
	 *                       - Wildcard convention from Codeigniter
	 *                       - (:num) for number and (:any) for string
	 * @param string $class_method - ClassController@method format
	 */

	public static function route($http_method, $path, $class_method)
	{
		if ($_SERVER['REQUEST_METHOD'] == $http_method) {

			// Convert '/' and wilcards (:num) and (:any) to RegEx
			$pattern = str_ireplace( '/', '\/', $path );
			$pattern = str_ireplace( '(:num)', '[0-9]+', $pattern );
			$pattern = str_ireplace( '(:any)', '[^\/]+', $pattern );
					
			// Check for subfolders from DocumentRoot and include in endpoint
			$sub = explode('/', dirname($_SERVER['SCRIPT_NAME']));
			if (! empty($sub[1])) { $subfolder = implode('\/', $sub); } else { $subfolder = ''; }

			$url_path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
			if ( preg_match('/^' . $subfolder . $pattern . '+$/i', $url_path) )  {

				if (is_string($class_method)) {
					if (strstr($class_method, '@')) {
						list($class, $method) = explode('@', $class_method);

						$object = new $class();
						$object->$method();
						exit();
					}
				} else {
					$class_method();
					exit();
				}

			}

		}
	}

	/**
	 * Passes data and renders the View
	 *
	 * @param string $view - View file, excluding .php extension
	 * @param array $data - Data as an array to pass to the View
	 */

	public static function view($view, $data=NULL)
	{
		// Convert array keys to variables
		if (isset($data)) { extract($data); }

		// Render Page View
		return require_once '../views/' . $view . '.php';
	}

	/**
	 * Handles the HTTP API Response
	 *
	 * @param integer $code     - HTTP response code
	 * @param string $data      - Data to transmit
	 */

	public static function api_response($code=200, $data='OK')
	{
		http_response_code($code); // Set HTTP response code and message
		echo $data; // Data in string format
	}

	/**
	 * Handles the HTTP API Call
	 *
	 * @param string $http_method - HTTP request method (e.g. 'GET', 'POST')
	 * @param string $url         - URL of external server API
	 * @param array $data         - Request body in array
	 * @param string $username    - Username
	 * @param string $password    - Password
	 */

	public static function api_call($http_method, $url, $data=NULL, $username=NULL, $password=NULL)
	{
		$ch = curl_init(); // Initialize cURL
		$data_input = json_encode($data); // Convert data to JSON

		// Set cURL options
		curl_setopt($ch, CURLOPT_URL, $url);
		curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $http_method);
		curl_setopt($ch, CURLOPT_POSTFIELDS, $data_input);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
		curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
		// curl_setopt($ch, CURLOPT_HTTPHEADER, array(                                                                          
		// 	'Content-Type: application/json',                                                                                
		// 	'Content-Length: ' . strlen($data_input))                                                                       
		// );

		$result = curl_exec($ch); // Execute cURL
		$http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE); // HTTP response code
		curl_close ($ch); // Close cURL connection

		return ['code' => $http_code, 'data' => $result];
	}

	/**
	 * Web Application Firewall
	 */

	public static function firewall()
	{
		if (FIREWALL_ON == TRUE) {

			// Allow only access from whitelisted IP addresses
			if (isset($_SERVER['REMOTE_ADDR']) && ! in_array($_SERVER['REMOTE_ADDR'], ALLOWED_IP_ADDR)) {
				header($_SERVER["SERVER_PROTOCOL"]." 403 Forbidden");
				exit('<p>You are not allowed to access the application using your IP address.</p>');
			}

			// Allow only URI_WHITELISTED characters on the Request URI.
			if (! empty(URI_WHITELISTED)) {

				$regex_array = str_replace('w', 'alphanumeric', URI_WHITELISTED);
				$regex_array = explode('\\', $regex_array);

				if (isset($_SERVER['REQUEST_URI']) && preg_match('/[^' . URI_WHITELISTED . ']/i', $_SERVER['REQUEST_URI'])) {

					header($_SERVER["SERVER_PROTOCOL"]." 400 Bad Request");
					exit('<p>The URI should only contain alphanumeric and GET request characters:</p><p><ul>' . implode('<li>', $regex_array) . '</ul></p>');
					
				}

			}

			// Deny POST_BLACKLISTED characters in $_POST and post body. '\' is blacklisted by default.
			if (! empty(POST_BLACKLISTED)) {

				$regex_array = explode('\\', POST_BLACKLISTED);

				if (isset($_POST) && preg_match('/[' . POST_BLACKLISTED . '\\\]/i', implode('/', $_POST)) ) {
					header($_SERVER["SERVER_PROTOCOL"]." 400 Bad Request");
					exit('<p>Submitted data should NOT contain the following characters:</p><p><ul>' . implode('<li>', $regex_array) . '<li>\</ul></p>');
				}

				$post_data = file_get_contents('php://input');

				if (isset($post_data) && preg_match('/[' . POST_BLACKLISTED . '\\\]/i', $post_data) ) {
					header($_SERVER["SERVER_PROTOCOL"]." 400 Bad Request");
					exit('<p>Submitted data should NOT contain the following characters:</p><p><ul>' . implode('<li>', $regex_array) . '<li>\</ul></p>');					
				}

			}

		}
	}

	/**
	 * Force application to use SSL
	 */

	public static function force_ssl()
	{
		if ( ENFORCE_SSL == TRUE && (! isset($_SERVER['HTTPS']) || $_SERVER['HTTPS'] !== 'on') ) {
			header('Location: https://' . $_SERVER['SERVER_NAME'] . $_SERVER['REQUEST_URI']);
			exit();
		}
	}

	/**
	 * Helper function to prevent Cross-Site Scripting (XSS)
	 * Uses htmlspecialchars() to prevent XSS
	 *
	 * @param string $string - String to escape
	 */

	public static function esc($string)
	{
		return htmlspecialchars($string, ENT_QUOTES, 'UTF-8');
	}

	/**
	 * Helper function to prevent Cross-Site Request Forgery (CSRF)
	 * Creates a per request token to handle CSRF using sessions
	 */

	public static function csrf_token()
	{
		if (isset($_SESSION)) {
			$_SESSION['csrf-token'] = bin2hex(random_bytes(32));
			return $_SESSION['csrf-token'];
		} else {
			$error_message = 'Please initialize Sessions.';
			$page_title = 'Sessions Error';

			$data = compact('error_message', 'page_title');
			self::view('error', $data);
		}
	}

	/**
	 * Encrypt data using AES CBC-HMAC, CTR-HMAC or GCM
	 *
	 * @param string $plaintext - Plaintext to be encrypted
	 * @return string - contains based64-encoded ciphertext
	 */

	public static function encrypt($plaintext)
	{
		// Encryption - Version 1
		if (! function_exists('encrypt_v1')) {

			function encrypt_v1($plaintext) {

				$version = 'enc-v1'; // Version
				$cipher = strtolower(CIPHER_METHOD); // Cipher Method
				$salt = random_bytes(16); // Salt
				$iv = $salt; // Initialization Vector

				// Derive Keys
				$masterKey = hash_pbkdf2('sha256', PASS_PHRASE, $salt, 10000); // Master Key
				$encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Encryption Key
				$hmacKey = hash_hkdf('sha256', $masterKey, 32, 'sha-256-authentication', $salt); // HMAC Key

				if ($cipher == 'aes-256-gcm') {

					$ciphertext = openssl_encrypt($plaintext, $cipher, $encKey, $options=0, $iv, $tag);
					return $version . '::' . base64_encode($ciphertext) . '::' . base64_encode($tag) . '::' . base64_encode($salt);

				} else {

					$ciphertext = openssl_encrypt($plaintext, $cipher, $encKey, $options=0, $iv);
					$hash = hash_hmac('sha256', $ciphertext, $hmacKey);
					return $version . '::' . base64_encode($ciphertext) . '::' . base64_encode($hash) . '::' . base64_encode($salt);

				}

			}

		}

		/** Version-based Encryption */
		// Default encryption function
		return encrypt_v1($plaintext);
	}

	/**
	 * Decrypt data using AES CBC-HMAC, CTR-HMAC or GCM
	 *
	 * @param string $encypted - base64-encoded ciphertext, hash,
	 *                         - and salt (and tag for GCM)
	 * @return string          - decrypted data
	 */

	public static function decrypt($encrypted)
	{
		// Decryption - Version 1
		if (! function_exists('decrypt_v1')) {

			function decrypt_v1($encrypted) {

				// Return empty if $encrypted is not set or empty.
				if (! isset($encrypted) || empty($encrypted)) { return ''; }

				$cipher = strtolower(CIPHER_METHOD); // Cipher Method

				if ($cipher == 'aes-256-gcm') {

					list($version, $ciphertext, $tag, $salt) = explode('::', $encrypted);
					$ciphertext = base64_decode($ciphertext);
					$tag = base64_decode($tag);
					$salt = base64_decode($salt);

					$iv = $salt; // Initialization Vector

					// Derive Keys
					$masterKey = hash_pbkdf2('sha256', PASS_PHRASE, $salt, 10000); // Master Key
					$encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Encryption Key
					$hmacKey = hash_hkdf('sha256', $masterKey, 32, 'sha-256-authentication', $salt); // HMAC Key

					$plaintext = openssl_decrypt($ciphertext, $cipher, $encKey, $options=0, $iv, $tag);

					// GCM authentication
					if ($plaintext !== FALSE) {
						return $plaintext;
					} else {
						exit ('Please verify authenticity of ciphertext.');
					}

				} else {

					list($version, $ciphertext, $hash, $salt) = explode('::', $encrypted);
					$ciphertext = base64_decode($ciphertext);
					$hash = base64_decode($hash);
					$salt = base64_decode($salt);

					$iv = $salt; // Initialization Vector

					// Derive keys
					$masterKey = hash_pbkdf2('sha256', PASS_PHRASE, $salt, 10000); // Master Key
					$encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Encryption Key
					$hmacKey = hash_hkdf('sha256', $masterKey, 32, 'sha-256-authentication', $salt); // HMAC Key

					$digest = hash_hmac('sha256', $ciphertext, $hmacKey);

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

		$version = explode('::', $encrypted)[0];

		/** Version-based Decryption */
		switch ($version) {
			case 'enc-v1':
				return decrypt_v1($encrypted);
				break;
			default:
				return $encrypted; // Return $encrypted if no encryption detected.
		}
	}

}