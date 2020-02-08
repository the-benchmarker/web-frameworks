<?php

/*
|--------------------------------------------------------------------------
| BasicPHP Functions Library
|--------------------------------------------------------------------------
|
| url_path()         - retrieves the URL path substring separated by '/'
| homepage()         - render hompage
| error404()         - Handle Error 404 - Page Not Found - Invalid URI
| route_rpc()        - JSON-RPC v2.0 compatibility layer
| route_auto()       - automatic routing of URL path to Class and method
| route_class()      - routes URL path request to Controllers
| view()             - passes data and renders the View
| pdo_conn()         - PHP Data Objects (PDO) database connection
| api_response()     - handles API response
| api_call()         - handles API call
| firewall()         - web application firewall
| force_ssl()       - force application to use SSL
| esc()             - uses htmlspecialchars() to prevent XSS
| csrf_token()      - uses sessions to create per request CSRF token
| encrypt()         - encrypt data using AES-CBC-HMAC
| decrypt()         - decrypt data using AES-CBC-HMAC
|
*/

/**
 * Get URL path string value after the BASE_URL.
 *
 * @param integer $order - URL substring position from the BASE_URL
 *                       - url_path(1) as first string after BASE_URL
 */

function url_path($order)
{

	if (isset($_SERVER['REQUEST_URI'])) {
		$url_path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
		$url = explode('/', $url_path);
	}

    if ( isset($url[$order+SUB_DIR]) || ! empty($url[$order+SUB_DIR]) ) {
		return $url[$order+SUB_DIR];
	} else {
		return FALSE;
	}

}

/**
 * Render Homepage
 */

function homepage()
{

	if ( empty(url_path(1)) ) {
		list($class, $method) = explode('@', HOME_PAGE);
		$object = new $class();
		return $object->$method();
	}

}

/**
 * Handle Error 404 - Page Not Found - Invalid URI
 * A valid page has $valid_page set to TRUE.
 */

function error404()
{

	if ( ! isset($valid_page) || $valid_page !== TRUE ) {
		header($_SERVER["SERVER_PROTOCOL"]." 404 Not Found");
		exit();
	}

}

/**
 * JSON-RPC v2.0 Compatibility Layer with 'method' member as 'class.method'
 */

function route_rpc()
{

	$valid_page = TRUE; // Set page as valid

	// Check if HTTP request method is 'POST', if there is POSTed data, and the POSTed data is in JSON format.
	if ($_SERVER['REQUEST_METHOD'] == 'POST' && file_get_contents('php://input') !== FALSE && json_decode( file_get_contents('php://input'), TRUE ) !== NULL) {

		$json_rpc = json_decode( file_get_contents('php://input'), TRUE );
	
		// Requires the 'jsonrpc', 'method' and 'id' members of the request object
		if (isset($json_rpc['jsonrpc']) && isset($json_rpc['method']) && isset($json_rpc['id'])) {
	
			if (strstr($json_rpc['method'], '.') == FALSE) exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => 32600, 'message' => "The JSON-RPC 'method' member should have the format 'class.method'."], 'id' => $json_rpc['id']]));
	
			list($class, $method) = explode('.', $json_rpc['method']);
			$class = ucfirst($class) . CONTROLLER_SUFFIX;
			$method = lcfirst($method);
	
			if (class_exists($class)) {
				$object = new $class();
				if ( method_exists($object, $method) ) {
					return $object->$method();
					exit();
				} else { exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => 32601, 'message' => "Method not found."], 'id' => $json_rpc['id']])); }
			} else { exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => 32601, 'message' => "Class not found."], 'id' => $json_rpc['id']])); }
		}
	
	}

}

/**
 * Automatic routing of url_path(1) and (2) as Class and method
 */

function route_auto()
{

	$valid_page = TRUE; // Set page as valid

	if (url_path(1) !== FALSE) { $class = ucfirst(url_path(1)) . CONTROLLER_SUFFIX; }
	if (url_path(2) !== FALSE) { $method = lcfirst(url_path(2)); } else { $method = METHOD_DEFAULT; }

	if (class_exists($class)) {
		$object = new $class();
		if ( method_exists($object, $method) ) {
			return $object->$method();
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

function route_class($http_method, $path, $class_method)
{

	$valid_page = TRUE; // Set page as valid

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

			list($class, $method) = explode('@', $class_method);

			$object = new $class();
			return $object->$method();

			}

	}

}

/**
 * Passes data and renders the View
 *
 * @param string $view - View file, excluding .php extension
 * @param array $data - Data as an array to pass to the View
 */

function view($view, $data=NULL)
{

	// Convert array keys to variables
	if (isset($data)) { extract($data); }

	// Render Page View
	return require_once '../views/' . $view . '.php';

}

/**
 * PHP Data Objects (PDO) database connection
 *
 * @param string $database - Database (e.g. mysql)
 * @param string $servername - Server Name (e.g localhost)
 * @param string $dbname - Database Name
 * @param string $username - Username
 * @param string $password - Password
 */

function pdo_conn($database, $servername, $dbname, $username, $password)
{

	$conn = new PDO("$database:host=$servername;dbname=$dbname", $username, $password, array(
		PDO::ATTR_PERSISTENT => TRUE
	));
	$conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

	return $conn;

}

/**
 * Handles the HTTP REST API Response
 *
 * @param array $data - Array to be encoded to JSON
 * @param string $message - Message to send with response
 */

function api_response($data, $message=NULL)
{

	// Define content type as JSON data through the header
	header("Content-Type: application/json; charset=utf-8");

	// Data and message as arrays to send with response
	$response['data'] = $data;
	$response['message'] = $message;

	// Encode $response array to JSON
	echo json_encode($response);

}

/**
 * Handles the HTTP REST API Calls
 *
 * @param string $http_method - HTTP request method (e.g. 'GET', 'POST')
 * @param string $url - URL of external server API
 * @param string $data - POST fields in array
 * @param string $username - Username
 * @param string $password - Password
 */

function api_call($http_method, $url, $data=NULL, $username=NULL, $password=NULL)
{

	// Initialize cURL
	$ch = curl_init();

	// Convert $data array parameter to JSON
	$data_json = json_encode($data);

	// Set cURL options
	curl_setopt($ch, CURLOPT_URL, $url);
	curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $http_method);
	// curl_setopt($ch, CURLOPT_POST, 1);
	curl_setopt($ch, CURLOPT_POSTFIELDS, $data_json);
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, TRUE);
	curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
	curl_setopt($ch, CURLOPT_HTTPHEADER, array(                                                                          
	    'Content-Type: application/json',                                                                                
	    'Content-Length: ' . strlen($data_json))                                                                       
	);

	// Execute cURL
	$result = curl_exec($ch);

	// Close cURL connection
	curl_close ($ch);

	// Convert JSON response from external server to an array
	$data_output = json_decode($result, TRUE);

	return $data_output;

}

/**
 * Web Application Firewall
 */

function firewall()
{

	if (FIREWALL_ON == TRUE) {

		// Allow only access from whitelisted IP addresses
		if (! in_array($_SERVER['REMOTE_ADDR'], ALLOWED_IP_ADDR)) {

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

function force_ssl()
{

	if ( ENFORCE_SSL == TRUE && ( ! isset($_SERVER['HTTPS']) || $_SERVER['HTTPS'] !== 'on' ) ) {
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

function esc($string)
{

	return htmlspecialchars($string, ENT_QUOTES, 'UTF-8');

}

/**
 * Helper function to prevent Cross-Site Request Forgery (CSRF)
 * Creates a per request token to handle CSRF using sessions
 */

function csrf_token()
{

	if (isset($_SESSION)) {

		$_SESSION['csrf-token'] = bin2hex(random_bytes(32));
		return $_SESSION['csrf-token'];

	} else {

		$error_message = 'Please initialize Sessions.';
		$page_title = 'Sessions Error';

		$data = compact('error_message', 'page_title');
		view('error', $data);

	}

}

/**
 * Encrypt data using AES-CBC-HMAC
 *
 * @param string $plaintext - Plaintext to be encrypted
 */

function encrypt($plaintext)
{

	function encrypt_v1($plaintext) {

		// Version
		$version = 'enc-v1';
		
		// Cipher method to AES with 256-bit key
		$cipher = strtolower(CIPHER_METHOD);
		// Salt for encryption key
		$salt_key = random_bytes(16);
		// Derive encryption key
		$key = hash_pbkdf2('sha256', PASS_PHRASE, $salt_key, 10000);
		// Initialization vector
		$iv = random_bytes(16);

		if ($cipher == 'aes-256-gcm') {

			$ciphertext = openssl_encrypt($plaintext, $cipher, $key, $options=0, $iv, $tag);
			return $version . '::' . base64_encode($ciphertext) . '::' . base64_encode($iv) . '::' . base64_encode($tag) . '::' . base64_encode($salt_key);

		} else {

			// Salt for HMAC key
			$salt_hmac = random_bytes(16);
			// Derive HMAC key
			$key_hmac = hash_pbkdf2('sha256', PASS_PHRASE, $salt_hmac, 10000);

			$ciphertext = openssl_encrypt($plaintext, $cipher, $key, $options=0, $iv);
			$hash = hash_hmac('sha256', $ciphertext, $key_hmac);
			return $version . '::' . base64_encode($ciphertext) . '::' . base64_encode($hash) . '::' . base64_encode($iv) . '::' . base64_encode($salt_key) . '::' . base64_encode($salt_hmac);

		}

	}

	/** Version-based Encryption */
	// Default encryption function
	return encrypt_v1($plaintext);

}

/**
 * Decrypt data using AES-CBC-HMAC
 *
 * @param string $encypted - base64_encoded ciphertext, hash,
 *                         - iv, salt_key, and salt_hmac
 */

function decrypt($encrypted)
{

	function decrypt_v1($encrypted) {

		// Return empty if $encrypted is not set or empty.
		if ( ! isset($encrypted) || empty($encrypted) ) { return ''; }

		// Cipher method to AES with 256-bit key
		$cipher = strtolower(CIPHER_METHOD);

		if ($cipher == 'aes-256-gcm') {

			list($version, $ciphertext, $iv, $tag, $salt_key) = explode('::', $encrypted);
			$ciphertext = base64_decode($ciphertext);
			$iv = base64_decode($iv);
			$tag = base64_decode($tag);
			$salt_key = base64_decode($salt_key);

			// Derive encryption key
			$key = hash_pbkdf2('sha256', PASS_PHRASE, $salt_key, 10000);

			$plaintext = openssl_decrypt($ciphertext, $cipher, $key, $options=0, $iv, $tag);

			// GCM authentication
			if ($plaintext !== FALSE) {
				return $plaintext;
			} else {
				exit ('<strong>Warning: </strong>Please verify authenticity of ciphertext.');
			}

		} else {

			list($version, $ciphertext, $hash, $iv, $salt_key, $salt_hmac) = explode('::', $encrypted);
			$ciphertext = base64_decode($ciphertext);
			$hash = base64_decode($hash);
			$iv = base64_decode($iv);
			$salt_key = base64_decode($salt_key);
			$salt_hmac = base64_decode($salt_hmac);

			// Derive encryption key
			$key = hash_pbkdf2('sha256', PASS_PHRASE, $salt_key, 10000);
			// Derive HMAC key
			$key_hmac = hash_pbkdf2('sha256', PASS_PHRASE, $salt_hmac, 10000);

			$digest = hash_hmac('sha256', $ciphertext, $key_hmac);

			// HMAC authentication
			if  ( hash_equals($hash, $digest) ) {
				return openssl_decrypt($ciphertext, $cipher, $key, $options=0, $iv);
				}
			else {
				exit ('<strong>Warning: </strong>Please verify authenticity of ciphertext.');
			}

		}

	}

	$version = explode('::', $encrypted)[0];

	/** Version-based Decryption */
	// Return $encrypted if no encryption detected.
	switch ($version) {
		case 'enc-v1':
			return decrypt_v1($encrypted);
			break;
		default:
			return $encrypted;
	}

}