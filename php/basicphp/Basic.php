<?php

/**
 * BasicPHP - A frameworkless class library for developing web applications and
 *          - application programming interfaces or API's.
 *          -
 *          - The purpose of the library is for developers to build applications
 *          - and services that are framework agnostic using native PHP functions
 *          - and API's.
 *
 * @package   BasicPHP
 * @version   v0.9.10
 * @link      https://github.com/ray-ang/basicphp
 * @author    Raymund John Ang <raymund@open-nis.org>
 * @copyright Copyright (c) 2019-2022 Raymund John Ang <raymund@open-nis.org>
 * @license   MIT License
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

        if (! isset($uri[$order+$sub_dir])) {
            return false;
        }

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
        if ($http_method === 'ANY') {
            $http_method = $_SERVER['REQUEST_METHOD'];
        } // Any HTTP Method

        if ($_SERVER['REQUEST_METHOD'] === $http_method) {

            // Convert '/' and wilcards (:num) and (:any) to RegEx
            $pattern = str_ireplace('/', '\/', $path);
            $pattern = str_ireplace('(:num)', '[0-9]+', $pattern);
            $pattern = str_ireplace('(:any)', '[^\/]+', $pattern);

            // Check for subfolders from DocumentRoot and include in endpoint
            $sub = explode('/', dirname($_SERVER['SCRIPT_NAME']));
            $subfolder = (! empty($sub[1])) ? implode('\/', $sub) : '';

            $uri = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
            if (preg_match('/^' . $subfolder . $pattern . '+$/i', $uri)) {
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

    public static function view($view, $data=null)
    {
        $file = '../views/' . $view . '.php';
        if (! empty($data)) {
            extract($data);
        } // Convert array keys to variables
        if (file_exists($file) && is_readable($file) && pathinfo($file)['extension'] === 'php') {
            require_once $file;
        } // Render page view
    }

    /**
     * HTTP API request call using cURL
     *
     * @param string $url         - URL of API endpoint
     * @param string $http_method - HTTP request method (e.g. 'GET', 'POST')
     * @param array $data         - Request body in array format
     * @param string $user_token  - Basic 'username:password' or Bearer token
     *
     * @return (int|string)[]     - HTTP response code and result of cURL execution
     */

    public static function apiCall($url, $http_method='GET', $data=null, $user_token=null)
    {
        if (substr(strtolower(trim($url)), 0, 16) !== 'http://localhost' && substr(strtolower(trim($url)), 0, 8) !== 'https://') {
            self::apiResponse(400, 'API URL should start with "https://".');
        } // Require HTTPS API URL

        $auth_scheme = (stristr($user_token, ':')) ? 'Basic' : 'Bearer'; // Authorization scheme
        $auth_cred = ($auth_scheme === 'Basic') ? base64_encode($user_token) : $user_token; // Credentials
        $content_type = (is_array($data)) ? 'application/json' : 'text/plain'; // Content Type
        $data = (is_array($data)) ? json_encode($data) : $data; // Data array to JSON
        $data = (is_object($data)) ? json_encode($data) : $data; // Data object to JSON

        $ch = curl_init(); // Initialize cURL

        // Set cURL options
        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $http_method);
        curl_setopt($ch, CURLOPT_POSTFIELDS, $data);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_SSL_VERIFYHOST, false);
        curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
        curl_setopt(
            $ch,
            CURLOPT_HTTPHEADER,
            array(
                "Authorization: $auth_scheme $auth_cred",
                "Content-Type: $content_type",
                'Content-Length: ' . strlen($data)
            )
        );

        $result = curl_exec($ch); // Execute cURL
        $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE); // HTTP response code
        curl_close($ch); // Close cURL connection

        return ['code' => $http_code, 'data' => $result];
    }

    /**
     * Handle HTTP API response
     *
     * @param integer $code        - HTTP response code
     * @param string $data         - Data to transmit
     * @param string $content_type - Header: Content-Type
     */

    public static function apiResponse($code, $data=null, $content_type='text/plain')
    {
        $data = (is_array($data)) ? json_encode($data) : $data; // Data array to JSON
        $data = (is_object($data)) ? json_encode($data) : $data; // Data object to JSON

        if ($code > 199 && $code < 300) {
            $message = 'OK';
        } // OK response
        if ($code < 200 || $code > 299) {
            $message = $data;
        } // If no data, $data = $message

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
     * Per request token using secure & httponly cookie
     * Basic::setFirewall() should be executed. $verify_csrf_token = TRUE (default)
     */

    public static function csrfToken()
    {
        $token = bin2hex(random_bytes(32));
        setcookie('csrf-token', $token, null, null, null, true, true);
        return $token;
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

    public static function encrypt($plaintext=null, $pass_phrase=null, $header='encv1', $cipher='aes-256-gcm', $hmac_algo='sha512')
    {
        if (! isset($plaintext)) {
            self::apiResponse(500, 'Set plaintext for encryption.');
        }
        if (! isset($pass_phrase)) {
            self::apiResponse(500, 'Set passphrase for the encryption key, or link for the encryption API.');
        }

        if ($cipher !== 'aes-256-gcm' && $cipher !== 'aes-256-ctr' && $cipher !== 'aes-256-cbc') {
            self::apiResponse(500, "Encryption cipher method should either be 'aes-256-gcm', 'aes-256-ctr', 'aes-256-cbc'.");
        }

        // Encryption - Version 2
        if (! function_exists('encrypt_v2')) {

            function encrypt_v2($plaintext, $pass_phrase, $header, $cipher, $hmac_algo)
            {

                if (filter_var($pass_phrase, FILTER_VALIDATE_URL)) {
                    $api = $pass_phrase . '?action=encrypt';
                    $response = Basic::apiCall($api, 'POST', ['key' => $pass_phrase]);

                    if ($response['code'] !== 200) {
                        Basic::apiResponse($response['code']);
                    }

                    $pass_phrase = bin2hex(random_bytes(32)); // Random password
                }

                // Derive keys
                $salt = hash('sha3-256', $pass_phrase);
                $masterKey = hash_pbkdf2('sha256', $pass_phrase, $salt, 10000); // Master key
                $encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Data Encryption key

                $ciphertext = openssl_encrypt($plaintext, $cipher, $encKey, $options=0);
                $encrypted = $header . '.' . $ciphertext;

                if (isset($api) && $response['code'] === 200) {
                    $response = Basic::apiCall($api, 'POST', ['key' => $pass_phrase]);
                    $data = json_decode($response['data'], true);
                    $dek_token = $data['key'];

                    return str_replace('=', '', $encrypted . '.' . $dek_token); // Strip off '='
                } else {
                    return str_replace('=', '', $encrypted); // Strip off '='
                }

            }

        }

        // Encryption - Version 1
        if (! function_exists('encrypt_v1')) {

            function encrypt_v1($plaintext, $pass_phrase, $header, $cipher, $hmac_algo)
            {

                $nonce = random_bytes(openssl_cipher_iv_length($cipher)); // Number once
                $iv = $nonce; // Initialization Vector
                $salt = $nonce; // Salt

                if (filter_var($pass_phrase, FILTER_VALIDATE_URL)) {
                    $api = $pass_phrase . '?action=encrypt';
                    $response = Basic::apiCall($api, 'POST', ['key' => $pass_phrase]);

                    if ($response['code'] !== 200) {
                        Basic::apiResponse($response['code']);
                    }

                    $pass_phrase = bin2hex(random_bytes(32)); // Random password
                }

                // Derive keys
                $masterKey = hash_pbkdf2('sha256', $pass_phrase, $salt, 10000); // Master key
                $encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Data Encryption key
                $hmacKey = hash_hkdf('sha256', $masterKey, 32, 'sha-256-authentication', $salt); // HMAC key

                if ($cipher === 'aes-256-gcm') {

                    $ciphertext = openssl_encrypt($plaintext, $cipher, $encKey, $options=0, $iv, $tag);
                    $encrypted = $header . '.' . base64_encode($ciphertext) . '.' . base64_encode($tag) . '.' . base64_encode($nonce);

                    if (isset($api) && $response['code'] === 200) {
                        $response = Basic::apiCall($api, 'POST', ['key' => $pass_phrase]);
                        $data = json_decode($response['data'], true);
                        $dek_token = $data['key']; // Encrypted passphrase token

                        return str_replace('=', '', $encrypted . '.' . $dek_token); // Strip off '='
                    } else {
                        return str_replace('=', '', $encrypted); // Strip off '='
                    }

                } else {

                    $ciphertext = openssl_encrypt($plaintext, $cipher, $encKey, $options=0, $iv);
                    $hash = hash_hmac($hmac_algo, $ciphertext, $hmacKey);
                    $encrypted = $header . '.' . base64_encode($ciphertext) . '.' . base64_encode($hash) . '.' . base64_encode($nonce);

                    if (isset($api) && $response['code'] === 200) {
                        $response = Basic::apiCall($api, 'POST', ['key' => $pass_phrase]);
                        $data = json_decode($response['data'], true);
                        $dek_token = $data['key'];

                        return str_replace('=', '', $encrypted . '.' . $dek_token); // Strip off '='
                    } else {
                        return str_replace('=', '', $encrypted); // Strip off '='
                    }

                }

            }

        }

        /** Version-based encryption */
        if ($header == 'encv2') {
            return encrypt_v2($plaintext, $pass_phrase, $header='encv2', $cipher='aes-256-ecb', $hmac_algo);
        }
        if ($header == 'encv1') {
            return encrypt_v1($plaintext, $pass_phrase, $header, $cipher, $hmac_algo);
        }
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

    public static function decrypt($encrypted=null, $pass_phrase=null, $header='encv1', $cipher='aes-256-gcm', $hmac_algo='sha512')
    {
        if (! isset($encrypted)) {
            self::apiResponse(500, 'Set encryption token for decryption.');
        }
        if (! isset($pass_phrase)) {
            self::apiResponse(500, 'Set passphrase for the encryption key, or link for the encryption API.');
        }

        if ($cipher !== 'aes-256-gcm' && $cipher !== 'aes-256-ctr' && $cipher !== 'aes-256-cbc') {
            self::apiResponse(500, "Encryption cipher method should either be 'aes-256-gcm', 'aes-256-ctr', 'aes-256-cbc'.");
        }

        // Decryption - Version 2
        if (! function_exists('decrypt_v2')) {

            function decrypt_v2($encrypted, $pass_phrase, $header, $cipher, $hmac_algo)
            {

                if (filter_var($pass_phrase, FILTER_VALIDATE_URL)) {
                    $api = $pass_phrase . '?action=decrypt';
                    $response = Basic::apiCall($api, 'POST', ['key' => $pass_phrase]);

                    if ($response['code'] !== 200) {
                        Basic::apiResponse($response['code']);
                    }

                    list($header, $ciphertext, $header_dek, $ciphertext_dek) = explode('.', $encrypted);
                } else {
                    list($header, $ciphertext) = explode('.', $encrypted);
                }

                if (isset($api) && $response['code'] === 200) {
                    $response = Basic::apiCall($api, 'POST', ['key' => $header_dek . '.' . $ciphertext_dek]);
                    $data = json_decode($response['data'], true);
                    $pass_phrase = $data['key']; // Decrypted passphrase
                }

                // Derive keys
                $salt = hash('sha3-256', $pass_phrase);
                $masterKey = hash_pbkdf2('sha256', $pass_phrase, $salt, 10000); // Master key
                $encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Encryption key

                return openssl_decrypt($ciphertext, $cipher, $encKey, $options=0);

            }

        }

        // Decryption - Version 1
        if (! function_exists('decrypt_v1')) {

            function decrypt_v1($encrypted, $pass_phrase, $header, $cipher, $hmac_algo)
            {

                if ($cipher === 'aes-256-gcm') {

                    if (filter_var($pass_phrase, FILTER_VALIDATE_URL)) {
                        $api = $pass_phrase . '?action=decrypt';
                        $response = Basic::apiCall($api, 'POST', ['key' => $pass_phrase]);

                        if ($response['code'] !== 200) {
                            Basic::apiResponse($response['code']);
                        }

                        list($header, $ciphertext, $tag, $nonce, $header_dek, $ciphertext_dek, $tag_dek, $nonce_dek) = explode('.', $encrypted);

                        $ciphertext = base64_decode($ciphertext);
                        $tag = base64_decode($tag);
                        $nonce = base64_decode($nonce); // Nonce
                        $iv = $nonce; // IV
                        $salt = $nonce; // Salt
                    } else {
                        list($header, $ciphertext, $tag, $nonce) = explode('.', $encrypted);

                        $ciphertext = base64_decode($ciphertext);
                        $tag = base64_decode($tag);
                        $nonce = base64_decode($nonce); // Nonce
                        $iv = $nonce; // IV
                        $salt = $nonce; // Salt
                    }

                    if (isset($api) && $response['code'] === 200) {
                        $response = Basic::apiCall($api, 'POST', ['key' => $header_dek . '.' . $ciphertext_dek . '.' . $tag_dek . '.' . $nonce_dek]);
                        $data = json_decode($response['data'], true);
                        $pass_phrase = $data['key']; // Decrypted random password
                    }

                    // Derive keys
                    $masterKey = hash_pbkdf2('sha256', $pass_phrase, $salt, 10000); // Master key
                    $encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Data Encryption key
                    $hmacKey = hash_hkdf('sha256', $masterKey, 32, 'sha-256-authentication', $salt); // HMAC key

                    $plaintext = openssl_decrypt($ciphertext, $cipher, $encKey, $options=0, $iv, $tag);

                    // GCM authentication
                    if ($plaintext) {
                        return $plaintext;
                    } else {
                        return false;
                    }

                } else {

                    if (filter_var($pass_phrase, FILTER_VALIDATE_URL)) {
                        $api = $pass_phrase . '?action=decrypt';
                        $response = Basic::apiCall($api, 'POST', ['key' => $pass_phrase]);

                        if ($response['code'] !== 200) {
                            Basic::apiResponse($response['code']);
                        }

                        list($header, $ciphertext, $hash, $nonce, $header_dek, $ciphertext_dek, $hash_dek, $nonce_dek) = explode('.', $encrypted);

                        $ciphertext = base64_decode($ciphertext);
                        $hash = base64_decode($hash);
                        $nonce = base64_decode($nonce); // Nonce
                        $iv = $nonce; // IV
                        $salt = $nonce; // Salt
                    } else {
                        list($header, $ciphertext, $hash, $nonce) = explode('.', $encrypted);

                        $ciphertext = base64_decode($ciphertext);
                        $hash = base64_decode($hash);
                        $nonce = base64_decode($nonce);
                        $iv = $nonce; // IV
                        $salt = $nonce; // Salt
                    }

                    if (isset($api) && $response['code'] === 200) {
                        $response = Basic::apiCall($api, 'POST', ['key' => $header_dek . '.' . $ciphertext_dek . '.' . $hash_dek . '.' . $nonce_dek]);
                        $data = json_decode($response['data'], true);
                        $pass_phrase = $data['key']; // Decrypted passphrase
                    }

                    // Derive keys
                    $masterKey = hash_pbkdf2('sha256', $pass_phrase, $salt, 10000); // Master key
                    $encKey = hash_hkdf('sha256', $masterKey, 32, 'aes-256-encryption', $salt); // Encryption key
                    $hmacKey = hash_hkdf('sha256', $masterKey, 32, 'sha-256-authentication', $salt); // HMAC key

                    $digest = hash_hmac($hmac_algo, $ciphertext, $hmacKey);

                    // HMAC authentication
                    if  (hash_equals($hash, $digest)) {
                        return openssl_decrypt($ciphertext, $cipher, $encKey, $options=0, $iv);
                    } else {
                        return false;
                    }

                }

            }

        }

        /** Version-based decryption */
        if ($header == 'encv2') {
            return decrypt_v2($encrypted, $pass_phrase, $header='encv2', $cipher='aes-256-ecb', $hmac_algo);
        }
        if ($header == 'encv1') {
            return decrypt_v1($encrypted, $pass_phrase, $header, $cipher, $hmac_algo);
        }
        if (! isset($encrypted) || empty($encrypted)) {
            return '';
        } // Return empty if $encrypted is not set or empty.
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

    public static function setErrorReporting($boolean=true)
    {
        if ($boolean) {
            error_reporting(E_ALL);
        } elseif (! $boolean) {
            error_reporting(0);
        } else {
            self::apiResponse(500, 'Boolean parameter for Basic::setErrorReporting() can only be TRUE or FALSE.');
        }
    }

    /**
     * JSON Request Body as $_POST - API Access
     */

    public static function setJsonBodyAsPOST()
    {
        $body = file_get_contents('php://input');
        if (! empty($body) && is_array(json_decode($body, true))) {
            $_POST = json_decode($body, true);
        }
    }

    /**
     * Web Application Firewall
     *
     * @param array $ip_blacklist          - Blacklisted IP addresses
     * @param boolean $verify_csrf_token   - Verify CSRF token
     * @param boolean $post_auto_escape    - Automatically escape $_POST
     * @param string $uri_whitelist        - Whitelisted URI RegEx characters
     */

    public static function setFirewall($ip_blacklist=[], $verify_csrf_token=true, $post_auto_escape=true, $uri_whitelist='\w\/\.\-\_\?\=\&\:\$')
    {
        // Deny access from blacklisted IP addresses
        if (isset($_SERVER['REMOTE_ADDR']) && in_array($_SERVER['REMOTE_ADDR'], $ip_blacklist)) {
            self::apiResponse(403, 'You are not allowed to access the application using your IP address.');
        }

        // Verify CSRF token
        if ($verify_csrf_token) {
            if (isset($_POST['csrf-token']) && isset($_COOKIE['csrf-token']) && ! hash_equals($_POST['csrf-token'], $_COOKIE['csrf-token'])) {
                self::apiResponse(400, 'Please check authenticity of CSRF token.');
            }
        }

        // Automatically escape $_POST values using htmlspecialchars()
        if ($post_auto_escape && isset($_POST)) {
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
        if (! is_array($classes)) {
            Basic::apiResponse(500, 'Basic::setAutoloadClass() argument should be an array.');
        }

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
        if (self::segment(2)) {
            $method = strtolower(self::segment(2));
        }

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
     * @param string $pass_phrase - KEK master key
     */

    public static function setEncryptApi($pass_phrase)
    {
        if (! isset($pass_phrase)) {
            self::apiResponse(500, 'Set passphrase for the encryption key.');
        }

        /* Require POST method */
        if ($_SERVER['REQUEST_METHOD'] !== 'POST') {
            self::apiResponse(405, "Method should be 'POST'.");
            exit();
        }

        $body = file_get_contents('php://input'); // Request body

        /* Require request body (not enctype="multipart/form-data") */
        if (empty($body)) {
            self::apiResponse(400, 'The request should have a body, and must not be enctype="multipart/form-data".');
            exit();
        }

        /* Require request body to be in JSON format */
        $body_array = json_decode($body, true); // Convert JSON body string into array

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
        if (empty($_GET[$action])) {
            self::apiResponse(400, "GET parameter '$action' should be set.");
        }
        if (substr(trim($_GET[$action]), 0, 1) === '.') {
            self::apiResponse(400, "GET parameter '$action' should not start with a period (.) .");
        }
        if (substr_count($_GET[$action], '.') < 1) {
            self::apiResponse(400, "GET parameter '$action' should contain a period (.) to separate class and method.");
        }
        if (substr_count($_GET[$action], '.') > 1) {
            self::apiResponse(400, "GET parameter '$action' should only contain one period (.) .");
        }

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
        $array = json_decode($body, true); // JSON body to array

        header('Content-Type: application/json'); // Set content type as JSON

        if ($_SERVER['REQUEST_METHOD'] !== 'GET' && $_SERVER['REQUEST_METHOD'] !== 'POST') {
            exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32601, 'message' => 'Only GET and POST methods allowed.'], 'id' => null]));
        } // Only GET and POST

        if ($_SERVER['HTTP_CONTENT_TYPE'] !== 'application/json') {
            exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32700, 'message' => "Request content type should be 'application/json'."], 'id' => null]));
        } // Accept only JSON request content type

        if (! $body) {
            exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32700, 'message' => 'Request should have a request body.'], 'id' => null]));
        } // Require request body

        if ($body && ! $array) {
            exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32700, 'message' => 'Provide request body data in valid JSON format.'], 'id' => null]));
        } // Require valid JSON

        if (strpos(ltrim($body), '[') === 0) {
            exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32700, 'message' => 'Batch processing not supported at this time.'], 'id' => null]));
        } // No batch processing

        if (! isset($array['jsonrpc']) || $array['jsonrpc'] !== '2.0') {
            exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32600, 'message' => "JSON-RPC 'version' member should be set, and assigned a value of '2.0'."], 'id' => null]));
        } // JSON-RPC (version) member

        if (! isset($array['method']) || ! strstr($array['method'], '.')) {
            exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32600, 'message' => "JSON-RPC 'method' member should be set with the format 'class.method'."], 'id' => null]));
        } // Method member

        list($class, $method) = explode('.', $array['method']); // Method member as 'class.method'
        $class = $class . $controller; // Default controller suffix

        // If class exists
        if (class_exists($class)) {
            if (! isset($array['id'])) {
                exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32600, 'message' => "JSON-RPC 'id' member should be set."], 'id' => null]));
            } // Require ID member

            $object = new $class();
            if (method_exists($object, $method)) {
                $object->$method();
                exit;
            } else {
                exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32601, 'message' => 'Method not found.'], 'id' => null]));
            }
        } else {
            exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => -32601, 'message' => 'Class not found.'], 'id' => null]));
        }
    }

}
