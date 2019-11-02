<?php

/*
|--------------------------------------------------------------------------
| BasicPHP Functions Library
|--------------------------------------------------------------------------
|
| These are core functions necessary to run the nano-framework:
|
| 1. url_value() - retrieves the URL path substring separated by '/'
| 2. route_rpc() - JSON-RPC v2.0 compatibility layer
| 3. route_auto() - automatic routing of URL path to Class and method
| 4. route_class() - routes URL path request to Controllers
| 5. view() - passes data and renders the View
| 6. pdo_conn() - PHP Data Objects (PDO) database connection
| 7. api_response() - handles API response
| 8. api_call() - handles API call
| 9. force_ssl() - force application to use SSL
| 10. esc() - uses htmlspecialchars() to prevent XSS
| 11. csrf_token() - uses sessions to create per request CSRF token
|
*/

/**
 * Get URL path string value after the BASE_URL.
 *
 * @param integer $order - URL substring position from the BASE_URL
 *                       - url_value(1) as first string after BASE_URL
 */

function url_value($order)
{
    if (isset($_SERVER['REQUEST_URI'])) {
        $url_path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
        $url = explode('/', $url_path);
    }

    if (isset($url[$order+SUB_DIR]) || ! empty($url[$order+SUB_DIR])) {
        return $url[$order+SUB_DIR];
    } else {
        return false;
    }
}

/**
 * JSON-RPC v2.0 Compatibility Layer with 'method' member as 'class.method'
 */

function route_rpc()
{

    // Check if HTTP request method is 'POST', if there is POSTed data, and the POSTed data is in JSON format.
    if ($_SERVER['REQUEST_METHOD'] == 'POST' && file_get_contents('php://input') !== false && json_decode(file_get_contents('php://input'), true) !== null) {
        $json_rpc = json_decode(file_get_contents('php://input'), true);
    
        // Requires the 'jsonrpc', 'method' and 'id' members of the request object
        if (isset($json_rpc['jsonrpc']) && isset($json_rpc['method']) && isset($json_rpc['id'])) {
            if (strstr($json_rpc['method'], '.') == false) {
                exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => 32600, 'message' => "The JSON-RPC 'method' member should have the format 'class.method'."], 'id' => $json_rpc['id']]));
            }
    
            list($class, $method) = explode('.', $json_rpc['method']);
            $class = ucfirst($class) . CONTROLLER_SUFFIX;
            $method = lcfirst($method);
    
            if (class_exists($class)) {
                $object = new $class();
                if (method_exists($object, $method)) {
                    $object->$method();
                    exit();
                } else {
                    exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => 32601, 'message' => "Method not found."], 'id' => $json_rpc['id']]));
                }
            } else {
                exit(json_encode(['jsonrpc' => '2.0', 'error' => ['code' => 32601, 'message' => "Class not found."], 'id' => $json_rpc['id']]));
            }
        }
    }
}

/**
 * Automatic routing of url_value(1) and (2) as Class and method
 */

function route_auto()
{
    if (url_value(1) !== false) {
        $class = ucfirst(url_value(1)) . CONTROLLER_SUFFIX;
    }
    if (url_value(2) !== false) {
        $method = lcfirst(url_value(2));
    } else {
        $method = METHOD_DEFAULT;
    }

    if (class_exists($class)) {
        $object = new $class();
        if (method_exists($object, $method)) {
            $object->$method();
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
    if ($_SERVER['REQUEST_METHOD'] == $http_method) {

        // Convert '/' and wilcards (:num) and (:any) to RegEx
        $pattern = str_ireplace('/', '\/', $path);
        $pattern = str_ireplace('(:num)', '[0-9]+', $pattern);
        $pattern = str_ireplace('(:any)', '[^\/]+', $pattern);
                
        // Check for subfolders from DocumentRoot and include in endpoint
        $sub = explode('/', dirname($_SERVER['SCRIPT_NAME']));
        if (! empty($sub[1])) {
            $subfolder = implode('\/', $sub);
        } else {
            $subfolder = '';
        }

        $url_path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
        if (preg_match('/^' . $subfolder . $pattern . '+$/i', $url_path)) {
            list($class, $method) = explode('@', $class_method);

            $object = new $class();
            $object->$method();
        }
    }
}

/**
 * Passes data and renders the View
 *
 * @param string $view - View file, excluding .php extension
 * @param array $data - Data as an array to pass to the View
 */

function view($view, $data=null)
{

    // Convert array keys to variables
    if (isset($data)) {
        extract($data);
    }

    // Render Page View
    require_once '../views/' . $view . '.php';
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
        PDO::ATTR_PERSISTENT => true
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

function api_response($data, $message=null)
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

function api_call($http_method, $url, $data=null, $username=null, $password=null)
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
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
    curl_setopt(
        $ch,
        CURLOPT_HTTPHEADER,
        array(
        'Content-Type: application/json',
        'Content-Length: ' . strlen($data_json))
    );

    // Execute cURL
    $result = curl_exec($ch);

    // Close cURL connection
    curl_close($ch);

    // Convert JSON response from external server to an array
    $data_output = json_decode($result, true);

    return $data_output;
}

/**
 * Force application to use SSL
 */

function force_ssl()
{
    if (ENFORCE_SSL == true && (! isset($_SERVER['HTTPS']) || $_SERVER['HTTPS'] !== 'on')) {
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
