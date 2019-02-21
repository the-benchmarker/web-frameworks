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

namespace Silver\Engine\Ghost;

use Silver\Core\Blueprints\RenderInterface;
use Silver\Core\Kernel;
use Silver\Core\Route;
use Silver\Http\Session;
use Silver\Http\Request;

/**
 * Template
 */
class Template implements RenderInterface
{
    private $_file;
    private $_data   = array();
    private $_master = null;
    private $_debug  = false;

    function __construct($file, $data = [])
    {
        $this->_file = $file;
        $this->_data = $data;
    }

    public function set($key, $value)
    {
        $this->_data[$key] = $value;
        return $this;
    }

    public function data()
    {
        return $this->_data;
    }

    public function render()
    {
        if (!file_exists($this->_file)) {
            throw new \Exception("File not found {$this->_file}");
        }

        $render = file_get_contents($this->_file);

        $render = $this->parseDebug($render);
        $render = $this->parseComments($render);
        $render = $this->parseIncludes($render);

        $render = $this->filter_if($render);
        $render = $this->filter_foreach($render);
        $render = $this->filter_for($render);
        $render = $this->parseLang($render);
        $render = $this->parseTrans($render);

        $render = $this->parseExtends($render);
        // $render = $this->parseUrl($render); // url()
        $render = $this->parseAssets($render); // asset()
        $render = $this->parseAssetsCss($render); // asset()
        $render = $this->parseAssetsJs($render); // asset()
        // $render = $this->parseSys($render); // sys()
        // $render = $this->parseRoutes($render); // path()
        $render = $this->parseUrlName($render); // name()
        // $render = $this->parseRouteName($render); // routes


        $render = $this->parseComponent($render);

        $render = $this->parse_blocks($render);
        $render = $this->parse_vars($render);
        $render = $this->parse_vars_skip($render);
        // $render = $this->parseRoute($render);


        if ($this->_master) {
            $render = $this->parseMaster($render);
        }

        foreach ($this->_data as $key => $value) {
            $$key = $value;
        }

        if ($this->_debug) {
            echo $render;
            exit;
        }


        try {
            ob_start();
            eval('?>' . $render);
            $render = ob_get_contents();
        } finally {
            ob_get_clean();
        }

        return $render;
    }

    private function parseDebug($body)
    {
        $self = $this;
        $body = preg_replace_callback(
            "/#debug[\s*]*/", function ($match) use ($self) {
                $self->_debug = true;
                return '';
            }, $body
        );
        return $body;
    }

    private function parse_vars($body)
    {

        $body = preg_replace_callback(
            "/{{{([^}]*)}}}/", function ($match) {
                $var = trim($match[1]);
                return "<?php echo @$var;?>";
            }, $body
        );

        //For vue/laravel skipping
        $body = preg_replace_callback(
            "/@{{([^}]*)}}/", function ($match) {
                $var = trim($match[1]);
                ndd($var);
                /*            return "<?php echo @htmlentities($var); ?>";*/
                return '{@{@' . htmlentities($var) . '@}@}';
            }, $body
        );

        $body = preg_replace_callback(
            "/{{([^}]*)}}/", function ($match) {
                $var = trim($match[1]);
                return "<?php echo @htmlentities($var); ?>";
            }, $body
        );

        return $body;
    }

    private function parse_vars_skip($body)
    {
        //For vue/laravel skipping
        $body = preg_replace_callback(
            "/{@{@([^}]*)@}@}/", function ($match) {
                $var = trim($match[1]);

                return '{{ ' . htmlentities($var) . ' }}';

            }, $body
        );

        return $body;
    }

    private function parse_blocks($body)
    {
        $body = preg_replace_callback(
            "/#block\\((.*)\\)/", function ($match) {
                $blockname = trim($match[1]);
                return "{{{ \$_block_{$blockname} }}}";
            }, $body
        );

        return $body;
    }

    private function filter_if($body)
    {
        $body = preg_replace_callback(
            "/#if.*/", function ($match) {
                $if = substr(trim($match[0]), 1);
                return "<?php $if { ?>";
            }, $body
        );

        $body = preg_replace_callback(
            "/#elseif.*/", function ($match) {
                $if = substr(trim($match[0]), 1);
                return "<?php } $if { ?>";
            }, $body
        );

        $body = preg_replace_callback(
            "/#else/", function ($match) {
                return "<?php } else { ?>";
            }, $body
        );

        $body = preg_replace_callback(
            "/#endif/", function ($match) {
                return "<?php } ?>";
            }, $body
        );

        return $body;
    }

    private function filter_foreach($body)
    {
        $body = preg_replace_callback(
            "/#foreach.*/", function ($match) {
                $if = substr(trim($match[0]), 1);
                return "<?php $if { ?>";
            }, $body
        );

        $body = preg_replace_callback(
            "/#endforeach/", function ($match) {
                return "<?php } ?>";
            }, $body
        );

        return $body;
    }

    private function filter_for($body)
    {
        $body = preg_replace_callback(
            "/#for.*/", function ($match) {
                $if = substr(trim($match[0]), 1);
                return "<?php $if { ?>";
            }, $body
        );

        $body = preg_replace_callback(
            "/#endfor/", function ($match) {
                return "<?php } ?>";
            }, $body
        );

        return $body;
    }

    protected function parseComments($body)
    {
        $body = preg_replace_callback(
            '/<!--(.*)-->/', function ($match) {
                return '';
            }, $body
        );
        return $body;
    }

    protected function parseMaster($body)
    {
        $master = str_replace('.', '/', $this->_master);

        $blocks = [];
        $current_block = null;
        $block_content = '';

        foreach (explode("\n", $body) as $line) {
            if (preg_match('/#set\[(.*)\]/s', $line, $matches)) {
                $current_block = $matches[1];
                continue;
            }

            if (preg_match('/#end/', $line)) {
                $blocks[$current_block] = $block_content;
                $block_content = '';
                $current_block = null;
                continue;
            }

            $block_content .= $line . "\n";
        }


        $fullpath = ROOT . "App/Views/{$master}.ghost" . EXT;

        if (!is_file($fullpath)) {
            $fullpath = ROOT . "App/Views/{$master}.ghost.tpl";
        }

        $ghost = new \Silver\Engine\Ghost\Template($fullpath);

        if ($blocks) {
            foreach ($blocks as $key => $value) {
                $ghost->set('_block_' . $key, $value);
            }
            foreach ($this->_data as $key => $value) {
                $ghost->set($key, $value);
            }
        }

        return $ghost->render('');
    }

    protected function parseAssets($body)
    {
        $bodyLines = explode("\n", $body);

        foreach ($bodyLines as $key => $value) {

            $bodyLines[$key] = preg_replace_callback(
                "/{{ asset\('(.*)'\) }}/s", function ($match) {
                    return $this->processAsset($match[1]);
                }, $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function parseAssetsCss($body)
    {
        $bodyLines = explode("\n", $body);

        foreach ($bodyLines as $key => $value) {

            $bodyLines[$key] = preg_replace_callback(
                "/{{ css\('(.*)'\) }}/s", function ($match) {
                    return $this->processAssetCss($match[1]);
                }, $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function parseAssetsJs($body)
    {
        $bodyLines = explode("\n", $body);

        foreach ($bodyLines as $key => $value) {

            $bodyLines[$key] = preg_replace_callback(
                "/{{ js\('(.*)'\) }}/s", function ($match) {
                    return $this->processAssetJs($match[1]);
                }, $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function parseSys($body)
    {
        $bodyLines = explode("\n", $body);

        foreach ($bodyLines as $key => $value) {

            $bodyLines[$key] = preg_replace_callback(
                "/{{ sys\('(.*)'\) }}/s", function ($match) {
                    return $this->processSys($match[1]);
                }, $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }


    protected function parseLang($body)
    {

        $bodyLines = explode("\n", $body);
        //        dd($bodyLines);

        foreach ($bodyLines as $key => $value) {

            $bodyLines[$key] = preg_replace_callback(
                "/{{ lang\('(.*)'\) }}/s", function ($match) {
                    return $this->processLang($match[1]);
                }, $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function parseTrans($body)
    {
        $bodyLines = explode("\n", $body);

        foreach ($bodyLines as $key => $value) {

            $bodyLines[$key] = preg_replace_callback(
                "/{{ trans\('(.*)'\) }}/s", function ($match) {
                    //                dd($match);
                    return $this->processLang($match[1]);
                }, $value
            );
            //            dd($match);
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function processUrl($relativePath)
    {
        return URL . $relativePath;
    }

    protected function processAsset($relativePath)
    {
        return URL . '/assets/' . $relativePath;
    }

    protected function processAssetCss($relativePath)
    {
        return '<link rel="stylesheet" href="' . URL . '/assets/css/' . $relativePath . '.css">';
    }

    protected function processAssetJs($relativePath)
    {
        return '<script src="' . URL . '/assets/js/' . $relativePath . '.js"></script>';
    }

    protected function processSys($relativePath)
    {
        return URL . 'System/' . $relativePath;
    }


    // protected function parseRoute($body)
    // {
    //     $bodyLines = explode("\n", $body);
    //
    //     foreach ($bodyLines as $key => $value) {
    //         $bodyLines[$key] = preg_replace_callback("/{{ route:(.*) }}/s", function ($match) {
    //             dd($march);
    //             return $this->processRouteName($match[0], $match[1]);
    //         }, $value);
    //     }
    //     $body = implode("\n", $bodyLines);
    //
    //     return $body;
    // }
    //
    // protected function processRouteName($name, $name2)
    // {
    //     $route = \Silver\Core\Kernel::getContext()->getRoute($name);
    //     dd($route);
    // }


    protected function processLang($relativePath)
    {
        //        dd($relativePath);
        $relativePath = explode('.', $relativePath);
        if (Session::exists('lang')) {
            $file = include ROOT . 'Storage/Lang/' . Session::get('lang') . '/' . $relativePath[0] . EXT;
        } else {
            $file = include ROOT . 'Storage/Lang/en/' . $relativePath[0] . EXT;
        }
        return $file[$relativePath[1]];
    }

    protected function parseIncludes($body)
    {
        $bodyLines = explode("\n", $body);

        foreach ($bodyLines as $key => $value) {
            // |(include \'(.*)\')
            $bodyLines[$key] = preg_replace_callback(
                "/{{ include\('(.*)'\) }}/", function ($match) {
                    return $this->includeFile($match[1]);
                }, $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function parseComponent($body)
    {
        $bodyLines = explode("\n", $body);

        foreach ($bodyLines as $key => $value) {
            $bodyLines[$key] = preg_replace_callback(
                "/{{ component\('(.*)'\) }}/", function ($match) {
                    return $this->includeComponent($match[1]);
                }, $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function parseRouteName($body)
    {
        $bodyLines = explode("\n", $body);
        foreach ($bodyLines as $key => $value) {
            $bodyLines[$key] = preg_replace_callback(
                "/{{ route\(([^,)]*)(.*)\) }}/s", function ($match) {
                    $route_string = trim($match[1], ' ');
                    $vars_string = trim($match[2], ', ');
                    return "<?php echo \\Silver\\Core\\Route::getRoute($route_string)->url($vars_string); ?>";
                }, $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function parseUrlName($body)
    {
        $bodyLines = explode("\n", $body);
        foreach ($bodyLines as $key => $value) {
            $bodyLines[$key] = preg_replace_callback(
                "/@routeName\(([^,)]*)(.*)\)/s",
                function ($match) {
                    // $route_string = trim($match[1], ' ');
                    $segment = new Request;
                    return $segment->segment(1);
                    // return false;
                },
                $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function parseExtends($body)
    {

        $body = preg_replace_callback(
            "/{{ extends\('(.*)'\) }}/", function ($match) {
                $this->_master = $match[1];
                return '';
            }, $body
        );

        return $body;
    }

    protected function parseUrl($body)
    {
        $bodyLines = explode("\n", $body);

        foreach ($bodyLines as $key => $value) {
            // |(include \'(.*)\')
            $bodyLines[$key] = preg_replace_callback(
                "/{{ url\('(.*)'\) }}/s", function ($match) {
                    return $this->generateUrl($match[1]);
                }, $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function generateUrl($link)
    {
        if (!empty($link)) {
            $url = URL . $link;
            return $url;
        } else {
            return URL;
        }

    }

    protected function parseRoutes($body)
    {

        $bodyLines = explode("\n", $body);

        foreach ($bodyLines as $key => $value) {
            // |(include \'(.*)\')
            $bodyLines[$key] = preg_replace_callback(
                "/{{ path\('(.*)'\) }}/s", function ($match) {
                    return $this->generateUrlFromRoute($match[1]);
                }, $value
            );
        }

        $body = implode("\n", $bodyLines);

        return $body;
    }

    protected function generateUrlFromRoute($as)
    {
        $route = Kernel::getContext()->getRoute($as);

        return $route['path'];
    }

    protected function includeFile($alias)
    {
        $alias = str_replace('.', '/', $alias);

        $loadPath = ROOT . "App/Views/{$alias}.ghost" . EXT;

        if (!is_file($loadPath)) {
            $loadPath = ROOT . "App/Views/{$alias}.ghost.tpl";
        }

        if (file_exists($loadPath)) {
            $ghost = new self($loadPath);
            return $ghost->render($alias);
        }

        return '';
    }

    protected function includeComponent($alias)
    {
        $alias = str_replace('.', '/', $alias);

        $loadPath = ROOT . "App/Views/components/{$alias}.ghost" . EXT;

        if (!is_file($loadPath)) {
            $loadPath = ROOT . "App/Views/components/{$alias}.ghost.tpl";
        }

        if (file_exists($loadPath)) {
            $ghost = new self($loadPath);

            // add data to the component
            $ghost->_data = $this->_data;

            return $ghost->render($alias);
        }

        return '';
    }

    protected function includeAsset($alias)
    {
        $loadPath = ROOT . "public/{$alias}";
        if (file_exists($loadPath)) {
            $ghost = new self($loadPath);
            return $ghost->render($alias);
        }

        return '';
    }

    protected function includeAssetCss($alias)
    {
        $loadPath = '<link rel="stylesheet" href="' . ROOT . "public/css/{$alias}" . '">';
        dd($loadPath);
        if (file_exists($loadPath)) {
            $ghost = new self($loadPath);
            return $ghost->render($alias);
        }

        return '';
    }

    protected function includeAssetJs($alias)
    {
        $loadPath = ROOT . "public/js/{$alias}";
        if (file_exists($loadPath)) {
            $ghost = new self($loadPath);
            return $ghost->render($alias);
        }

        return '';
    }

    protected function includeSys($alias)
    {
        $loadPath = ROOT . "System/{$alias}";
        if (file_exists($loadPath)) {
            $ghost = new self($loadPath);
            return $ghost->render($alias);
        }

        return '';
    }

    protected function getRoute($route_name, $vars = [])
    {
        $route = Route::getRoute($route_name);
        if ($route) {
            return $route->url($vars);
        } else {
            throw new \Exception("Route $route_name not found.");
        }
    }
}
