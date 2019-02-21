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

namespace Silver\Helpers;

class HTMLElement
{
    private $tagname;
    private $attrs;
    private $children;

    public static function make($tagname, $attrs = [], ...$children)
    {
        return new static($tagname, $attrs, ...$children);
    }

    public function __construct($tagname, $attrs = [], ...$children)
    {
        $this->tagname = $tagname;
        $this->attrs = $attrs;
        $this->children = $children;
    }

    public function appendChild($child)
    {
        if (is_string($child)) {
            $child = htmlspecialchars($child);
        }
        $this->children[] = $child;

        return $this;
    }

    public function children()
    {
        return $this->children;
    }

    public function setAttr($name, $value = true)
    {
        $this->attrs[ $name ] = $value;

        return $this;
    }

    public function attrs()
    {
        return $this->attrs;
    }

    public function __toString()
    {
        $html = '<' . $this->tagname;

        foreach ($this->attrs as $key => $value) {
            if ($value === true) {
                $html .= ' ' . $key;
            } elseif ($value === false) {
                /// ignore
            } else {
                $html .= ' ' . $key . '="' . htmlspecialchars($value) . '"';
            }
        }

        if ($this->children) {
            $html .= '>';
            $html .= implode(
                '', array_map(
                    function ($c) {
                        if (is_string($c)) {
                            return htmlspecialchars($c);
                        }
                        return $c;
                    }, $this->children
                )
            );
            $html .= '</' . $this->tagname . '>';
        } else {
            if (strtolower($this->tagname) == 'script') {
                $html .= '></' . $this->tagname . '>';
            } else {
                $html .= ' />';
            }
        }

        return $html;
    }
}