<?php

/**
 * This file is part of Collision.
 *
 * (c) Nuno Maduro <enunomaduro@gmail.com>
 *
 *  For the full copyright and license information, please view the LICENSE
 *  file that was distributed with this source code.
 */

namespace NunoMaduro\Collision;

use JakubOnderka\PhpConsoleColor\ConsoleColor;
use JakubOnderka\PhpConsoleHighlighter\Highlighter as BaseHighlighter;
use NunoMaduro\Collision\Contracts\Highlighter as HighlighterContract;

/**
 * This is an Collision Highlighter implementation.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
class Highlighter extends BaseHighlighter implements HighlighterContract
{
    /**
     * Holds the theme.
     *
     * @var array
     */
    protected $theme = [
        BaseHighlighter::TOKEN_STRING => ['light_gray'],
        BaseHighlighter::TOKEN_COMMENT => ['dark_gray', 'italic'],
        BaseHighlighter::TOKEN_KEYWORD => ['yellow'],
        BaseHighlighter::TOKEN_DEFAULT => ['default', 'bold'],
        BaseHighlighter::TOKEN_HTML => ['blue', 'bold'],
        BaseHighlighter::ACTUAL_LINE_MARK => ['bg_red', 'bold'],
        BaseHighlighter::LINE_NUMBER => ['dark_gray', 'italic'],
    ];

    /**
     * Creates an instance of the Highlighter.
     *
     * @param \JakubOnderka\PhpConsoleColor\ConsoleColor|null $color
     */
    public function __construct(ConsoleColor $color = null)
    {
        parent::__construct($color = $color ?: new ConsoleColor);

        foreach ($this->theme as $name => $styles) {
            $color->addTheme($name, $styles);
        }
    }

    /**
     * {@inheritdoc}
     */
    public function highlight(string $content, int $line): string
    {
        return rtrim($this->getCodeSnippet($content, $line, 4, 4));
    }
}
