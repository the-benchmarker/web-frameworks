<?php

/**
 * This file is part of Collision.
 *
 * (c) Nuno Maduro <enunomaduro@gmail.com>
 *
 *  For the full copyright and license information, please view the LICENSE
 *  file that was distributed with this source code.
 */

namespace NunoMaduro\Collision\Contracts;

use Whoops\Exception\Inspector;
use Symfony\Component\Console\Output\OutputInterface;

/**
 * This is the Collision Writer contract.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
interface Writer
{
    /**
     * Ignores traces where the file string matches one
     * of the provided regex expressions.
     *
     * @param  string[] $ignore The regex expressions.
     *
     * @return \NunoMaduro\Collision\Contracts\Writer
     */
    public function ignoreFilesIn(array $ignore): Writer;

    /**
     * Declares whether or not the Writer should show the trace.
     *
     * @param  bool $show
     *
     * @return \NunoMaduro\Collision\Contracts\Writer
     */
    public function showTrace(bool $show): Writer;

    /**
     * Declares whether or not the Writer should show the editor.
     *
     * @param  bool $show
     *
     * @return \NunoMaduro\Collision\Contracts\Writer
     */
    public function showEditor(bool $show): Writer;

    /**
     * Writes the details of the exception on the console.
     *
     * @param \Whoops\Exception\Inspector $inspector
     */
    public function write(Inspector $inspector): void;

    /**
     * Sets the output.
     *
     * @param  \Symfony\Component\Console\Output\OutputInterface $output
     *
     * @return \NunoMaduro\Collision\Contracts\Writer
     */
    public function setOutput(OutputInterface $output): Writer;

    /**
     * Gets the output.
     *
     * @return \Symfony\Component\Console\Output\OutputInterface
     */
    public function getOutput(): OutputInterface;
}
