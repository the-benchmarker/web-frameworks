<?php

/*
 * This file is part of Collision.
 *
 * (c) Nuno Maduro <enunomaduro@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace NunoMaduro\Collision\Contracts;

use Whoops\Handler\HandlerInterface;
use Symfony\Component\Console\Output\OutputInterface;

/**
 * This is an Collision Handler contract.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
interface Handler extends HandlerInterface
{
    /**
     * Sets the output.
     *
     * @param  \Symfony\Component\Console\Output\OutputInterface $output
     *
     * @return \NunoMaduro\Collision\Contracts\Handler
     */
    public function setOutput(OutputInterface $output): Handler;

    /**
     * Returns the writer.
     *
     * @return \NunoMaduro\Collision\Contracts\Writer
     */
    public function getWriter(): Writer;
}
