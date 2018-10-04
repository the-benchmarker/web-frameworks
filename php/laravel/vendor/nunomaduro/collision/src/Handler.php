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

use Whoops\Handler\Handler as AbstractHandler;
use Symfony\Component\Console\Output\OutputInterface;
use NunoMaduro\Collision\Contracts\Writer as WriterContract;
use NunoMaduro\Collision\Contracts\Handler as HandlerContract;

/**
 * This is an Collision Handler implementation.
 *
 * @author Nuno Maduro <enunomaduro@gmail.com>
 */
class Handler extends AbstractHandler implements HandlerContract
{
    /**
     * Holds an instance of the writer.
     *
     * @var \NunoMaduro\Collision\Contracts\Writer
     */
    protected $writer;

    /**
     * Creates an instance of the Handler.
     *
     * @param \NunoMaduro\Collision\Contracts\Writer|null $writer
     */
    public function __construct(WriterContract $writer = null)
    {
        $this->writer = $writer ?: new Writer;
    }

    /**
     * {@inheritdoc}
     */
    public function handle()
    {
        $this->writer->write($this->getInspector());

        return static::QUIT;
    }

    /**
     * {@inheritdoc}
     */
    public function setOutput(OutputInterface $output): HandlerContract
    {
        $this->writer->setOutput($output);

        return $this;
    }

    /**
     * {@inheritdoc}
     */
    public function getWriter(): WriterContract
    {
        return $this->writer;
    }
}
