<?php

declare(strict_types=1);

namespace App\Command;

use Override;
use Psr\Http\Message\ServerRequestFactoryInterface;
use Psr\Http\Message\StreamFactoryInterface;
use Psr\Http\Message\UploadedFileFactoryInterface;
use Spiral\RoadRunner\Http\PSR7Worker as HttpWorker;
use Spiral\RoadRunner\Worker;
use Sunrise\Http\Router\RouterInterface;
use Symfony\Component\Console\Attribute\AsCommand;
use Symfony\Component\Console\Command\Command;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;

#[AsCommand('rr:work')]
final class RoadRunnerWorker extends Command
{
    public function __construct(
        private readonly RouterInterface $router,
        private readonly ServerRequestFactoryInterface $requestFactory,
        private readonly StreamFactoryInterface $streamFactory,
        private readonly UploadedFileFactoryInterface $uploadsFactory,
    ) {
        parent::__construct();
    }

    #[Override]
    protected function execute(InputInterface $input, OutputInterface $output): int
    {
        $worker = new HttpWorker(Worker::create(), $this->requestFactory, $this->streamFactory, $this->uploadsFactory);

        while ($request = $worker->waitRequest()) {
            $worker->respond($this->router->handle($request));
        }

        return self::SUCCESS;
    }
}
