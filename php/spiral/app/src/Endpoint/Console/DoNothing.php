<?php

declare(strict_types=1);

namespace App\Endpoint\Console;

use Spiral\Console\Attribute\Argument;
use Spiral\Console\Attribute\AsCommand;
use Spiral\Console\Attribute\Option;
use Spiral\Console\Attribute\Question;
use Spiral\Console\Command;
use Symfony\Component\Console\Input\InputOption;

/**
 * Simple command that does nothing, but demonstrates how to use arguments and options.
 *
 * To execute this command run:
 * php app.php do-nothing foo --times=20
 *
 * Run `php app.php help do-nothing` to see all available options.
 */
#[AsCommand(name: 'do-nothing', description: 'The command does nothing.')]
final class DoNothing extends Command
{
    #[Argument(description: 'Task name')]
    #[Question(question: 'Provide task name')]
    private string $name;

    #[Option(
        shortcut: 't',
        description: 'Number of times to repeat',
        mode: InputOption::VALUE_OPTIONAL
    )]
    private int $times = 10;

    public function __invoke(): int
    {
        $this->info(\sprintf(
            'The task "%s" has been successfully completed "%d" times!',
            $this->name,
            $this->times
        ));

        return self::SUCCESS;
    }
}
