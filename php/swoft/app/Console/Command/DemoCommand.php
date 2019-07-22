<?php declare(strict_types=1);

namespace App\Console\Command;

use Swoft\Console\Annotation\Mapping\Command;
use Swoft\Console\Annotation\Mapping\CommandMapping;
use Swoft\Console\Exception\ConsoleErrorException;
use Swoft\Console\Helper\Show;
use Swoft\Console\Input\Input;

/**
 * Class DemoCommand
 *
 * @Command()
 */
class DemoCommand
{
    /**
     * @CommandMapping()
     * @param Input $input
     */
    public function test(Input $input): void
    {
        Show::prettyJSON([
            'args' => $input->getArgs(),
            'opts' => $input->getOptions(),
        ]);
    }

    /**
     * @CommandMapping("err")
     * @throws ConsoleErrorException
     */
    public function coError(): void
    {
        ConsoleErrorException::throw('this is an error message');
    }
}
