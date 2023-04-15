<?php

declare(strict_types=1);

namespace App\Endpoint\Console;

use App\Domain\User\Service\CreateUserService;
use Spiral\Console\Attribute\Argument;
use Spiral\Console\Attribute\AsCommand;
use Spiral\Console\Attribute\Question;
use Spiral\Console\Command;

/**
 * Simple command that demonstrates how to use Cycle ORM.
 */
#[AsCommand(name: 'create:user', description: 'Create a new user')]
class CreateUserCommand extends Command
{
    #[Argument(description: 'User name')]
    #[Question(question: 'Provide username')]
    private string $username;

    #[Argument(description: 'User E-mail')]
    #[Question(question: 'Provide e-mail address')]
    private string $email;

    public function __invoke(CreateUserService $service): int
    {
        $createdUser = $service->create($this->username, $this->email);

        $this->info(
            \sprintf(
                'The user "%s" with ID "%d" has been successfully created!',
                $createdUser->getUsername(),
                $createdUser->getId(),
            ),
        );

        return self::SUCCESS;
    }
}
