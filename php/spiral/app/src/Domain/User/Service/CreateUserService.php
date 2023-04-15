<?php

declare(strict_types=1);

namespace App\Domain\User\Service;

use Cycle\ORM\EntityManagerInterface;
use App\Domain\User\Entity\User;

/**
 * Simple service that creates new user.
 */
final class CreateUserService
{
    public function __construct(
        private readonly EntityManagerInterface $em,
    ) {
    }

    public function create(string $username, string $email): User
    {
        $user = new User($username, $email);
        $this->em->persist($user)->run();

        return $user;
    }
}
