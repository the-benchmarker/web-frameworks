<?php

declare(strict_types=1);

namespace App\Domain\User\Entity;

use Cycle\Annotated\Annotation\Column;
use Cycle\Annotated\Annotation\Entity;
use App\Domain\User\Repository\UserRepositoryInterface;

#[Entity(
    repository: UserRepositoryInterface::class,
)]
class User
{
    #[Column(type: 'primary')]
    private int $id;

    public function __construct(
        #[Column(type: 'string')]
        private string $username,
        #[Column(type: 'string')]
        private string $email,
    ) {
    }

    public function getId(): int
    {
        return $this->id;
    }

    public function getUsername(): string
    {
        return $this->username;
    }

    public function getEmail(): string
    {
        return $this->email;
    }
}
