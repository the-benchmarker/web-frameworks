<?php

declare(strict_types=1);

namespace App\Infrastructure\CycleORM\Typecaster;

use Cycle\Database\DatabaseInterface;
use Cycle\ORM\Parser\CastableInterface;
use Cycle\ORM\Parser\UncastableInterface;
use Ramsey\Uuid\Uuid;
use Ramsey\Uuid\UuidInterface;

class UuidTypecast implements CastableInterface, UncastableInterface
{
    private array $rules = [];

    public function __construct(
        private readonly DatabaseInterface $database
    ) {
    }

    public function setRules(array $rules): array
    {
        foreach ($rules as $key => $rule) {
            if ($rule === 'uuid') {
                unset($rules[$key]);
                $this->rules[$key] = $rule;
            }
        }

        return $rules;
    }

    public function cast(array $values): array
    {
        foreach ($this->rules as $column => $rule) {
            if (!isset($values[$column])) {
                continue;
            }

            $values[$column] = Uuid::fromString($values[$column]);
        }

        return $values;
    }

    public function uncast(array $values): array
    {
        foreach ($this->rules as $column => $rule) {
            if (!isset($values[$column]) || !$values[$column] instanceof UuidInterface) {
                continue;
            }

            $values[$column] = $values[$column]->toString();
        }

        return $values;
    }
}
