<?php declare(strict_types=1);

namespace App\Validator;

use Swoft\Validator\Annotation\Mapping\Validator;
use Swoft\Validator\Contract\ValidatorInterface;
use Swoft\Validator\Exception\ValidatorException;

/**
 * Class CustomerValidator
 *
 * @since 2.0
 *
 * @Validator(name="userValidator")
 */
class CustomerValidator implements ValidatorInterface
{
    /**
     * @param array $data
     * @param array $params
     *
     * @return array
     * @throws ValidatorException
     */
    public function validate(array $data, array $params): array
    {
        $start = $data['start'] ?? null;
        $end = $data['end'] ?? null;
        if ($start === null && $end === null) {
            throw new ValidatorException('Start time and end time cannot be empty');
        }

        if ($start > $end) {
            throw new ValidatorException('Start cannot be greater than the end time');
        }

        return $data;
    }
}
