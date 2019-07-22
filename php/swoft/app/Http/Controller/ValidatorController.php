<?php declare(strict_types=1);

namespace App\Http\Controller;

use Swoft\Http\Message\Request;
use Swoft\Http\Server\Annotation\Mapping\Controller;
use Swoft\Http\Server\Annotation\Mapping\RequestMapping;
use Swoft\Validator\Annotation\Mapping\Validate;

/**
 * Class ValidatorController
 *
 * @Controller()
 */
class ValidatorController
{
    /**
     * Verify all defined fields in the TestValidator validator
     *
     * @RequestMapping()
     * @Validate(validator="TestValidator")
     *
     * @param Request $request
     *
     * @return array
     */
    function validateAll(Request $request): array
    {
        return $request->getParsedBody();
    }

    /**
     * Verify only the type field in the TestValidator validator
     *
     * @RequestMapping()
     * @Validate(validator="TestValidator", fields={"type"})
     *
     * @param Request $request
     *
     * @return array
     */
    function validateType(Request $request): array
    {
        return $request->getParsedBody();
    }

    /**
     * Verify only the password field in the TestValidator validator
     *
     * @RequestMapping()
     * @Validate(validator="TestValidator", fields={"password"})
     *
     * @param Request $request
     *
     * @return array
     */
    function validatePassword(Request $request): array
    {
        return $request->getParsedBody();
    }

    /**
     * Customize the validator with userValidator
     *
     * @RequestMapping()
     * @Validate(validator="userValidator")
     *
     * @param Request $request
     *
     * @return array
     */
    function validateCustomer(Request $request): array
    {
        return $request->getParsedBody();
    }
}
