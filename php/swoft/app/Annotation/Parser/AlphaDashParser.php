<?php declare(strict_types=1);

namespace App\Annotation\Parser;

use ReflectionException;
use Swoft\Annotation\Annotation\Mapping\AnnotationParser;
use Swoft\Annotation\Annotation\Parser\Parser;
use App\Annotation\Mapping\AlphaDash;
use Swoft\Validator\Exception\ValidatorException;
use Swoft\Validator\ValidatorRegister;

/**
 * Class AlphaDashParser
 *
 * @AnnotationParser(annotation=AlphaDash::class)
 */
class AlphaDashParser extends Parser
{
    /**
     * @param int $type
     * @param object $annotationObject
     *
     * @return array
     * @throws ReflectionException
     * @throws ValidatorException
     */
    public function parse(int $type, $annotationObject): array
    {
        if ($type != self::TYPE_PROPERTY) {
            return [];
        }
        ValidatorRegister::registerValidatorItem($this->className, $this->propertyName, $annotationObject);
        return [];
    }
}
