<?php declare(strict_types=1);


namespace App\Http\Controller;

use App\Model\Logic\BreakerLogic;
use Exception;
use Swoft\Bean\Annotation\Mapping\Inject;
use Swoft\Http\Server\Annotation\Mapping\Controller;
use Swoft\Http\Server\Annotation\Mapping\RequestMapping;

/**
 * Class BreakerController
 *
 * @since 2.0
 *
 * @Controller(prefix="breaker")
 */
class BreakerController
{
    /**
     * @Inject()
     *
     * @var BreakerLogic
     */
    private $logic;

    /**
     * @RequestMapping()
     *
     * @return string
     * @throws Exception
     */
    public function breaked(): string
    {
        return $this->logic->func();
    }

    /**
     * @RequestMapping()
     *
     * @return array
     * @throws Exception
     */
    public function unbreak(): array
    {
        return [$this->logic->func2()];
    }

    /**
     * @RequestMapping()
     *
     * @return string
     * @throws Exception
     */
    public function loopBraker(): string
    {
        return $this->logic->loop();
    }

    /**
     * @RequestMapping()
     *
     * @return string
     * @throws Exception
     */
    public function unFallback(): string
    {
        return $this->logic->unFallback();
    }
}