<?php declare(strict_types=1);


namespace App\Common;


use Swoft\Bean\Annotation\Mapping\Bean;
use Swoft\Db\Connection\Connection;
use Swoft\Db\Contract\DbSelectorInterface;

/**
 * Class DbSelector
 *
 * @since 2.0
 *
 * @Bean()
 */
class DbSelector implements DbSelectorInterface
{
    /**
     * @param Connection $connection
     */
    public function select(Connection $connection): void
    {
        $selectIndex  = (int)context()->getRequest()->query('id', 0);
        $createDbName = $connection->getDb();

        if ($selectIndex == 0) {
            $selectIndex = '';
        }

        if($createDbName == 'test2'){
            $createDbName = 'test';
        }

        $dbName = sprintf('%s%s', $createDbName, (string)$selectIndex);
        $connection->db($dbName);
    }
}
