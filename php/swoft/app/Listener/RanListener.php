<?php declare(strict_types=1);


namespace App\Listener;

use Swoft\Db\Connection\Connection;
use Swoft\Db\DbEvent;
use Swoft\Event\Annotation\Mapping\Listener;
use Swoft\Event\EventHandlerInterface;
use Swoft\Event\EventInterface;
use Swoft\Stdlib\Helper\StringHelper;

/**
 * Class RanListener
 *
 * @since 2.0
 *
 * @Listener(DbEvent::SQL_RAN)
 */
class RanListener implements EventHandlerInterface
{

    /**
     * SQL ran
     *
     * @param EventInterface $event
     *
     */
    public function handle(EventInterface $event): void
    {
        /* @var Connection $connection */
        $connection = $event->getTarget();

        $querySql = $event->getParam(0);
        $bindings = $event->getParam(1);

        $rawSql = $this->getRawSql($querySql, $bindings, $connection);

//        output()->info($rawSql);
    }

    /**
     * Returns the raw SQL by inserting parameter values into the corresponding placeholders in [[sql]].
     * Note that the return value of this method should mainly be used for logging purpose.
     * It is likely that this method returns an invalid SQL due to improper replacement of parameter placeholders.
     *
     * @param string     $sql
     * @param array      $bindings
     * @param Connection $connection
     *
     * @return string the raw SQL with parameter values inserted into the corresponding placeholders in [[sql]].
     */
    public function getRawSql(string $sql, array $bindings, Connection $connection)
    {
        if (empty($bindings)) {
            return $sql;
        }
        foreach ($bindings as $name => $value) {
            if (is_int($name)) {
                $name = '?';
            }

            if (is_string($value) || is_array($value)) {
                $param = $connection->getQueryGrammar()->quoteString($value);
            } elseif (is_bool($value)) {
                $param = ($value ? 'TRUE' : 'FALSE');
            } elseif ($value === null) {
                $param = 'NULL';
            } else {
                $param = (string)$value;
            }

            $sql = StringHelper::replaceFirst($name, $param, $sql);
        }

        return $sql;
    }
}
