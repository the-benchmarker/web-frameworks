<?php declare(strict_types=1);


namespace App\Migration;


use Swoft\Devtool\Annotation\Mapping\Migration;
use Swoft\Devtool\Migration\Migration as BaseMigration;

/**
 * Class AddMsg20190630164222
 *
 * @since 2.0
 *
 * @Migration(20190630164222)
 */
class AddMsg extends BaseMigration
{
    /**
     * @return void
     */
    public function up(): void
    {
        $sql = <<<sql
CREATE TABLE `users` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `age` int(11) NOT NULL DEFAULT '0',
  `password` varchar(100) CHARACTER SET utf8mb4 NOT NULL DEFAULT '',
  `user_desc` varchar(120) CHARACTER SET utf8mb4 NOT NULL DEFAULT '',
  `name` varchar(20) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
sql;
        $this->execute($sql);
    }

    /**
     * @return void
     */
    public function down(): void
    {

        $dropSql = <<<sql
drop table if exists `users`;
sql;
        $this->execute($dropSql);
    }
}
