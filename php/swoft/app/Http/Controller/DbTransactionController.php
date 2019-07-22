<?php declare(strict_types=1);


namespace App\Http\Controller;

use App\Model\Entity\Count;
use App\Model\Entity\User;
use App\Model\Entity\User3;
use Swoft\Db\DB;
use Swoft\Http\Server\Annotation\Mapping\Controller;
use Swoft\Http\Server\Annotation\Mapping\RequestMapping;
use Throwable;
use function sgo;

/**
 * Class DbTransactionController
 *
 * @since 2.0
 *
 * @Controller("dbTransaction")
 */
class DbTransactionController
{
    /**
     * @RequestMapping(route="ts")
     *
     * @return false|string
     * @throws Throwable
     */
    public function ts()
    {
        $id = $this->getId();

        DB::beginTransaction();
        $user = User::find($id);

        sgo(function () use ($id) {
            DB::beginTransaction();
            User::find($id);
        });

        return json_encode($user->toArray());
    }

    /**
     * @RequestMapping(route="cm")
     *
     * @return false|string
     * @throws Throwable
     */
    public function cm()
    {
        $id = $this->getId();

        DB::beginTransaction();
        $user = User::find($id);
        DB::commit();

        sgo(function () use ($id) {
            DB::beginTransaction();
            User::find($id);
            DB::commit();
        });

        return json_encode($user->toArray());
    }

    /**
     * @RequestMapping(route="rl")
     *
     * @return false|string
     * @throws Throwable
     */
    public function rl()
    {
        $id = $this->getId();

        DB::beginTransaction();
        $user = User::find($id);
        DB::rollBack();

        sgo(function () use ($id) {
            DB::beginTransaction();
            User::find($id);
            DB::rollBack();
        });

        return json_encode($user->toArray());
    }

    /**
     * @RequestMapping(route="ts2")
     *
     * @return false|string
     * @throws Throwable
     */
    public function ts2()
    {
        $id = $this->getId();

        DB::connection()->beginTransaction();
        $user = User::find($id);

        sgo(function () use ($id) {
            DB::connection()->beginTransaction();
            User::find($id);
        });

        return json_encode($user->toArray());
    }

    /**
     * @RequestMapping(route="cm2")
     *
     * @return false|string
     * @throws Throwable
     */
    public function cm2()
    {
        $id = $this->getId();

        DB::connection()->beginTransaction();
        $user = User::find($id);
        DB::connection()->commit();

        sgo(function () use ($id) {
            DB::connection()->beginTransaction();
            User::find($id);
            DB::connection()->commit();
        });

        return json_encode($user->toArray());
    }

    /**
     * @RequestMapping(route="rl2")
     *
     * @return false|string
     * @throws Throwable
     */
    public function rl2()
    {
        $id = $this->getId();

        DB::connection()->beginTransaction();
        $user = User::find($id);
        DB::connection()->rollBack();

        sgo(function () use ($id) {
            DB::connection()->beginTransaction();
            User::find($id);
            DB::connection()->rollBack();
        });

        return json_encode($user->toArray());
    }

    /**
     * @RequestMapping()
     */
    public function multiPool()
    {
        DB::beginTransaction();

        // db3.pool
        $user = new User3();
        $user->setAge(mt_rand(1, 100));
        $user->setUserDesc('desc');

        $user->save();
        $uid3 = $user->getId();


        //db.pool
        $uid = $this->getId();

        $count = new Count();
        $count->setUserId(mt_rand(1, 100));
        $count->setAttributes('attr');
        $count->setCreateTime(time());

        $count->save();
        $cid = $count->getId();

        DB::rollBack();

        $u3 = User3::find($uid3)->toArray();
        $u  = User::find($uid);
        $c  = Count::find($cid);

        return [$u3, $u, $c];
    }

    /**
     * @return int
     * @throws Throwable
     */
    public function getId(): int
    {
        $user = new User();
        $user->setAge(mt_rand(1, 100));
        $user->setUserDesc('desc');

        $user->save();

        return $user->getId();
    }
}