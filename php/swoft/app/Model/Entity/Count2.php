<?php declare(strict_types=1);


namespace App\Model\Entity;


use Swoft\Db\Annotation\Mapping\Column;
use Swoft\Db\Annotation\Mapping\Entity;
use Swoft\Db\Annotation\Mapping\Id;
use Swoft\Db\Eloquent\Model;

/**
 * Class Count
 *
 * @since 2.0
 *
 * @Entity(table="count", pool="db2.pool")
 */
class Count2 extends Model
{
    /**
     * @Id(incrementing=true)
     *
     * @Column(name="id", prop="id")
     * @var int|null
     */
    private $id;

    /**
     * @Column(name="user_id", prop="userId")
     * @var int|null
     */
    private $userId;

    /**
     * @Column(name="create_time", prop="createTime")
     *
     * @var int|null
     */
    private $createTime;

    /**
     * attributes
     *
     * @Column()
     *
     * @var string|null
     */
    private $attributes;

    /**
     * @return null|int
     */
    public function getId(): ?int
    {
        return $this->id;
    }

    /**
     * @param null|int $id
     */
    public function setId(?int $id): void
    {
        $this->id = $id;
    }

    /**
     * @return null|int
     */
    public function getUserId(): ?int
    {
        return $this->userId;
    }

    /**
     * @param null|int $userId
     */
    public function setUserId(?int $userId): void
    {
        $this->userId = $userId;
    }

    /**
     * @return null|int
     */
    public function getCreateTime(): ?int
    {
        return $this->createTime;
    }

    /**
     * @param null|int $createTime
     */
    public function setCreateTime(?int $createTime): void
    {
        $this->createTime = $createTime;
    }

    /**
     * @return null|string
     */
    public function getAttributes(): ?string
    {
        return $this->attributes;
    }

    /**
     * @param null|string $attributes
     */
    public function setAttributes(?string $attributes): void
    {
        $this->attributes = $attributes;
    }
}