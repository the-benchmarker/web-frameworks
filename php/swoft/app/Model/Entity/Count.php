<?php declare(strict_types=1);


namespace App\Model\Entity;

use Swoft\Db\Annotation\Mapping\Column;
use Swoft\Db\Annotation\Mapping\Entity;
use Swoft\Db\Annotation\Mapping\Id;
use Swoft\Db\Eloquent\Model;


/**
 *
 * Class Count
 *
 * @since 2.0
 *
 * @Entity(table="count")
 */
class Count extends Model
{
    protected const UPDATED_AT = 'update_time';
    protected const CREATED_AT = 'create_time';

    /**
     *
     * @Id()
     * @Column()
     * @var int|null
     */
    private $id;

    /**
     *
     *
     * @Column(name="user_id", prop="userId")
     * @var int|null
     */
    private $userId;

    /**
     *
     *
     * @Column(name="create_time", prop="createTime")
     * @var int|null
     */
    private $createTime;

    /**
     *
     *
     * @Column()
     * @var string|null
     */
    private $attributes;

    /**
     *
     *
     * @Column(name="update_time", prop="updateTime")
     * @var string|null
     */
    private $updateTime;


    /**
     * @param int|null $id
     *
     * @return void
     */
    public function setId(?int $id): void
    {
        $this->id = $id;
    }

    /**
     * @param int|null $userId
     *
     * @return void
     */
    public function setUserId(?int $userId): void
    {
        $this->userId = $userId;
    }

    /**
     * @param int|null $createTime
     *
     * @return void
     */
    public function setCreateTime(?int $createTime): void
    {
        $this->createTime = $createTime;
    }

    /**
     * @param string|null $attributes
     *
     * @return void
     */
    public function setAttributes(?string $attributes): void
    {
        $this->attributes = $attributes;
    }

    /**
     * @param string|null $updateTime
     *
     * @return void
     */
    public function setUpdateTime(?string $updateTime): void
    {
        $this->updateTime = $updateTime;
    }

    /**
     * @return int|null
     */
    public function getId(): ?int
    {
        return $this->id;
    }

    /**
     * @return int|null
     */
    public function getUserId(): ?int
    {
        return $this->userId;
    }

    /**
     * @return int|null
     */
    public function getCreateTime(): ?int
    {
        return $this->createTime;
    }

    /**
     * @return string|null
     */
    public function getAttributes(): ?string
    {
        return $this->attributes;
    }

    /**
     * @return string|null
     */
    public function getUpdateTime(): ?string
    {
        return $this->updateTime;
    }

}
