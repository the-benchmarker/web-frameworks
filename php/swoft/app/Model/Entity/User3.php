<?php declare(strict_types=1);


namespace App\Model\Entity;


use Swoft\Db\Annotation\Mapping\Column;
use Swoft\Db\Annotation\Mapping\Entity;
use Swoft\Db\Annotation\Mapping\Id;
use Swoft\Db\Eloquent\Model;


/**
 * Class User
 *
 * @since 2.0
 *
 * @Entity(table="user", pool="db3.pool")
 */
class User3 extends Model
{
    /**
     * @Id()
     *
     * @Column(name="id", prop="id")
     * @var int|null
     */
    private $id;

    /**
     * @Column()
     * @var string|null
     */
    private $name;

    /**
     * @Column(name="password", hidden=true)
     * @var string|null
     */
    private $pwd;

    /**
     * @Column()
     *
     * @var int|null
     */
    private $age;

    /**
     * @Column(name="user_desc", prop="udesc")
     * @var string|null
     */
    private $userDesc;

    /**
     * @return int|null
     */
    public function getId(): ?int
    {
        return $this->id;
    }

    /**
     * @param int|null $id
     */
    public function setId(?int $id): void
    {
        $this->id = $id;
    }

    /**
     * @return int|null
     */
    public function getAge(): ?int
    {
        return $this->age;
    }

    /**
     * @param int|null $age
     */
    public function setAge(?int $age): void
    {
        $this->age = $age;
    }

    /**
     * @return string|null
     */
    public function getName(): ?string
    {
        return $this->name;
    }

    /**
     * @param string|null $name
     */
    public function setName(?string $name): void
    {
        $this->name = $name;
    }

    /**
     * @return string|null
     */
    public function getPwd(): ?string
    {
        return $this->pwd;
    }

    /**
     * @param string|null $pwd
     */
    public function setPwd(?string $pwd): void
    {
        $this->pwd = $pwd;
    }

    /**
     * @return string|null
     */
    public function getUserDesc(): ?string
    {
        return $this->userDesc;
    }

    /**
     * @param string|null $userDesc
     */
    public function setUserDesc(?string $userDesc): void
    {
        $this->userDesc = $userDesc;
    }
}