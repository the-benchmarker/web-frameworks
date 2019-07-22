<?php declare(strict_types=1);


namespace App\Model\Entity;

use Swoft\Db\Annotation\Mapping\Column;
use Swoft\Db\Annotation\Mapping\Entity;
use Swoft\Db\Annotation\Mapping\Id;
use Swoft\Db\Eloquent\Model;


/**
 * 
 * Class User
 *
 * @since 2.0
 *
 * @Entity(table="user")
 */
class User extends Model
{
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
     * @Column()
     * @var string
     */
    private $name;

    /**
     * 
     *
     * @Column()
     * @var int
     */
    private $age;

    /**
     * 
     *
     * @Column(hidden=true)
     * @var string
     */
    private $password;

    /**
     * 
     *
     * @Column(name="user_desc", prop="userDesc")
     * @var string
     */
    private $userDesc;

    /**
     * 
     *
     * @Column()
     * @var int|null
     */
    private $add;

    /**
     * 
     *
     * @Column()
     * @var int|null
     */
    private $hahh;

    /**
     * 
     *
     * @Column(name="test_json", prop="testJson")
     * @var array|null
     */
    private $testJson;


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
     * @param string $name
     *
     * @return void
     */
    public function setName(string $name): void
    {
        $this->name = $name;
    }

    /**
     * @param int $age
     *
     * @return void
     */
    public function setAge(int $age): void
    {
        $this->age = $age;
    }

    /**
     * @param string $password
     *
     * @return void
     */
    public function setPassword(string $password): void
    {
        $this->password = $password;
    }

    /**
     * @param string $userDesc
     *
     * @return void
     */
    public function setUserDesc(string $userDesc): void
    {
        $this->userDesc = $userDesc;
    }

    /**
     * @param int|null $add
     *
     * @return void
     */
    public function setAdd(?int $add): void
    {
        $this->add = $add;
    }

    /**
     * @param int|null $hahh
     *
     * @return void
     */
    public function setHahh(?int $hahh): void
    {
        $this->hahh = $hahh;
    }

    /**
     * @param array|null $testJson
     *
     * @return void
     */
    public function setTestJson(?array $testJson): void
    {
        $this->testJson = $testJson;
    }

    /**
     * @return int|null
     */
    public function getId(): ?int
    {
        return $this->id;
    }

    /**
     * @return string
     */
    public function getName(): string
    {
        return $this->name;
    }

    /**
     * @return int
     */
    public function getAge(): int
    {
        return $this->age;
    }

    /**
     * @return string
     */
    public function getPassword(): string
    {
        return $this->password;
    }

    /**
     * @return string
     */
    public function getUserDesc(): string
    {
        return $this->userDesc;
    }

    /**
     * @return int|null
     */
    public function getAdd(): ?int
    {
        return $this->add;
    }

    /**
     * @return int|null
     */
    public function getHahh(): ?int
    {
        return $this->hahh;
    }

    /**
     * @return array|null
     */
    public function getTestJson(): ?array
    {
        return $this->testJson;
    }

}
