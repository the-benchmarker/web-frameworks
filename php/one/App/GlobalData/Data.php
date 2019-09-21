<?php
/**
 * Created by PhpStorm.
 * User: admin
 * Date: 2018/10/12
 * Time: 11:21
 */

namespace App\GlobalData;

class Data
{
    private $data = [];

    private $time = [];

    private $path = '';

    private $limit = [];

    public function __construct($path = '')
    {
        $this->path = $path;
        if (file_exists($path)) {
            list($this->data, $this->time) = msgpack_unpack(file_get_contents($path));
        }
    }

    public function save()
    {
        return file_put_contents($this->path, msgpack_pack([$this->data, $this->time]));
    }


    public function setQueueLimit($k, $limit)
    {
        $this->limit[$k] = $limit;
    }

    public function delQueueLimit($k)
    {
        unset($this->limit[$k]);
    }

    /**
     * @param $key
     * @return array|mixed|null
     */
    public function getAndDel($key)
    {
        $r = $this->get($key);
        $this->del($key);
        return $r;
    }

    /**
     * 在队列末尾追加一个元素
     * @param $k
     * @param $v
     * @return int
     */
    public function push($k, $v)
    {
        $k = trim($k, '.');
        $this->set("{$k}.", $v);
        if (isset($this->limit[$k])) {
            $this->limit($k, $this->limit[$k]);
        }
        return 1;
    }


    private function limit($k, $n)
    {
        $ar = $this->toKeys($k);
        $wr = &$this->data;
        foreach ($ar as $v) {
            if (is_array($wr) && isset($wr[$v])) {
                $wr = &$wr[$v];
            } else {
                return null;
            }
        }
        if (is_array($wr)) {
            while (count($wr) > $n) {
                array_shift($wr);
            }
        }
    }

    /**
     * 将头部元素弹出
     * @param $k
     */
    public function pop($k)
    {
        $ar = $this->toKeys($k);
        $wr = &$this->data;
        foreach ($ar as $v) {
            if (is_array($wr) && isset($wr[$v])) {
                $wr = &$wr[$v];
            } else {
                return null;
            }
        }
        if (is_array($wr)) {
            $r = array_shift($wr);
        } else if (is_string($wr)) {
            $r  = $wr{0};
            $wr = substr($wr, 1);
        } else {
            $r = null;
        }

        if (empty($wr)) {
            $this->del($k);
        }
        return $r;
    }

    /**
     * 数据长度
     * @param $k
     */
    public function length($k)
    {
        $ar = $this->toKeys($k);
        $wr = &$this->data;
        foreach ($ar as $v) {
            if (is_array($wr) && isset($wr[$v])) {
                $wr = &$wr[$v];
            } else {
                return null;
            }
        }
        if (is_array($wr)) {
            return count($wr);
        } else if (is_string($wr)) {
            return strlen($wr);
        } else {
            return 0;
        }
    }

    public function incr($k, $v = 1, $ttl = 0)
    {
        $i = $this->get($k);
        if ($i !== null) {
            $v += $i;
        }
        $this->set($k, $v, $ttl);
        return $v;
    }

    private function gc()
    {
        $i = rand(1, 10);
        if ($i === 8) {
            $t = time();
            foreach ($this->time as $k => $v) {
                if ($v < $t) {
                    $this->del($k);
                }
            }
        }
    }

    /**
     * 设置
     * @param string $key
     * @param mixed $val
     * @return int
     */
    public function set($key, $val, $time = 0)
    {
        $ar  = $this->toKeys($key);
        $br  = $ar;
        $wr  = &$this->data;
        $len = count($ar);
        foreach ($ar as $i => $v) {
            array_shift($br);
            if (is_array($wr) && isset($wr[$v]) && ($i < $len - 1 && is_array($wr[$v]))) {
                $wr = &$wr[$v];
            } else {
                if ($v) {
                    $wr[$v] = $this->join($br, $val);
                } else {
                    $wr[] = $this->join($br, $val);
                }
                return 1;
            }
        }
        if ($wr !== $val) {
            $wr = $val;
        }
        if ($time > time()) {
            $this->time[$key] = $time;
            $this->gc();
        }
        return 1;
    }

    private function join($arr, $v, $i = 0)
    {
        if (isset($arr[$i])) {
            if ($arr[$i]) {
                return [$arr[$i] => $this->join($arr, $v, $i + 1)];
            } else {
                return [$this->join($arr, $v, $i + 1)];
            }
        } else {
            return $v;
        }
    }

    private function toKeys($key)
    {
        return explode('.', $key);
    }


    /**
     * 获取
     * @param string $key
     * @return array|mixed|null
     */
    public function get($key)
    {
        $ar = $this->toKeys($key);
        $wr = &$this->data;
        foreach ($ar as $v) {
            if (is_array($wr) && isset($wr[$v])) {
                $wr = &$wr[$v];
            } else {
                return null;
            }
        }
        if (isset($this->time[$key])) {
            $this->gc();
            if ($this->time[$key] < time()) {
                return null;
            }
        }
        return $wr;
    }

    /**
     * 删除
     * @param string $key
     * @return int
     */
    public function del($key)
    {
        unset($this->time[$key], $this->limit[$key]);
        $ar = $this->toKeys($key);
        $this->_del($ar);
        return 1;
    }

    private function _del($ar, $d = 0)
    {
        $k  = array_pop($ar);
        $wr = &$this->data;
        foreach ($ar as $v) {
            if (is_array($wr) && isset($wr[$v])) {
                $wr = &$wr[$v];
            }
        }

        if (is_array($wr) && isset($wr[$k]) && ($d == 0 || (is_array($wr[$k]) && count($wr[$k]) < 1))) {
            unset($wr[$k]);
        }

        if (count($ar) > 0) {
            $this->_del($ar, ++$d);
        }
    }


    /**
     * @param $fd
     * @param $id
     * @param string $fd_key
     * @param string $id_key
     * @return int
     */
    public function bindId($fd, $id, $fd_key = 'fd', $id_key = 'id')
    {
        $old_name = $this->get("{$fd_key}-{$id_key}.{$fd}");
        if ($old_name) {
            $this->del("{$id_key}-{$fd_key}.{$old_name}");
        }
        $this->set("{$id_key}-{$fd_key}.{$id}.{$fd}", 1);
        $this->set("{$fd_key}-{$id_key}.{$fd}", $id);
        return 1;
    }

    /**
     * @param $fd
     * @param string $fd_key
     * @param string $id_key
     * @return int
     */
    public function unBindFd($fd, $fd_key = 'fd', $id_key = 'id')
    {
        $id = $this->getIdByFd($fd, $fd_key, $id_key);
        $this->del("{$id_key}-{$fd_key}.{$id}.{$fd}");
        $this->del("{$fd_key}-{$id_key}.{$fd}");
        return 1;
    }

    /**
     * 解除绑定
     * @param $id
     * @param string $fd_key
     * @param string $id_key
     * @return int
     */
    public function unBindId($id, $fd_key = 'fd', $id_key = 'id')
    {
        $fds = $this->get("{$id_key}-{$fd_key}.{$id}");
        foreach ($fds as $fd => $v) {
            $this->del("{$fd_key}-{$id_key}.{$fd}");
        }
        $this->del("{$id_key}-{$fd_key}.{$id}");
        return 1;
    }

    /**
     * @param $id
     * @param string $fd_key
     * @param string $id_key
     * @return array
     */
    public function getFdById($id, $fd_key = 'fd', $id_key = 'id')
    {
        $arr = $this->get("{$id_key}-{$fd_key}.{$id}");
        return $arr ? array_keys($arr) : [];
    }


    /**
     * @param $fd
     * @param string $fd_key
     * @param string $id_key
     * @return string
     */
    public function getIdByFd($fd, $fd_key = 'fd', $id_key = 'id')
    {
        return $this->get("{$fd_key}-{$id_key}.{$fd}");
    }

}
