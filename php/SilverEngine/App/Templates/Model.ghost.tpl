<?php echo '<?php'; ?>

namespace App\Models;

use Silver\Core\Model;
/**
 *
 */
class {{{ucfirst($name)}}}Model extends Model
{

    protected $table = '{{ strtolower($name) }}';
    protected $primaryKey = 'id';

    protected $selectable = [

    ];

    protected $fillable = [

    ];

    protected $filterable = [

    ];

    protected $includable = [

    ];

    protected $hidden = [

    ];

    public function get{{$name}}s()
    {
        return $this->select('{{ strtolower($name) }}')->all();
    }
}
