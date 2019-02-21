#+title: Query class
#+author: Matija
#+email: obidm@gmx.com
#+PROPERTY: header-args:php :session *php*
#+PROPERTY: header-args :results output


* About Query

  Query is class which helps us to build SQL query and get result from
  database.

  With using Query class we have next advantages:
  1. All values are properly escaped.
  2. Same query can be executed on different databases.
  3. Easy use of transactions and sub-transactions (aka savepoints)
  4. ...

* Building quries

  #+name: Select query
  #+BEGIN_SRC php
    Query::select('column')
        ->from('table')
        ->where('column2', '=', 'value')
        ->orderBy('column3', 'asc')
        ->limit(2);
  #+END_SRC

  #+RESULTS: Select

  This is an example query which, give us following result:
  
  #+name: Select query SQL (for mysql)
  #+BEGIN_SRC php
    SELECT `column` FROM `table` WHERE `column2` = ? ORDER BY `column3` ASC LIMIT 2;
    -- values: ['value']
  #+END_SRC

  First when we want to create query, we call one of next functions
  which give use specialized Query for our statement:
  - ::select() :: [[System/Database/Query/Select.php][Select]]
  - ::delete() :: [[System/Database/Query/Delete.php][Delete]]
  - ::update() :: [[System/Database/Query/Update.php][Update]]
  - ::insert() :: [[System/Database/Query/Insert.php][Insert]]
  - ::drop() :: [[System/Database/Query/Drop.php][Drop]]
  - ::create() :: [[System/Database/Query/Create.php][Create]]
  - ::alter() :: [[System/Database/Query/Alter.php][Alter]]


  When we get query, we methods of this query. Because more queries
  uses same methods, they are stored separated and included as Traits.

** Query methods
   
   Each provided method accept values which are known as Query
   parts. Query part type is written before variable name (only in
   this documentation).

   What can we pass for Query part is described in [[Query parts]]
   chapter.
   
*** From
    
    - from($table, $alias = null, Column $id1 = null, Column $id2 = null) ::
	 From which table we select data.
    #+name: From example
    #+BEGIN_SRC php
      $q->from('table');
      $q->from('table', 't');
      $q->from('table t');
      $q->from('table2', 't2', 't1.id', 't2.id_table1');
      $q->from('table2 t2', 't1.id', 't2.id_table1');
      $q->from('table2', 't1.id', 'table2.id_table1');
    #+END_SRC

    #+RESULTS: From

    We can pass subquery too:
    
    #+name: From subquery
    #+BEGIN_SRC php
      $q->from(Query::select()->from('other'), 'alias_must_exists');
      // or
      $q->from(function() {
          return Query::select()->from('tbl')->whereActive(true);
      }, 'alias');
    #+END_SRC


*** Join

    - join(Table $table, JoinCondition $condition) :: Inner join table
    - leftJoin(Table $table, JoinCondition $condition) :: Left join table
    - rightJoin(Table $table, JoinCondition $condition) :: Right join table

	 
    
    #+name: Join example
    #+BEGIN_SRC php
      $q->join('table', ['table.id', 'other_table.id']);
    #+END_SRC

    #+RESULTS: Join

***  Where and Having

     - where(Column $column, Raw $operator, Value $value, string $how = 'and', boolean $not = false)
     - having(Column $column, Raw $operator, Value $value, string $how = 'and', boolean $not = false)

       
     
     #+name: where/having
     #+BEGIN_SRC php
       $q->where('column', '=', 'value');
       $q->where('column', 'between', [10, 20], 'or', true); // or not between 10-20
     #+END_SRC

     #+RESULTS: where/having

       
     We can skip operator and write value instead (but in this case,
     we cant modify $how and $not values)
       
     
     #+name: short notation
     #+BEGIN_SRC php
       $q->where('column', 'value'); // column = value
     #+END_SRC

     #+RESULTS: short


     __call() function simplify where call for us. We can concatenate
     more words for method name to get what we want: (Order is important!)
     - or :: if we want connect this condition with OR (optional)
     - not :: if we want negate this condition (optional)
     - where / having :: one of them
     - column name :: Column name in camelCase will be transformed
                      into snake_case (optional)
		      
		      
     When we call this special method, we need to pass folowing arguments:
     - column (if it is not passed yet)
     - operator (required)
     - value (required)

       
     
     #+name: Special syntax
     #+BEGIN_SRC php
       $q->whereId(1); // operator is skipped
       $q->whereId('=', 1); // same thing
       $q->whereAge('between', [10, 20]); // AND age between 10 and 20
       $q->notWhereAge('between', [10, 20]); // AND NOT age between 10 and 20
       $q->orNotWhereAge('between', [10, 20]); // OR NOT age between 10 and 20
       $q->orWhere('specialColumn', 3); // AND specialColumn = 3
     #+END_SRC

**** Operators
     
     Default operators are:
     - IN :: If value is array
     - IS :: If value is null
     - = :: For all other values


     Special operators:
     - BETWEEN :: it needs an array of two values for value

     All other operators are transformed to upper case. Operator is
     not escaped or treated specially. It is concatenated to SQL
     statement like raw value. We can write into whatever we want
     (spaces and comment too... please don't do any stupid)

**** Special use of where - parentheses

     If we want determine operators priority, we can use where()
     function to put conditions in parentheses.

     
     #+name: Parens example
     #+BEGIN_SRC php
       // WHERE active = true AND age = 10 OR age = 11
       $q->whereActive(true)
           ->whereAge(10)
           ->orWhereAge(11);

       // WHERE active = true AND (age = 10 OR age = 11)
       $q->whereActive(true)
           ->where(function ($q) {
               $q->whereAge(10)
                   ->orWhereAge(11);
           });

       // High order function example
       function find_best($sex) { return function($q) use ($sex) { /* ... */ } }
       $q->where(find_best('female'));
     #+END_SRC

     In this example, all where conditions (not having) is putted into
     parens. In inner function we still can do whatever we want with
     query (order by, having, limit, ...), if we really want this...
*** GroupBy

    If we use having from previous chapter, we need first group some
    data.

    - groupBy(Column $column)

      
    This is it. If we want group more columns, we can call groupBy()
    more times.

*** Order

    - orderBy(Name $column, $dir = 'asc')

      
    
    #+name: Order example
    #+BEGIN_SRC php
      $q->orderBy('model')
          ->orderBy('year', 'desc');

      // ORDER BY model asc, year desc
    #+END_SRC

*** Limit

    - limit(int $count, $offset = null) :: Standard limit
    - offset(int $offset) :: If limit is not specified yet it will be set to 1
    - page(int $page, int $per_page = null) :: First argument is page
         number (starting with 1), second argument is page size.
	 

    
    #+name: Limitations
    #+BEGIN_SRC php
      $q->limit(4); // LIMIT 4
      $q->limit(4, 6); // LIMIT 4 OFFSET 5
      $q->offset(3); // LIMIT 1 OFFSET 3
      // ^ Actually limit should be 4, becouse we set it in second row

      /* Page size = 10
       ,*
       ,* Page:
       ,* 1. 0-9
       ,* 2. 10-19
       ,* 3. 20-29
       ,*/ 
      $q->page(3, 10); // LIMIT 10 OFFSET 20
    #+END_SRC

*** Union
    
    - union(Query $q)
    - unionAll(Query $q)


    
    #+name: Union
    #+BEGIN_SRC php
      $q->union(Query::select()->from('table')->limit(1));

      // If we are more comfortable with callback, we can use it:
      $q->union(function() {
          return Query::select()
              ->from('table')
              ->limit(1);
      });
    #+END_SRC
** Query parts

   Query parts are stored in [[System/Database/Parts/]]. We can construct
   them with $part = OutPart::ensure(['construct', 'values']); or via
   constructor $part = new OurPart('construct', 'values');

   If Part is defined before Query method parameter (in this doc),
   value will be passed through Part::ensure($value) function. If
   value is already part (maybe Raw), ensure will not touch it,
   otherwise value will be constructed into declared Part.

   
   #+name: Ensure examples
   #+BEGIN_SRC php
     Column::ensure('col'); /* is same as */ Column::ensure(['col']);

     $q->where('col', '=', 24);

     /*
      ,* So, because where is declared as: where(Column $c, $op, Value $v)
      ,* 'col' will be transformed with Column::ensure('col')
      ,* and 24 with Value::ensure(24);
      ,*/

     // If we want create Column like: new Column('table', 'col', 'alias');
     $q->where(['table', 'col'], ...);

     // becouse we can construct Column like: new Column('table.col alias'):
     $q->where('table col');

     // If we pass our own part into where, Column::ensure will skip it
     $q->where(new Raw('COUNT(*)'), 5)
   #+END_SRC

   We can pass alias into where column too, but we don't want to.

   
*** Raw
    
    Arguments: (string $raw_value)
    
    In raw we pass raw sql.
    
    #+name: Raw example
    #+BEGIN_SRC php
      $q->select(new Raw('COUNT(*) count'))->from('table');
    #+END_SRC

*** Literal

    Arguments: (mixed $value)

    Literal ensure that passed value will be properly
    quoted. Following methods are predefined:
    - Literal::wild() :: new Raw('*')
    - Literal::null() :: new Literal(null)
    - Literal::true() :: new Literal(true)
    - Literal::false() :: new Literal(false)


    We can pass into literal any value, and it will appear in sql
    statement:

    #+name: Literal
    #+BEGIN_SRC php
      $q->select(Literal::wild())
          ->where('active', Literal::true())
          ->notWhereLastLogin(Literal::null())
          ->where('string', new Literal('this is string'))
          ->where('age', 'IN', new Literal([20, 21, 22])); // We must write
                                                           // IN operator,
                                                           // becouse
                                                           // Literal is not
                                                           // recognized as
                                                           // array
      // SELECT * FROM ? WHERE active = true AND NOT WHERE last_login = null AND string = 'this is string' AND age IN [20, 21, 22]
      // We should specify IS operator for NULL too!
    #+END_SRC

    If we doesn't use Literal, Query will transform values into Value
    part, which uses placeholders.

    #+name: Literal
    #+BEGIN_SRC php
      $q->select(Literal::wild())
          ->where('active', true)
          ->notWhereLastLogin(null)
          ->where('string', 'this is string')
          ->where('age', [20, 21, 22]);
      // SELECT * FROM ? WHERE active = ? AND NOT WHERE last_login IS ? AND string = ? AND age IN [?, ?, ?]
    #+END_SRC

*** Value

    Arguments: (mixed $value)

    All values passed into Query are transformed into Value. Value is
    stored into Query bindings and '?' placeholder is inserted into
    SQL statement.

*** Fn

    Arguments: (string $name, ...$args)

    Function call:
    - Fn::count(Column $column = null) :: (default is Literal::wild())
    - Fn::groupConcat(Column $column, string $sep = ',') :: GROUP_CONCAT function
	 
	 
    Other function call can be constructed via special __call() method like:
    - Fn::myOwnFunction(1,2,3) :: myOwnFunction(1,2,3);
    - Fn::THIS_IS_FN(1,'string',new Value(24)) :: THIS_IS_FN(1,'string',?);

*** Column

    Arguments: (string $table_or_column,  string $column = null, string $alias = null)

    Column is part which represent column name with table (optional)
    and alias (optional).

    We can construct it in more ways:
    - new Column('table', 'column', 'alias')
    - new Column('table', 'column')
    - new Column(null, 'column', 'alias')
    - new Column('table.column alias')
    - new Column('table.column', null, 'alias')


    Becouse what we give into query, is passed through
    Column::ensure() function, we can define column in next ways:
    
    #+name: Query column
    #+BEGIN_SRC php
      $q->select('table.column alias', ['table', 'column'], ['t', 'c', 'als'], [null, 'col', 'alias'], 'col alias');
    #+END_SRC

*** Name

    Arguments: (string $name)

    Name is like Column, but it doesn't have table and alias. It is
    used for column and table names.

*** JoinCondition

    Arguments: (Name $col1, $operator = null, Name $col2)

    Join condition can be ON (c1 = c2) or USING (c).
    
    #+name: JoinCondition
    #+BEGIN_SRC php
      echo new JoinCondition('col1', '=', 'col2'); // ON `col` = `col2`
      echo new JoinCondition('col1', 'col2'); // same thing
      echo new JoinCondition('col'); // USING (`col`)
    #+END_SRC

    Column names are automatically escaped, becouse they are
    transformed into Name part.

*** Table

    Arguments: (Name or Query $table,  Name $alias = null)

    Table is used to specify source of data. It is used in join
    method.

    
    #+name: Table example
    #+BEGIN_SRC php
      $q->join(new Table(Query::select()->from('tbl'), 'alias'));
      $q->join(new Table('table', 'alias'));
      $q->join(new Table('tbl', 'alias'));
      $q->join(new Table('tbl'));

      // Becouse join already make Table part, we can skip new Table:
      $q->join([Query::select()->from('tbl'), 'alias']);
      $q->join(['table', 'alias']);
      $q->join(['tbl', 'alias']);
      $q->join(['tbl']); /* same as */ $q->join('tbl');

    #+END_SRC
** Inserting data

   1. We can insert full row: $data1 = [1, 2, 'value'];
   2. We can insert key=>value row with default data: $data2 = ['text' => 'value'];

      
   #+name: Inserting
   #+BEGIN_SRC php
     Query::insert('table', $data1);
     Query::insert('table', $data2);
     // Dont forget to execute query...

     // We can insert even more data at once
     Query::insert('table', [$data1, $data1, $data1]); // id conflict with maybe
     Query::insert('table', [$data2, $data2, $data]);

     // But all data in query must have same format (full-row or key-value)
   #+END_SRC

      
** Updating data

   #+name: Updating data
   #+BEGIN_SRC php
     $q = Query::update('table', [
         'col' => 'new value',
         'col2' => 'new value',
         'col3' => Fn::CONCAT('col4', 'col5')
     ]);

     // if we forget something
     $q->set('ups', null);

     // Add filter
     $q->whereId(3);
   #+END_SRC

   For filters documentation check [[Where and Having]] chapter.
   

* Working with schema
  
  For manipulating database schema, we have next queries:
  - ::drop() :: [[System/Database/Query/Drop.php][Drop]]
  - ::create() :: [[System/Database/Query/Create.php][Create]]
  - ::alter() :: [[System/Database/Query/Alter.php][Alter]]

** Droping table
   
   #+name: Drop example
   #+BEGIN_SRC php
     Query::drop('table');
     Query::drop('table')->ifExists();
   #+END_SRC

** Creating table
   
   #+name: Create new table
   #+BEGIN_SRC php
     $q = Query::create('table_name', function($q) {
         $q->integer('id')->primary();
         // ... other column definitions
     });

     $q->ifNotExists();
     // other table properties
   #+END_SRC

*** Column types
    
    Query support next column types:
    #+name: Column definitions
    #+BEGIN_SRC php
      $q->boolean('true_false');
      $q->enum('sex', 'male', 'female', 'alien');
      $q->set('digits', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      $q->smallInt('small');
      $q->mediumInt('medium');
      $q->integer('int');
      $q->bigInt('big');
      $q->decimal('decimal', $precision = 10, $scale = 5);
      $q->vachar('varchar', 255);
      $q->varchar('text');
      $q->timestamp('timestamp');
      $q->time('time');
      $q->date('date');
      $q->datetime('date_time');
      $q->year('year');
    #+END_SRC

    Some of them are not supported on all databases. For enum and set
    are used varchar on sqlite database. (Check
    [[Database/Parts/Sqlite/ColumnDef.php]] implementation.)

*** Column modifiers
    
    Columns can have next modifiers:
    
    #+name: Column modifiers
    #+BEGIN_SRC php
      $col = $q->integer('int_col');
      $col->unsigned(); // unsigned value
      $col->nullable(); // default is not null
      $col->default(24); // default is null
      $col->primary(); // primary key
      $col->autoincrement();
      $col->unique(); // unique index
    #+END_SRC

*** Column references
    
    We can make reference to other column:
    
    #+name: References
    #+BEGIN_SRC php
      Query::create('table', function($q) {
          $q->integer('id')->primary()->autoincrement();
      });

      Query::create('table2', function($q) {
          $q->integer('id')->primary()->autoincrement();
          $q->integer('id_table')->references('table.id');
      });
    #+END_SRC

*** Table modifiers

    Table can have modifiers too:

    #+name: Table modifiers
    #+BEGIN_SRC php
      $table = Query::create('table', function($q) { /* ... */ });
      $table->temporary(); // create table in memory
      $table->ifNotExists(); // skip if already exists

      $table->option('key', 'value'); // custom option KEY VALUE

      // Some of options are predefined
      $table->engine('MyIsam'); // ENGINE=MyIsam
      $table->charset('utf-8'); // CHARSET SET 'utf-8'
      $table->defaultCharset('utf-8'); // DEFAULT CHARSET SET 'utf-8'
    #+END_SRC

** Alter table

   Alter is buggy and untested, please report bugs.

   #+name: Add new column
   #+BEGIN_SRC php
     Query::alter('table')->addColumn('new_column', 'varchar', 255);
     Query::alter('table')->addColumn('new_int_col', 'integer')->nullable();
   #+END_SRC
   
   #+name: Modify column
   #+BEGIN_SRC php
     Query::alter('table')->modifyColumn('existing_column', 'text')->nullable(false);
     Query::alter('table')->modifyColumn('existing_int_column', 'bigInt')->default(123);
   #+END_SRC

   #+name: Change column
   #+BEGIN_SRC php
     Query::changeColumn('existing_column', 'new_name', 'varchar', 255)->nullable();
   #+END_SRC

* Executing queries

  If we want execute query without needed any data in result, we
  simply call:
  
  #+name: execute
  #+BEGIN_SRC php
    $q->execute();
  #+END_SRC

  Otherwise we use method ->fetch() to get next result. If we want all
  results in one call we use method ->fetchAll() which works exactly
  same way exept it returns all result in one array.

  Fetch has first argument for specify which columns we want in
  result. If single column is passed single value is returned (scalar)
  if array is passed, array of values is returned.

  
  #+name: Fetching data with get()
  #+BEGIN_SRC php
    $q->select('type', 'count')->from('table');

    // Get first column
    while ($type = $q->single()) {
        echo "Type: $type\n";
    }

    // Get one (named) column
    while ($res = $q->get('count')) {
        echo "One column (count): $count\n";
    }

    // Get more columns
    while ($res = $q->get(['type', 'count'])) {
        echo "{$res['type']} = {$res['count']}\n";
    }

    // Get row as object
    while ($res = $q->get()) {
        echo "{$res->type} = {$res->count}\n";
    }

  #+END_SRC

  
  #+name: Fetching data with all()
  #+BEGIN_SRC php
    $q->select('type', 'count')->from('table');

    // Get first column
    foreach ($q->singleAll() as $type) {
        echo "Type: $type\n";
    }

    // Get one (named) column
    foreach ($q->all('count') as $res) {
        echo "One column (count): $count\n";
    }

    // Get more columns
    foreach ($q->all(['type', 'count']) as $res) {
        echo "{$res['type']} = {$res['count']}\n";
    }

    // Get row as object
    foreach ($q->all() as $res) {
        echo "{$res->type} = {$res->count}\n";
    }


    // Custom result transformation
    $results = $q->all(['type', 'count'], function ($row) {
        return $row['type'] . ' => ' . $row['count'];
    });
    echo implode("\n", $results);
  #+END_SRC

* Working with objects

  PHP PDO library has option to pack received data into an object. So
  if we use this feature we can fetch data like this:
  
  #+name: Query fetch class
  #+BEGIN_SRC php
    $class = $q->fetch(MyClass::class);
    assert($class instanceof MyClass);
  #+END_SRC

  But, we want using object to represent one row in database. For this
  purpose each class must have defined table name and primary key.

** Using objects

  Table name is class name in snake_case and primary key is 'id'. We
  can change that with implement methods which returns table name and
  primary key.

  
  #+name: Custom table name and primary key
  #+BEGIN_SRC php
    class MyTable {
        public static function tableName() {
            return 'my_table_v2';
        }

        public static function primaryKey() {
            return 'id_my_table';
        }
    }
  #+END_SRC

  Even if class doesn't have these two methods, we can accessing to
  its table name and primary. But it must extends from QueryObject
  class.

  #+name: QueryObject
  #+BEGIN_SRC php
    class QueryObject {
        public static function primaryKey() {
            return 'id';
        }

        public static function tableName() {
            return snake_case(static::class);
        }
    }
  #+END_SRC


  When we done this, we doesn't need to remember table names any more:
  
  #+name: Fetching objects
  #+BEGIN_SRC php
    Query::select()
        ->from(User::class)
        ->whereId(3)
        ->fetch(User::class);
  #+END_SRC

  NOTE: id is still column `id`. If we want find user by primary key,
  we should write: ->where(user::primaryKey(), 3)

  In background Name and Column part check if table is a class and
  replace it with Class::tableName().

  
  #+name: Accessing columns
  #+BEGIN_SRC php
    $q = Query::select('Obj.id objid', 'name')
       ->from(Obj::class)
       ->join(Obj2::class, ['Obj2.id_obj', 'Obj.id']) // id is not necessary primary
       ->where('Obj2.something', true)
       ->orderBy('Obj2.sort')
       ->fetch(['objid', 'name']);
  #+END_SRC

  There is only one problem. Obj::class could have namespaces
  A\B\Obj. In this case, Query will not found Obj class, because it
  doesn't exists in Query package. So, when new object is declared (in
  from or join method), Query will create alias in one of his sub
  namespaces. When column is referencing a class, Query will look in
  this namespace if it exists.

** Name conflicts                                                     :CHECK:
  Maybe you think, what if I use two classes with same name in
  different namespaces?
  
  #+name: Conflict example
  #+BEGIN_SRC php
    namespace General {
        class User { } // tablename = users
    }

    namespace Deleted {
        class User { } // tablename = deleted_users
    }

    Query::select()
        ->from(General\User::class)
        ->from(Deleted\User::class)
        ->where('User.id', 3); // which one?
  #+END_SRC

  If you really want to have same object names, you can create
  class_alias(Existing::class, 'NewName') and use alias.

  There is no problem using objects with same names on different
  databases. Query handle that properly.
  
  #+name: Same named objects
  #+BEGIN_SRC php
    $q1 = Query::select()
        ->from(mysql\User::class)
        ->where('User.id', 1); // referencing Query\Aliases\Mysql\User -> mysql\User

    $q2 = Query::select()
        ->from(sqlite\User::class)
        ->where('User.id', 1); // referencing Query\Aliases\Sqlite\User -> sqlite\User

    Query::withConnection('mysql', function() {
        $q1->fetch();
    });

    Query::withConnection('sqlite', function() {
        $q2->fetch();
    });
  #+END_SRC

  Different connections will have different namespaces (eg:
  Query\Aliases\<Connection>\<Alias>) for used object aliases. But one
  connection must have unique names for objects.


  This was a long chapter.
  
** TODO Joining with objects

   Joins from previous chapter works, but they are not good. We still
   wneed to know relation between objects, their table names and their
   id columns.

   #+name: Join
   #+BEGIN_SRC php :exports code
     Query::select()
         ->from(User::class)
         ->join(Group::class, ['group.id', 'user.group_id']);
   #+END_SRC

   If we want write this right, we should do it in that way:
   
   #+name: Join 2
   #+BEGIN_SRC php :exports code
     Query::select()
         ->from(User::class)
         ->join(Group::class, [Group::class . '.' . Group::primaryKey(),
                               User::class . '.group_id']);
   #+END_SRC

   Uff... and we still need to know `group_id' column everywhere in
   code.

*** TODO Relation definitions

    Basic idea is to write join definition into model and then use it
    all over the code.
    
    #+name: Reference example
    #+BEGIN_SRC php :exports code
      class User extends QueryObject {
          protected class refGroup() {
              return Reference::toOne(Group::class, 'id_group');
          }
      }

      class Group extends QueryObject {
          protected class refUsers() {
              return Reference::toMany(User::class, 'id_group');
          }
      }
    #+END_SRC

    Now we can start joining:
    
    #+name: Joins
    #+BEGIN_SRC php :exports code
      Query::select('g.name')
          ->from(User::class)
          ->join('User.group g');
      // and
      Query::count()
          ->from(Group::class, 'g')
          ->join('Group.users')
          ->where('g.name', 'Guest')
          ->single();
    #+END_SRC

    Problems:
    - [ ] Table can be aliased
    - [ ] ...

**** Devel
     
     #+name: Model example
     #+BEGIN_SRC php
       class Model {
           public function tableName() {
               return 'ime_tabele';
           }
           public function primary() {
               return 'ID_ime_tabele';
           }
       }
     #+END_SRC

     #+RESULTS: Model
     : // NULL

     #+name: Query with sources
     #+BEGIN_SRC php 
       class Query {
           private $components = [];   // Unused...
           private $bindings = [];
           private $sources = [
               'table or alias' => 'source info',
               'alias' => 'table_name',
               'table_name2' => 'table_name2',
               // 'g' => new Source('groups', 'g')
           ];
           private $select = [
               'name' => 'whatever(*)',
               'count' => new Column(),
               'xxx' => new Raw()
           ];
       }
     #+END_SRC

     #+RESULTS: Query
     : [8]     *> [8]     *> [8]     *> [8]     *>

     
     #+name: Source
     #+BEGIN_SRC php :exports code
              abstract class Source {
                  private $name;

                  public function __construct($name) {
                      $this->name = $name;
                  }

                  public function name() {
                      return $this->name;
                  }

                  abstract public function primary();
                  abstract public function table();

                  public function debug() {
                      echo "Table: `" . $this->name() . "` primary: `" . $this->primary() . "`\n";
                  }
              }
     #+END_SRC

     #+RESULTS: Source
     : PHP Fatal error:  Cannot redeclare class Source in /home/matija/Documents/code/php/boris/lib/Boris/EvalWorker.php(152) : eval()'d code on line 1

     #+name: TableSource
     #+BEGIN_SRC php 
       class TableSource extends Source {
           private $table;

           public function __construct($table, $name = null) {
               parent::__construct($name ?: $table);
               $this->table = $table;
           }

           public function primary() {
               return 'id';
           }

           public function table() {
               return $table;
           }
       }
     #+END_SRC

     #+RESULTS: TableSource
     : // NULL

       
     #+name: ModelSource
     #+BEGIN_SRC php 
       class ModelSource extends TableSource {
           private $model;

           public function __construct($model, $name = null) {
               $this->setModel($model);
               parent::__construct($model::tableName(), $name);
           }

           private function setModel($model) {
               // if (class_exists($model) and is_subclass_of($model, QueryObject::class)) {
               if (class_exists($model)) {
                   $this->model = $model;
               } else {
                   throw new Exception("Wrong class name '$model'"); // Should be QueryException
               }
           }

           public function primary() {
               $m =  $this->model;
               return $m::primary();
           }

           public function table() {
               $m = $this->model;
               return $m::tableName();
           }
       };
     #+END_SRC

     #+RESULTS: ModelSource
     : PHP Fatal error:  Cannot redeclare class ModelSource in /home/matija/Documents/code/php/boris/lib/Boris/EvalWorker.php(152) : eval()'d code on line 27


     #+name: Usage
     #+BEGIN_SRC php :exports both
       $s1 = new ModelSource(Model::class);
       $s2 = new ModelSource(Model::class, 'alias');
       $s3 = new TableSource('tabela', 'alias');
       echo "\n";
       $s1->debug();
       $s2->debug();
       $s3->debug();
     #+END_SRC

     #+RESULTS: Usage
     : Table: `ime_tabele` primary: `ID_ime_tabele`
     : Table: `alias` primary: `ID_ime_tabele`
     : Table: `alias` primary: `id`



***** Query

      #+BEGIN_SRC php
        class Query {
            // private $components = [];   // Unused...
            private $bindings = [];
            private $sources = [
                'table or alias' => 'source info',
                'alias' => 'table_name',
                'table_name2' => 'table_name2',
                // 'g' => new Source('groups', 'g')
            ];

            // private $select = [
            //     'name' => 'whatever(*)',
            //     'count' => new Column(),
            //     'xxx' => new Raw()
            // ];

            public function from($source, $alias = null) {
                if (class_exists($source)) {
                    // it is model
                    $source = new ModelSource($source, $alias);
                } else {
                    $source = new TableSource($source, $alias);
                }

                $sources[$source->name()] = $source;
            }
        }

      #+END_SRC
** TODO Relations
   
   - Note taken on [2017-05-01 Mon 21:12] \\
     Popravi query, da bo selektal samo prvo from tabelo (v primeru
     modelov)
   - Note taken on [2017-05-01 Mon 21:11] \\
     'id' v querijih ni nujno primarni kljuc

     
   #+name: Relation definition
   #+BEGIN_SRC php

     class Person as QueryObject {
         public static function relGroup() {
             return self::reference()
                 ->hasOne(Group::class, 'group_id');
         }

         public static function relPosts() {
             return self::reference()
                 ->hasMany(Post::class, 'person_id');
         }
     }
   #+END_SRC

   #+name: Joining with relations
   #+BEGIN_SRC php
     Query::select()
         ->from(Person::class)
         ->join('Person.group g')
         ->join('Person.posts posts')
         ->groupBy(Person::class)
         ->where('g.name', 'Regular user')
         ->having(Fn::count('posts.!id!'), '>', 10)
         ->all();
   #+END_SRC
* QueryObject

  If we want using objects in Query, they must extends QueryObject. If
  we want different tablename or primary key for object, we can
  override tableName() or primaryKey() function.

  In case we want totaly different rules to generate tablenames and
  primary keys, we can use our own QueryObject class which extends
  original QueryObject.

  #+name: QueryObject
  #+BEGIN_SRC php
    class QueryObject {
        public static function tableName() {
            return camel_case(drop_namespace(QueryObject::class));
        }

        public static function primaryKey() {
            return 'id';
        }
    }
  #+END_SRC
