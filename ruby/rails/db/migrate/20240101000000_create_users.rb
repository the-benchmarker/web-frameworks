class CreateUsers < ActiveRecord::Migration[8.1]
  def change
    create_table :users do |t|
      t.string :name, null: false
      t.string :email, null: false
      t.string :token
      t.timestamps
    end
    
    add_index :users, :email, unique: true
    add_index :users, :name
    add_index :users, :token, unique: true
  end
end
