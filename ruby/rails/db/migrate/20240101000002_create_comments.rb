class CreateComments < ActiveRecord::Migration[8.1]
  def change
    create_table :comments do |t|
      t.text :content, null: false
      t.references :user, null: false, foreign_key: true
      t.references :post, null: false, foreign_key: true
      t.timestamps
    end
    
    add_index :comments, :user_id
    add_index :comments, :post_id
    add_index :comments, :created_at
  end
end
