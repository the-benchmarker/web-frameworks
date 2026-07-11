class CreatePosts < ActiveRecord::Migration[8.1]
  def change
    create_table :posts do |t|
      t.string :title, null: false
      t.text :content, null: false
      t.references :user, null: false, foreign_key: true
      t.timestamps
    end
    
    add_index :posts, :user_id
    add_index :posts, :title
    add_index :posts, :created_at
  end
end
