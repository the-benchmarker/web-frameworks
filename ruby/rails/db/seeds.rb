# This file should contain all the record creation needed to seed the database with its
# default values.
# The data can then be loaded with the bin/rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: "Star Wars" }, { name: "Lord of the Rings" }])
#   Character.create(name: "Luke", movie: movies.first)

# Create sample users
users = User.create!([
  { name: "Alice Johnson", email: "alice@example.com", token: SecureRandom.uuid },
  { name: "Bob Smith", email: "bob@example.com", token: SecureRandom.uuid },
  { name: "Charlie Brown", email: "charlie@example.com", token: SecureRandom.uuid },
  { name: "Diana Prince", email: "diana@example.com", token: SecureRandom.uuid },
  { name: "Eve Wilson", email: "eve@example.com", token: SecureRandom.uuid }
])

puts "Created #{users.size} users"

# Create sample posts
posts = Post.create!([
  { title: "First Post", content: "This is the first sample post content.", user: users[0] },
  { title: "Second Post", content: "This is the second sample post content.", user: users[1] },
  { title: "Third Post", content: "This is the third sample post content.", user: users[2] },
  { title: "Fourth Post", content: "This is the fourth sample post content.", user: users[3] },
  { title: "Fifth Post", content: "This is the fifth sample post content.", user: users[4] },
  { title: "Rails is Awesome", content: "Ruby on Rails is a great framework for building web applications.", user: users[0] },
  { title: "API Development", content: "Building APIs with Rails is straightforward and powerful.", user: users[1] },
  { title: "Database Design", content: "Good database design is crucial for application performance.", user: users[2] },
  { title: "Authentication", content: "Implementing authentication securely is very important.", user: users[3] },
  { title: "Testing", content: "Writing tests should be part of every development workflow.", user: users[4] }
])

puts "Created #{posts.size} posts"

# Create sample comments
comments = Comment.create!([
  { content: "Great post!", user: users[1], post: posts[0] },
  { content: "Thanks for sharing", user: users[2], post: posts[0] },
  { content: "Very informative", user: users[3], post: posts[1] },
  { content: "I learned a lot", user: users[4], post: posts[1] },
  { content: "Looking forward to more", user: users[0], post: posts[2] },
  { content: "Well written", user: users[1], post: posts[3] },
  { content: "Interesting perspective", user: users[2], post: posts[4] },
  { content: "Agree completely", user: users[3], post: posts[2] },
  { content: "Thanks for this", user: users[4], post: posts[3] },
  { content: "Helpful content", user: users[0], post: posts[4] }
])

puts "Created #{comments.size} comments"

puts "Database seeded successfully!"
