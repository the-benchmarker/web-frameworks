# Static Files

## Dynamic Public Files

All file paths in the static directory are cached whent the server is initialized for quicker lookup. Most other comparable web frameworks do an IO operation to check if the request path is a reference to an existing file in the public directory (for each and every request). The static directory indexing increased requests per second performance for Raze by 20%.

If you will be adding/deleting files in the static directory during runtime, Raze by default won't recognize the changes. So, to track changes there are 2 main options:

  1) Add and Remove file indexes from the StaticFileIndexer (Best Performance)
  2) Specify a DynamicStatic  directory

### Adding/Removing file references in the StaticFileIndexer

Paths start from the `static/` directory, so you should not include it.

**Adding a file index**

```ruby
Raze::StaticFileIndexer.static_files["images/my-cool-img.png"] = "file"
```

**Removing a file index**

```ruby
Raze::StaticFileIndexer.static_files.delete("images/my-cool-img.png")
```

Raze only cares if the file exists or not. You don't need to update anything if file contents are changed.

### Specifying a DynamicStatic Directory

Dynamic (*noun*) - characterized by constant change, activity, or progress
Static (*noun*) - lacking in movement, action, or change
Oxymoron (*noun*) - a figure of speech in which contradictory terms appear in conjunction

