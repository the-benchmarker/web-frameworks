from masoniteorm.migrations import Migration


class CreateUsersTable(Migration):
    def up(self):
        """Run the migrations."""
        with self.schema.create("users") as table:
            table.increments("id")
            table.string("name")
            table.string("email").unique()
            table.string("password")
            table.string("second_password").nullable()
            table.string("remember_token").nullable()
            table.string("phone").nullable()
            table.timestamp("verified_at").nullable()
            table.timestamps()
            table.soft_deletes()

    def down(self):
        """Revert the migrations."""
        self.schema.drop("users")
