from masoniteorm.migrations import Migration


class CreatePasswordResetTable(Migration):
    def up(self):
        """Run the migrations."""
        with self.schema.create("password_resets") as table:
            table.string("email").unique()
            table.string("token")
            table.datetime("expires_at").nullable()
            table.datetime("created_at")

    def down(self):
        """Revert the migrations."""
        self.schema.drop("password_resets")
