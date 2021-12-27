namespace web
{
    using Carter;
    using System.Threading.Tasks;

    public class HomeModule : CarterModule
    {
        public HomeModule()
        {
            Get("/", (req, res) => Task.CompletedTask);
        }
    }
}
