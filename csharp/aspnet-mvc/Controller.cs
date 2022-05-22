using Microsoft.AspNetCore.Mvc;

namespace web;

[ApiController]
public class Controller : ControllerBase
{
    [HttpGet("/")]
    public IActionResult GetHome()
    {
        return Ok("");
    }

    [HttpGet("/user/{id}")]
    public IActionResult GetUserById(string id)
    {
        return Ok(id);
    }

    [HttpPost("/user")]
    public IActionResult GetUser()
    {
        return Ok("");
    }
}
