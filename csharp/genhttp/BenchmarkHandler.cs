using GenHTTP.Api.Content;
using GenHTTP.Api.Protocol;

using Strings = GenHTTP.Modules.IO.Strings;

namespace web;

internal class BenchmarkHandler : IHandler
{
    private static readonly FlexibleContentType _TextType = FlexibleContentType.Get(ContentType.TextPlain);

    private static readonly Strings.StringContent _EmptyContent = new("");

    public ValueTask PrepareAsync() => new();

    public ValueTask<IResponse> HandleAsync(IRequest request)
    {
        IResponse response = null;

        var target = request.Target;

        if (target.Ended)
        {
            response = GetEmptyResponse(request);
        }
        else if (target.Current.Original == "user")
        {
            target.Advance();

            if (target.Ended)
            {
                response = GetEmptyResponse(request);
            }
            else
            {
                response = GetEmptyResponse(request, new Strings.StringContent(target.Current.Original));
            }
        }

        return new(response);
    }

    private static IResponse GetEmptyResponse(IRequest request, Strings.StringContent content = null)
    {
        return request.Respond()
                      .Type(_TextType)
                      .Content(content ?? _EmptyContent)
                      .Build();
    } 

}