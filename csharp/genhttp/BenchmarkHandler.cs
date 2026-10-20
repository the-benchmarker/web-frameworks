using GenHTTP.Api.Content;
using GenHTTP.Api.Infrastructure;
using GenHTTP.Api.Protocol;

using GenHTTP.Modules.IO;

namespace web;

internal sealed class BenchmarkHandler : IHandler
{
    private static readonly ReadOnlyMemory<byte> EmptyContent = new();

    private static readonly PathSegment UserSegment = new("user");

    public ValueTask PrepareAsync(IServer server) => new();

    public ValueTask<IResponse> HandleAsync(IRequest request)
    {
        IResponse response = null;

        var target = request.Header.Target;

        if (target.Current == null)
        {
            response = GetResponse(request);
        }
        else if (target.Current.Value == UserSegment)
        {
            target.Advance();

            if (target.Current == null)
            {
                response = GetResponse(request);
            }
            else
            {
                response = GetResponse(request, target.Current.Value.Bytes);
            }
        }

        return new(response);
    }

    private static IResponse GetResponse(IRequest request, ReadOnlyMemory<byte>? content = null)
    {
        return request.Respond()
                      .Content(content ?? EmptyContent, ContentType.TextPlain)
                      .Build();
    }

}
