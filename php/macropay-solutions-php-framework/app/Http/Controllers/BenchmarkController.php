<?php

namespace App\Http\Controllers;

use MacropaySolutions\Kernel\Http\JsonResponse;
use MacropaySolutions\Kernel\Http\Request;
use MacropaySolutions\Kernel\Http\Response;
use Symfony\Component\HttpFoundation\File\UploadedFile;

/**
 * the-benchmarker/web-frameworks v2 scenario
 */
class BenchmarkController extends Controller
{
    public function index(): Response
    {
        return new Response();
    }

    public function createUser(): Response
    {
        return new Response();
    }

    public function user(string $id): Response|JsonResponse
    {
        // the router decodes the path parameter before it reaches the controller
        if (1 !== \preg_match('/\A[0-9]{8}\z/', $id)) {
            return new Response('', 400);
        }

        return new JsonResponse(['id' => $id]);
    }

    public function serialization(Request $request): Response|JsonResponse
    {
        $queryString = (string)$request->server->get('QUERY_STRING', '');
        $pair = \explode('=', $queryString, 2);

        // the query bag collapses repeated names, so the raw query must hold exactly one seed=value pair
        if (2 !== \count($pair) || \str_contains($queryString, '&') || 'seed' !== \urldecode($pair[0])) {
            return new Response('', 400);
        }

        $seed = $request->query->all()['seed'] ?? null;

        if (!\is_string($seed) || 1 !== \preg_match('/\A[a-z]{8}\z/', $seed)) {
            return new Response('', 400);
        }

        return new JsonResponse(\array_fill(0, 100, ['seed' => $seed]));
    }

    public function deserialization(Request $request): Response|JsonResponse
    {
        if (!$this->isJsonMediaType((string)$request->headers->get('Content-Type'))) {
            return new Response('', 415);
        }

        if ('' !== (string)$request->server->get('QUERY_STRING', '')) {
            return new Response('', 400);
        }

        try {
            // objects stay objects, so {} and [] can be told apart
            $data = \json_decode($request->getContent(), false, 512, \JSON_THROW_ON_ERROR);
        } catch (\JsonException) {
            return new Response('', 400);
        }

        $items = $data instanceof \stdClass ? ($data->items ?? null) : null;

        if (!\is_array($items) || 100 !== \count($items)) {
            return new Response('', 400);
        }

        $checksum = 0;

        foreach ($items as $item) {
            $id = $item instanceof \stdClass ? ($item->id ?? null) : null;

            if (!$this->isValidId($id)) {
                return new Response('', 400);
            }

            $checksum += (int)$id;
        }

        return new JsonResponse(['count' => 100, 'checksum' => $checksum]);
    }

    public function upload(Request $request): Response|JsonResponse
    {
        $boundary = $this->multipartBoundary((string)$request->headers->get('Content-Type'));

        if (null === $boundary) {
            return new Response('', 415);
        }

        if ('' === $boundary) {
            return new Response('', 400);
        }

        // PHP's own multipart parser keeps only the last part of a repeated name and accepts truncated bodies,
        // so enable_post_data_reading is off and the body is checked here
        $part = $this->singleFilePart($request->getContent(), $boundary);

        if (null === $part) {
            return new Response('', 400);
        }

        $path = \tempnam(\sys_get_temp_dir(), 'upload');

        try {
            \file_put_contents($path, $part['contents']);
            $request->files->set(
                'file',
                new UploadedFile($path, $part['filename'], $part['type'], \UPLOAD_ERR_OK, true)
            );
            $file = $request->file('file');
            $contents = $file->getContent();
            $size = \strlen($contents);

            return new JsonResponse([
                'first_byte' => 0 === $size ? null : \ord($contents[0]),
                'last_byte' => 0 === $size ? null : \ord($contents[$size - 1]),
                'checksum' => \hash('sha256', $contents),
            ]);
        } finally {
            \unlink($path);
        }
    }

    protected function isJsonMediaType(string $contentType): bool
    {
        $mediaType = \strtolower(\trim(\explode(';', $contentType, 2)[0]));

        return 'application/json' === $mediaType
            || 'text/x-json' === $mediaType
            || 'application/jsonrequest' === $mediaType
            || 1 === \preg_match('~\Aapplication/[a-z0-9!#$&^_.+-]+\+json\z~', $mediaType);
    }

    protected function isValidId(mixed $id): bool
    {
        if (\is_int($id)) {
            return 0 <= $id && $id <= 1000000;
        }

        // 2.0 and 2e0 are integral numbers too
        return \is_float($id) && 0.0 <= $id && $id <= 1000000.0 && \floor($id) === $id;
    }

    /**
     * Null for another media type, '' for a missing or invalid boundary.
     */
    protected function multipartBoundary(string $contentType): ?string
    {
        $parameters = \explode(';', $contentType);

        if ('multipart/form-data' !== \strtolower(\trim($parameters[0]))) {
            return null;
        }

        foreach (\array_slice($parameters, 1) as $parameter) {
            $pair = \explode('=', $parameter, 2);

            if (2 === \count($pair) && 'boundary' === \strtolower(\trim($pair[0]))) {
                $boundary = \trim($pair[1]);
                $boundary = \str_starts_with($boundary, '"') ? \trim($boundary, '"') : $boundary;

                // RFC 2046: 1 to 70 characters
                return \strlen($boundary) <= 70 ? $boundary : '';
            }
        }

        return '';
    }

    /**
     * The only selected file of the "file" field, or null for a malformed body, a text "file" field
     * or anything but exactly one selected file.
     */
    protected function singleFilePart(string $body, string $boundary): ?array
    {
        $delimiter = "\r\n--" . $boundary;
        // the first delimiter may start the body without the leading CRLF
        $position = \str_starts_with($body, '--' . $boundary) ? -2 : \strpos($body, $delimiter);
        $found = null;

        while (false !== $position) {
            $position += \strlen($delimiter);

            if ('--' === \substr($body, $position, 2)) {
                return $found;
            }

            if ("\r\n" !== \substr($body, $position, 2)) {
                return null;
            }

            $headersEnd = \strpos($body, "\r\n\r\n", $position);
            $next = \strpos($body, $delimiter, $position);

            if (false === $headersEnd || false === $next || $next < $headersEnd) {
                return null;
            }

            $disposition = $this->partHeaders(\substr($body, $position + 2, $headersEnd - $position - 2));

            if (null === $disposition) {
                return null;
            }

            if ('file' === ($disposition['name'] ?? null)) {
                // a text field cannot supply the file
                if (!isset($disposition['filename'])) {
                    return null;
                }

                // filename="" is an unselected file input
                if ('' !== $disposition['filename']) {
                    if (null !== $found) {
                        return null;
                    }

                    $found = [
                        'filename' => $disposition['filename'],
                        'type' => $disposition['type'] ?? null,
                        'contents' => \substr($body, $headersEnd + 4, $next - $headersEnd - 4),
                    ];
                }
            }

            $position = $next;
        }

        // no closing delimiter
        return null;
    }

    /**
     * The form-data name, filename and Content-Type of a part, or null when it is not a form-data part.
     */
    protected function partHeaders(string $headers): ?array
    {
        $result = null;
        $type = null;

        foreach ('' === $headers ? [] : \explode("\r\n", $headers) as $header) {
            $pair = \explode(':', $header, 2);

            if (2 !== \count($pair)) {
                return null;
            }

            $name = \strtolower(\trim($pair[0]));

            if ('content-type' === $name) {
                $type = \trim($pair[1]);

                continue;
            }

            if ('content-disposition' !== $name) {
                continue;
            }

            if ('form-data' !== \strtolower(\trim(\explode(';', $pair[1], 2)[0]))) {
                return null;
            }

            // parameter = token or quoted string
            \preg_match_all(
                '/;\s*([a-zA-Z*]+)\s*=\s*(?:"((?:[^"\\\\]|\\\\.)*)"|([^;\s]*))/',
                $pair[1],
                $matches,
                \PREG_SET_ORDER
            );
            $result = [];

            foreach ($matches as $match) {
                $result[\strtolower($match[1])] = $match[3] ?? \preg_replace('/\\\\(.)/s', '$1', $match[2]);
            }
        }

        if (null === $result) {
            return null;
        }

        $result['type'] = $type;

        return $result;
    }
}
