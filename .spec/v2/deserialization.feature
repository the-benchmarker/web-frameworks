@v2
Feature: JSON deserialization
  Parse exactly 100 items and sum their integral numeric IDs from the supplied body.

  Scenario Outline: Compute count and checksum
    Given request bodies use <transport>
    Given the JSON payload is "<payload>"
    When I send POST "/deserialization"
    Then the JSON response has count 100 and checksum <checksum>

    Examples:
      | transport | payload | checksum |
      | content-length | fixture | 4950 |
      | content-length | minimal | 4950 |
      | content-length | all IDs 0 | 0 |
      | content-length | all IDs 1000000 | 100000000 |
      | content-length | all IDs 2.0 | 200 |
      | content-length | all IDs 2e0 | 200 |
      | content-length | extra fields | 4950 |
      | content-length | load token 123456 | 10122220 |
      | content-length | load token 987654 | 14443210 |
      | chunked | fixture | 4950 |
      | chunked | minimal | 4950 |
      | chunked | all IDs 0 | 0 |
      | chunked | all IDs 1000000 | 100000000 |
      | chunked | all IDs 2.0 | 200 |
      | chunked | all IDs 2e0 | 200 |
      | chunked | extra fields | 4950 |
      | chunked | load token 123456 | 10122220 |
      | chunked | load token 987654 | 14443210 |

  Scenario Outline: Accept JSON media types
    Given request bodies use <transport>
    Given the JSON payload is "minimal"
    And the request Content-Type is "<type>"
    When I send POST "/deserialization"
    Then the JSON response has count 100 and checksum 4950

    Examples:
      | transport | type |
      | content-length | application/json |
      | content-length | application/json; charset=utf-8 |
      | content-length | application/problem+json |
      | content-length | application/vnd.api+json |
      | content-length | application/vnd.benchmark+json; charset=utf-8 |
      | content-length | text/x-json |
      | content-length | application/jsonrequest |
      | chunked | application/json |
      | chunked | application/json; charset=utf-8 |
      | chunked | application/problem+json |
      | chunked | application/vnd.api+json |
      | chunked | application/vnd.benchmark+json; charset=utf-8 |
      | chunked | text/x-json |
      | chunked | application/jsonrequest |

  Scenario Outline: Reject malformed JSON for supported media types
    Given request bodies use <transport>
    Given the request body is "{"
    And the request Content-Type is "<type>"
    When I send POST "/deserialization"
    Then the response status is 400

    Examples:
      | transport | type |
      | content-length | application/json |
      | content-length | application/json; charset=utf-8 |
      | content-length | application/problem+json |
      | content-length | application/vnd.api+json |
      | content-length | application/vnd.benchmark+json; charset=utf-8 |
      | content-length | text/x-json |
      | content-length | application/jsonrequest |
      | chunked | application/json |
      | chunked | application/json; charset=utf-8 |
      | chunked | application/problem+json |
      | chunked | application/vnd.api+json |
      | chunked | application/vnd.benchmark+json; charset=utf-8 |
      | chunked | text/x-json |
      | chunked | application/jsonrequest |

  Scenario Outline: Reject invalid JSON bodies and schemas
    Given request bodies use <transport>
    Given the JSON payload is "<payload>"
    When I send POST "/deserialization"
    Then the response status is 400

    Examples:
      | transport | payload |
      | content-length | empty |
      | content-length | whitespace |
      | content-length | truncated object |
      | content-length | truncated array |
      | content-length | missing items |
      | content-length | null |
      | content-length | array |
      | content-length | boolean |
      | content-length | number |
      | content-length | string |
      | content-length | trailing garbage |
      | content-length | two objects |
      | content-length | non-JSON keys |
      | content-length | trailing comma |
      | content-length | top-level items |
      | content-length | item count 0 |
      | content-length | item count 1 |
      | content-length | item count 99 |
      | content-length | item count 101 |
      | content-length | item count 200 |
      | content-length | items value null |
      | content-length | items value false |
      | content-length | items value true |
      | content-length | items value 100 |
      | content-length | items value {} |
      | content-length | items value \"items\" |
      | content-length | item value null |
      | content-length | item value false |
      | content-length | item value true |
      | content-length | item value [] |
      | content-length | item value \"item\" |
      | content-length | item value 7 |
      | content-length | item value {} |
      | content-length | ID null at 0 |
      | content-length | ID null at 50 |
      | content-length | ID null at 99 |
      | content-length | ID false at 0 |
      | content-length | ID false at 50 |
      | content-length | ID false at 99 |
      | content-length | ID true at 0 |
      | content-length | ID true at 50 |
      | content-length | ID true at 99 |
      | content-length | ID \"2\" at 0 |
      | content-length | ID \"2\" at 50 |
      | content-length | ID \"2\" at 99 |
      | content-length | ID 1.5 at 0 |
      | content-length | ID 1.5 at 50 |
      | content-length | ID 1.5 at 99 |
      | content-length | ID -1 at 0 |
      | content-length | ID -1 at 50 |
      | content-length | ID -1 at 99 |
      | content-length | ID 1000001 at 0 |
      | content-length | ID 1000001 at 50 |
      | content-length | ID 1000001 at 99 |
      | content-length | ID [] at 0 |
      | content-length | ID [] at 50 |
      | content-length | ID [] at 99 |
      | content-length | ID {} at 0 |
      | content-length | ID {} at 50 |
      | content-length | ID {} at 99 |
      | chunked | empty |
      | chunked | whitespace |
      | chunked | truncated object |
      | chunked | truncated array |
      | chunked | missing items |
      | chunked | null |
      | chunked | array |
      | chunked | boolean |
      | chunked | number |
      | chunked | string |
      | chunked | trailing garbage |
      | chunked | two objects |
      | chunked | non-JSON keys |
      | chunked | trailing comma |
      | chunked | top-level items |
      | chunked | item count 0 |
      | chunked | item count 1 |
      | chunked | item count 99 |
      | chunked | item count 101 |
      | chunked | item count 200 |
      | chunked | items value null |
      | chunked | items value false |
      | chunked | items value true |
      | chunked | items value 100 |
      | chunked | items value {} |
      | chunked | items value \"items\" |
      | chunked | item value null |
      | chunked | item value false |
      | chunked | item value true |
      | chunked | item value [] |
      | chunked | item value \"item\" |
      | chunked | item value 7 |
      | chunked | item value {} |
      | chunked | ID null at 0 |
      | chunked | ID null at 50 |
      | chunked | ID null at 99 |
      | chunked | ID false at 0 |
      | chunked | ID false at 50 |
      | chunked | ID false at 99 |
      | chunked | ID true at 0 |
      | chunked | ID true at 50 |
      | chunked | ID true at 99 |
      | chunked | ID \"2\" at 0 |
      | chunked | ID \"2\" at 50 |
      | chunked | ID \"2\" at 99 |
      | chunked | ID 1.5 at 0 |
      | chunked | ID 1.5 at 50 |
      | chunked | ID 1.5 at 99 |
      | chunked | ID -1 at 0 |
      | chunked | ID -1 at 50 |
      | chunked | ID -1 at 99 |
      | chunked | ID 1000001 at 0 |
      | chunked | ID 1000001 at 50 |
      | chunked | ID 1000001 at 99 |
      | chunked | ID [] at 0 |
      | chunked | ID [] at 50 |
      | chunked | ID [] at 99 |
      | chunked | ID {} at 0 |
      | chunked | ID {} at 50 |
      | chunked | ID {} at 99 |

  Scenario Outline: Reject query parameters
    Given request bodies use <transport>
    Given the JSON payload is "<payload>"
    When I send POST "/deserialization?<query>"
    Then the response status is 400

    Examples:
      | transport | payload | query |
      | content-length | minimal | seed=abcdefgh |
      | content-length | minimal | items=100 |
      | content-length | minimal | other=value |
      | content-length | minimal | format=json |
      | content-length | minimal | %73eed=abcdefgh |
      | content-length | minimal | other= |
      | content-length | minimal | other |
      | content-length | missing items | items[0][id]=1 |
      | chunked | minimal | seed=abcdefgh |
      | chunked | minimal | items=100 |
      | chunked | minimal | other=value |
      | chunked | minimal | format=json |
      | chunked | minimal | %73eed=abcdefgh |
      | chunked | minimal | other= |
      | chunked | minimal | other |
      | chunked | missing items | items[0][id]=1 |

  Scenario Outline: Check Content-Type before parsing, regardless of Accept
    Given request bodies use <transport>
    Given the JSON payload is "<payload>"
    And the request Content-Type is "<type>"
    And the request header "Accept" is "application/json"
    When I send POST "/deserialization"
    Then the response status is 415

    Examples:
      | transport | payload | type |
      | content-length | minimal |  |
      | content-length | minimal | text/plain |
      | content-length | minimal | application/xml |
      | content-length | minimal | application/x-www-form-urlencoded |
      | content-length | minimal | application/jsonp |
      | content-length | truncated object |  |
      | content-length | truncated object | text/plain |
      | content-length | truncated object | application/xml |
      | content-length | truncated object | application/x-www-form-urlencoded |
      | content-length | truncated object | application/jsonp |
      | content-length | empty |  |
      | content-length | empty | text/plain |
      | content-length | empty | application/xml |
      | content-length | empty | application/x-www-form-urlencoded |
      | content-length | empty | application/jsonp |
      | chunked | minimal |  |
      | chunked | minimal | text/plain |
      | chunked | minimal | application/xml |
      | chunked | minimal | application/x-www-form-urlencoded |
      | chunked | minimal | application/jsonp |
      | chunked | truncated object |  |
      | chunked | truncated object | text/plain |
      | chunked | truncated object | application/xml |
      | chunked | truncated object | application/x-www-form-urlencoded |
      | chunked | truncated object | application/jsonp |
      | chunked | empty |  |
      | chunked | empty | text/plain |
      | chunked | empty | application/xml |
      | chunked | empty | application/x-www-form-urlencoded |
      | chunked | empty | application/jsonp |

  Scenario Outline: Continue processing after invalid input and compute each request
    Given request bodies use <transport>
    Given the JSON payload is "missing items"
    When I send POST "/deserialization"
    Then the response status is 400
    Given the JSON payload is "last ID 999999"
    When I send POST "/deserialization"
    Then the JSON response has count 100 and checksum 1004850
    Given the JSON payload is "last ID 0"
    When I send POST "/deserialization"
    Then the JSON response has count 100 and checksum 4851
    Given the JSON payload is "last ID 42"
    When I send POST "/deserialization"
    Then the JSON response has count 100 and checksum 4893

    Examples:
      | transport |
      | content-length |
      | chunked |
