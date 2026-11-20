@v2
Feature: Multipart upload
  Hash every byte of exactly one uploaded file through the uploaded-file API.

  Scenario Outline: Describe the uploaded contents
    Given request bodies use <transport>
    Given the upload payload is "<payload>"
    When I send POST "/upload"
    Then the JSON response describes every uploaded byte

    Examples:
      | transport | payload |
      | content-length | fixture |
      | content-length | empty file |
      | content-length | one byte |
      | content-length | unsigned edge bytes |
      | content-length | every byte value |
      | content-length | embedded newlines |
      | content-length | boundary-like file data |
      | content-length | filename sample.txt |
      | content-length | filename another-file.dat |
      | content-length | no file media type |
      | content-length | boundary another-boundary |
      | content-length | boundary quoted:boundary |
      | content-length | boundary x |
      | content-length | load token 123456 |
      | content-length | load token 987654 |
      | content-length | middle byte changed |
      | content-length | text fields |
      | content-length | unselected first |
      | content-length | unselected last |
      | chunked | fixture |
      | chunked | empty file |
      | chunked | one byte |
      | chunked | unsigned edge bytes |
      | chunked | every byte value |
      | chunked | embedded newlines |
      | chunked | boundary-like file data |
      | chunked | filename sample.txt |
      | chunked | filename another-file.dat |
      | chunked | no file media type |
      | chunked | boundary another-boundary |
      | chunked | boundary quoted:boundary |
      | chunked | boundary x |
      | chunked | load token 123456 |
      | chunked | load token 987654 |
      | chunked | middle byte changed |
      | chunked | text fields |
      | chunked | unselected first |
      | chunked | unselected last |

  Scenario Outline: Return the documented fixture response
    Given request bodies use <transport>
    Given the upload payload is "fixture"
    When I send POST "/upload"
    Then the JSON response is:
      """
      {"first_byte":97,"last_byte":100,"checksum":"1f91053dcf43206eb082c0962785d35d86d4f629345f8bff25be7394416db908"}
      """

    Examples:
      | transport |
      | content-length |
      | chunked |

  Scenario Outline: Reject missing, repeated, or malformed uploads
    Given request bodies use <transport>
    Given the upload payload is "<payload>"
    When I send POST "/upload"
    Then the response status is 400

    Examples:
      | transport | payload |
      | content-length | no file |
      | content-length | wrong field |
      | content-length | text file field |
      | content-length | repeated empty uploads |
      | content-length | repeated uploads |
      | content-length | file then text |
      | content-length | text then file |
      | content-length | one unselected |
      | content-length | two unselected |
      | content-length | repeated with unselected |
      | content-length | empty body |
      | content-length | incomplete delimiter |
      | content-length | mismatched boundary |
      | content-length | truncated headers |
      | content-length | incomplete trailing part |
      | content-length | missing boundary |
      | content-length | empty boundary |
      | content-length | quoted empty boundary |
      | chunked | no file |
      | chunked | wrong field |
      | chunked | text file field |
      | chunked | repeated empty uploads |
      | chunked | repeated uploads |
      | chunked | file then text |
      | chunked | text then file |
      | chunked | one unselected |
      | chunked | two unselected |
      | chunked | repeated with unselected |
      | chunked | empty body |
      | chunked | incomplete delimiter |
      | chunked | mismatched boundary |
      | chunked | truncated headers |
      | chunked | incomplete trailing part |
      | chunked | missing boundary |
      | chunked | empty boundary |
      | chunked | quoted empty boundary |

  Scenario Outline: Query parameters cannot supply a file
    Given request bodies use <transport>
    Given the upload payload is "no file"
    When I send POST "/upload?file=payload.bin"
    Then the response status is 400

    Examples:
      | transport |
      | content-length |
      | chunked |

  Scenario Outline: Reject unsupported media types before parsing
    Given request bodies use <transport>
    Given the upload payload is "<payload>"
    And the request Content-Type is "<type>"
    And the request header "Accept" is "application/json"
    When I send POST "/upload"
    Then the response status is 415

    Examples:
      | transport | payload | type |
      | content-length | fixture |  |
      | content-length | fixture | application/json |
      | content-length | fixture | text/plain |
      | content-length | fixture | application/x-www-form-urlencoded |
      | content-length | fixture | multipart/mixed |
      | content-length | malformed |  |
      | content-length | malformed | application/json |
      | content-length | malformed | text/plain |
      | content-length | malformed | application/x-www-form-urlencoded |
      | content-length | malformed | multipart/mixed |
      | content-length | empty body |  |
      | content-length | empty body | application/json |
      | content-length | empty body | text/plain |
      | content-length | empty body | application/x-www-form-urlencoded |
      | content-length | empty body | multipart/mixed |
      | chunked | fixture |  |
      | chunked | fixture | application/json |
      | chunked | fixture | text/plain |
      | chunked | fixture | application/x-www-form-urlencoded |
      | chunked | fixture | multipart/mixed |
      | chunked | malformed |  |
      | chunked | malformed | application/json |
      | chunked | malformed | text/plain |
      | chunked | malformed | application/x-www-form-urlencoded |
      | chunked | malformed | multipart/mixed |
      | chunked | empty body |  |
      | chunked | empty body | application/json |
      | chunked | empty body | text/plain |
      | chunked | empty body | application/x-www-form-urlencoded |
      | chunked | empty body | multipart/mixed |

  Scenario Outline: Recover after errors and recompute changing contents
    Given request bodies use <transport>
    Given the upload payload is "malformed"
    When I send POST "/upload"
    Then the response status is 400
    Given the upload payload is "repeated uploads"
    When I send POST "/upload"
    Then the response status is 400
    Given the upload payload is "fixture"
    When I send POST "/upload"
    Then the JSON response describes every uploaded byte
    Given the upload payload is "middle byte changed"
    When I send POST "/upload"
    Then the JSON response describes every uploaded byte
    Given the upload payload is "fixture"
    When I send POST "/upload"
    Then the JSON response describes every uploaded byte

    Examples:
      | transport |
      | content-length |
      | chunked |
