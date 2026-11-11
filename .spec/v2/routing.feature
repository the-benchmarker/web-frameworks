@v2
Feature: V2 routing
  Successful requests return exactly 200 and IDs remain eight ASCII digits.

  Scenario: Static route
    When I send GET "/"
    Then the response status is 200
    And the response body is empty

  Scenario: Method dispatch
    Given the request body is ""
    When I send POST "/user"
    Then the response status is 200
    And the response body is empty

  Scenario Outline: Decode and preserve the path ID
    When I send GET "/user/<path>"
    Then the JSON response has only the ID "<id>"

    Examples:
      | path | id |
      | 00000000 | 00000000 |
      | 00000042 | 00000042 |
      | 12345678 | 12345678 |
      | 99999999 | 99999999 |
      | %30%30%30%30%30%30%34%32 | 00000042 |
      | 000000%34%32 | 00000042 |

  Scenario Outline: Reject invalid IDs
    When I send GET "/user/<id>"
    Then the response status is 400 or 404

    Examples:
      | id |
      | 0 |
      | 0000042 |
      | 000000042 |
      | abcdefgh |
      | 1234abcd |
      | 12345.67 |
      | -1234567 |
      | +1234567 |
      | 1234567%20 |
      | 1234567%0A |
      | 12345678%0A |
      | %0A12345678 |
      | 1234567%C3%A9 |
      | %D9%A0%D9%A1%D9%A2%D9%A3%D9%A4%D9%A5%D9%A6%D9%A7 |

  Scenario Outline: Query parameters cannot replace the path ID
    When I send GET "/user/00000042?<query>"
    Then the JSON response has only the ID "00000042"

    Examples:
      | query |
      | id=99999999 |
      | id[]=99999999 |

  Scenario: Body parameters cannot replace the path ID
    Given the request Content-Type is "application/json"
    And the request body is:
      """
      {"id":"99999999"}
      """
    When I send GET "/user/00000042"
    Then the JSON response has only the ID "00000042"

  Scenario: Valid parameters cannot rescue an invalid path ID
    Given the request Content-Type is "application/json"
    And the request body is:
      """
      {"id":"00000042"}
      """
    When I send GET "/user/invalid?id=00000042"
    Then the response status is 400 or 404

  Scenario: Compute the ID from each successive request
    When I send GET "/user/00000042"
    Then the JSON response has only the ID "00000042"
    When I send GET "/user/99999999"
    Then the JSON response has only the ID "99999999"
    When I send GET "/user/00000000"
    Then the JSON response has only the ID "00000000"
    When I send GET "/user/00000042"
    Then the JSON response has only the ID "00000042"
