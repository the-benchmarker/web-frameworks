@v1
Feature: Legacy routing
  The v1 contract preserves the original three routing workloads.

  Scenario: Static route
    When I send GET "/"
    Then the response is successful
    And the response body is empty

  Scenario: Method dispatch
    Given the request Content-Type is "text/plain"
    And the request body is ""
    When I send POST "/user"
    Then the response is successful
    And the response body is empty

  Scenario Outline: Read the ID from the path
    When I send GET "/user/<id>"
    Then the response is successful
    And the response body is "<id>"

    Examples:
      | id |
      | 0 |
      | 42 |
      | 12345678 |
