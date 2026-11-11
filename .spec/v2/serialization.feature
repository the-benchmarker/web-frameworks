@v2
Feature: JSON serialization
  Only one query parameter named seed containing eight lowercase ASCII letters is accepted.

  Scenario Outline: Serialize the supplied seed
    When I send GET "/serialization?<query>"
    Then the JSON response contains 100 objects with only seed "<seed>"

    Examples:
      | query | seed |
      | seed=abcdefgh | abcdefgh |
      | seed=aaaaaaaa | aaaaaaaa |
      | seed=zzzzzzzz | zzzzzzzz |
      | seed=azbycxdw | azbycxdw |
      | %73eed=abcdefgh | abcdefgh |
      | seed=%61%62%63%64%65%66%67%68 | abcdefgh |

  Scenario Outline: Reject invalid or additional query parameters
    When I send GET "/serialization<query>"
    Then the response status is 400

    Examples:
      | query |
      |  |
      | ? |
      | ?seed |
      | ?seed= |
      | ?seed=abcdefg |
      | ?seed=abcdefghi |
      | ?seed=12345678 |
      | ?seed=abcdefg1 |
      | ?seed=ABCDEFGH |
      | ?seed=abcd-fgh |
      | ?seed=abcd+fgh |
      | ?seed=abcd%C3%A9fgh |
      | ?seed=abcdefgh%0A |
      | ?seed=%0Aabcdefgh |
      | ?seed[]=abcdefgh |
      | ?seed[value]=abcdefgh |
      | ?seed%5B%5D=abcdefgh |
      | ?Seed=abcdefgh |
      | ?seed=abcdefgh&seed=zzzzzzzz |
      | ?seed=abcdefgh&seed=abcdefgh |
      | ?seed=abcdefgh&%73eed=zzzzzzzz |
      | ?%73eed=abcdefgh&seed=zzzzzzzz |
      | ?seed=abcdefgh&seed[]=zzzzzzzz |
      | ?seed=abcdefgh&count=100 |
      | ?seed=abcdefgh&other= |

  Scenario: The body cannot supply a missing query seed
    Given the request Content-Type is "application/json"
    And the request body is:
      """
      {"seed":"abcdefgh"}
      """
    When I send GET "/serialization"
    Then the response status is 400

  Scenario: Use the query seed even if the body contains another seed
    Given the request Content-Type is "application/json"
    And the request body is:
      """
      {"seed":"zzzzzzzz"}
      """
    When I send GET "/serialization?seed=abcdefgh"
    Then the JSON response contains 100 objects with only seed "abcdefgh"

  Scenario: Serialize the current seed on successive requests
    When I send GET "/serialization?seed=abcdefgh"
    Then the JSON response contains 100 objects with only seed "abcdefgh"
    When I send GET "/serialization?seed=zzzzzzzz"
    Then the JSON response contains 100 objects with only seed "zzzzzzzz"
    When I send GET "/serialization?seed=abcdefgh"
    Then the JSON response contains 100 objects with only seed "abcdefgh"
