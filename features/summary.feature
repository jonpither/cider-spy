Feature: Summary Display
  Background:

  Scenario:
    When I refresh with "{\"devs\":{\"Awesomedude\" : \"foo.bar\"}}"
    Then I should see "Devs Hacking:\n  Awesomedude: foo.bar" in the "devs" section
