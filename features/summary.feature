Feature: Summary Display
  Background:

  Scenario:
    When I refresh with {"devs":{"some-id" : {"alias" : "Awesomedude", "nses" : ["foo.bar"]}}}
    Then I should see "Devs Hacking:  Awesomedude: [foo.bar]" in the "devs" section
