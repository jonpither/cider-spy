Feature: Summary Display
  Background:

  Scenario:
    When I refresh with {"devs":{"some-id" : {"alias" : "Awesomedude", "nses" : ["foo.bar"]}}}
    Then I should see "Devs Hacking:  Awesomedude: [foo.bar]" in the "devs" section

  Scenario:
    When I refresh with {"ns-trail":[{"ns" : "proja.core"}, {"ns" : "proja.core2", "seconds" : 100}]}
    Then I should see "Your Namespace Trail:  proja.core (Am here)  proja.core2 (100 seconds)" in the "ns-trail" section
