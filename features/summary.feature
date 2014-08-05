Feature: Summary Display
  Background:

  Scenario:
    When I refresh with {"devs":{"some-id" : {"alias" : "Awesomedude", "nses" : ["foo.bar"]}}}
    Then I should see "Devs Hacking:  Awesomedude: [foo.bar]" in the "devs" section

  Scenario:
    When I refresh with {"ns-trail":[{"ns" : "proja.core"}, {"ns" : "proja.core2", "seconds" : 100}]}
    Then I should see "Your Namespace Trail:  proja.core (Am here)  proja.core2 (100 seconds)" in the "ns-trail" section

  Scenario:
    When I refresh with {"nses-loaded" : {"proja.corea" : 1, "proja.coreb" : 2}}
    Then I should see "Your Namespaces Loaded:  proja.coreb (2 times)  proja.corea (1 times)" in the "nses-loaded" section

  Scenario:
    When I refresh with {"fns" : {"clojure.core/println" : 2, "clojure.core/str" : 1}}
    Then I should see "Your Function Calls:  clojure.core/println (2 times)  clojure.core/str (1 times)" in the "fns" section

  Scenario:
    When I refresh with {"session" : {"started" : "08:59:34", "seconds" : 21}}
    Then I should see "Your Session:  Started 08:59:34, uptime: 21 seconds." in the "session" section

  Scenario:
    When I refresh with {"devs":{"some-id" : {"alias" : "Awesomedude", "nses" : ["foo.bar"]}}, "ns-trail":[{"ns" : "proja.core"}], "nses-loaded" : {"proja.corea" : 1}, "fns" : {"clojure.core/println" : 2}, "session" : {"started" : "08:59:34", "seconds" : 21}}
    And I go to beginning of summary buffer
    And I go to the next section
    Then I should be on a "dev" section
