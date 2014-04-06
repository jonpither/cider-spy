THIS PROJECT IS UNSTABLE / IN DEVELOPMENT
RIGHT NOW IT'S GROSSLY UNSTABLE - COME BACK SOON!

# CIDER Spy

Spy on CIDER nREPL sessions. Depends on [cider-spy-nrepl](https://github.com/jonpither/cider-spy-nrepl) where most of the development is happening.

## Purpose

To help developers and teams share information about development flows.

## Changing direction

This originally started life as a tool to help users learn about their own REPL behavour, and to function as an uber smart project navigation tool.

Since then it's morphed into being a tool to help developers share information. I.e. to ping code to each other and see where colleagues are.

## TODOS

* There does need to be a way for turning it off for a project.. I.e. I don't want my private proj stuff showing up on CIDER SPY
* Right now summary is refreshed all the time - I should make a better process of firing off deltas (i.e. registrations). Perhaps I should start by having a dedicated 'flash message' section of the spy buffer.
* Connected repl sessions - i.e. send someone some code
* Dev x has been on this file.. etc etc.
* Popup some code in a buffer on another devs emacs
* Where is everyone right now? (project specific)
* Refactor: Split out an interaction file.
* Test: Do a ping
* Ability to clear/reset the spy buffer
* Namespaces/hunks should be links
* Functions run most often outside of immediate namespace
* Functions run that fail most often
* Time Fns: https://github.com/clojure-emacs/cider/issues/477.
* Keep ref to last REPL eval (saves deffing)
* REPL Summary: I spent x amount of time in this namespace today

# License

Copyright © 2014 Jon Pither

Distributed under the GNU General Public License, version 3
