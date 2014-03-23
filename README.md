THIS PROJECT IS UNSTABLE / IN DEVELOPMENT

# CIDER Spy

Spy on CIDER nREPL sessions. Depends on [cider-spy-nrepl](https://github.com/jonpither/cider-spy-nrepl).

## Purpose

To help developers and teams share information about development flows.

## TODOS

* Remove junk from function calls if possible
* Ability to clear/reset the spy buffer
* Namespaces/hunks should be links
* Functions run most often outside of immediate namespace
* Functions run that fail most often
* Time Fns: https://github.com/clojure-emacs/cider/issues/477.
* Keep ref to last REPL eval (saves deffing)

## Abstract Directions

* Could actually be useful as a "learnt" project navigation tool
* I spent x amount of time in this namespace today
* track a users journey

## Ver 2

### Connected REPL sessions

* Connected repl sessions - i.e. send someone some code
* Dev x has been on this file.. etc etc.
* Popup some code in a buffer on another devs emacs
* Where is everyone right now? (project specific)

# License

Copyright © 2014 Jon Pither

Distributed under the GNU General Public License, version 3
