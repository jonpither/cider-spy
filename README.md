THIS IS POTENTIALLY USELESS - ITS A PLAYGROUND FOR NOW

# CIDER Spy

Spy on CIDER nREPL sessions

## Purpose

To help developers and teams share information about development flows.

### Make namespaces links

### Initial Ideas

#### Namespace intelligence

* Could actually be useful as a "learnt" project navigation tool
i.e. menu of where you've been (with some stats)
* I spent x amount of time in this namespace today
* track a users journey
* Can navigate from hunks to get to nses

#### Function intelligence

* Functions run most often outside of immediate namespace
* Functions run that fail most often

#### Popular namespaces

#### Activity Summary (a buffer)

Where I've been
Most popular functions

## Concerns

### Why is this libary anything to do with CIDER?

Good question. It could be exclusively nrepl-middleware. I guess there's ideas like cider-spy might be able to provide new CIDER functionality based of spied on data.. like most popular commands.

A follow on is that I should revise whether I need a dependency on cider-nrepl

## Future ideas

Connected repl sessions - i.e. send someone some code
Dev x has been on this file.. etc etc.
Where is everyone right now? (project specific)

## Immediate TODOs

* Ability to clear the spy buffer
* Implement https://github.com/clojure-emacs/cider/issues/477. Could have a toggle to print the time duration after each request.
