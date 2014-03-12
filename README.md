# CIDER Spy

Spy on CIDER nREPL sessions

## Purpose

To help developers and teams share information about development flows.

### Initial Ideas

#### Namespace intelligence

* I spent x amount of time in this namespace today

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
