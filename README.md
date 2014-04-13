THIS PROJECT IS UNSTABLE / IN DEVELOPMENT
RIGHT NOW IT'S GROSSLY UNSTABLE - COME BACK SOON!

# CIDER Spy

Spy on CIDER nREPL sessions and help developers and teams share information, send code snippets to each other etc.

# Installation

## Prerequisites

You need the [`CIDER-SPY-NREPL`](https://github.com/jonpither/cider-spy-nrepl) middleware. See installation instructions there. `CIDER-SPY` also depends on [`CIDER`](https://github.com/clojure-emacs/cider).

If you want the developer interactivity behavours, then you need a run a `CIDER-SPY-HUB`. See the middleware documentation for how to set one up. It's trivial enough.

## Configuration

You can certainly use `CIDER-SPY` without configuring it any further
to get summary information about your own REPL session. But configuration
is needed if you want to connect to a `CIDER-SPY-HUB` to interact with your
colleagues.

### Basic configuration

* Set `cider-spy-hub-endpoint` to where the `CIDER-SPY-HUB` is running.

```el
(setq cider-spy-hub-endpoint '("localhost" 7771)
```

* Give yourself an alias on the hub.

```el
(setq cider-spy-hub-alias "my-alias")
```



# Keyboard Shortcuts

### cider-spy-mode

Keyboard shortcut                    | Description
-------------------------------------|-------------------------------
<kbd>g</kbd>| Refresh the `*cider-spy*` buffer.
<kbd>r</kbd>| Reset the tracking data underpinning the `*cider-spy*` buffer.
<kbd>n</kbd>| Goto to next section.
<kbd>p</kbd>| Goto to previous section.
<kbd>TAB</kbd>| Toggle section visibility.

# Background

This originally started life as a tool to help users learn about their own REPL behavour, and to function as an uber smart project navigation tool.

Since then it's morphed into being a tool to help developers share information. I.e. to ping code to each other and see where colleagues are.

## TODOS

* Integrate with travis.. follow CIDER
* Right now summary is refreshed all the time - I should make a better process of firing off deltas (i.e. registrations). Perhaps I should start by having a dedicated 'flash message' section of the spy buffer.
* Connected repl sessions - i.e. send someone some code
* Dev x has been on this file.. etc etc.
* Popup some code in a buffer on another devs emacs
* Where is everyone right now? (project specific)
* Refactor: Split out an interaction file.
* Test: Do a ping
* Namespaces/hunks should be links
* Functions run most often outside of immediate namespace
* Functions run that fail most often
* Time Fns: https://github.com/clojure-emacs/cider/issues/477.
* Keep ref to last REPL eval (saves deffing)
* REPL Summary: I spent x amount of time in this namespace today

# License

Copyright © 2014 Jon Pither

Distributed under the GNU General Public License, version 3
