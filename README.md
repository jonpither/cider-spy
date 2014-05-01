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
<kbd>RETURN</kbd>| Visit section.
<kbd>TAB</kbd>| Toggle section visibility.

# Background

This originally started life as a tool to help users learn about their own REPL behavour, and to function as an uber smart project navigation tool.

Since then it's morphed into being a tool to help developers share information. I.e. to ping code to each other and see where colleagues are.

## Feature TODOS

* Then cider-spy-hub should work if people do not specify an alias - it should show them as an `anon-user23323` or something.
* We should then have a command so that people can change their alias.
* Popup some code in a buffer on another devs emacs
* Dev x has been on this file.. etc etc.
* Functions run most often outside of immediate namespace
* Functions run that fail most often
* Time Fns: https://github.com/clojure-emacs/cider/issues/477.
* REPL Summary: I spent x amount of time in this namespace today

## Refactor TODOS

* Integrate with travis.. follow CIDER
* Right now summary is refreshed all the time - I should make a better process of firing off deltas (i.e. registrations). Perhaps I should start by having a dedicated 'flash message' section of the spy buffer.
* Refactor: Split out an interaction file.
* Test: Do a ping
* Document credits in official way. Magnars because I used a lot of `magit` patterns and `clj-refactor` build setup. bbatsov because he basically taught me Elisp via hacking on CIDER.

## Config Rumination

Can we move hub endpoint config into project.clj? It's a shared project thing. We're not subscribing people automatically as it's already optional in part; the hub functionality only works if people install `CIDER-SPY`. We could go further and completely disable the entire middleware if `CIDER-SPY` is not present.

If we can't move it into project.clj, then perhaps just stick a `.cider-spy-root` file in the project root.

Another config option is to use `ENVIRON` to allow users another chance at overriding the central project settings in `.cider-spy-root`. A further option is to use `ENVIRON` full stop and cut out the middle file.

Once config storage has been sorted, we could give the option to `cider-spy-hub-connect-on-demand`. If true the nrepl-server instance only connects to hub when user initiates it through `CIDER-SPY`, otherwise it auto connects and people get the benefit straight away of seeing where ppl are (this is best option to show what `CIDER-SPY-HUB` can do). The rationale is that I like the idea it's a bit magical, that everything just works, even if other devs haven't install the `CIDER-SPY` Emacs package; other devs can still see where people are.

BTW this rumination is the start of a section in the docs that explains the thought process. People will be rightfully worried about auto connecting and being spied on.

# License

Copyright © 2014 Jon Pither

Distributed under the GNU General Public License, version 3
