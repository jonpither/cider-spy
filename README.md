_THIS PROJECT IS IN ACTIVE DEVELOPMENT. It's not ready for general use but will be soon._

# CIDER Spy

[![Build Status](https://travis-ci.org/jonpither/cider-spy.svg?branch=master)](https://travis-ci.org/jonpither/cider-spy)

Get visibility on CIDER nREPL sessions and help developers in teams to share information, send code snippets to each other etc.

# Installation

## Prerequisites

You need the [`CIDER-SPY-NREPL`](https://github.com/jonpither/cider-spy-nrepl) middleware. See installation instructions there. `CIDER-SPY` also depends on [`CIDER`](https://github.com/clojure-emacs/cider).

If you want the developer interactivity behavours, then you need a run a `CIDER-SPY-HUB`. See the middleware documentation for how to set one up. It's trivial enough.

## Configuration

You can use `CIDER-SPY` without configuring it any further
to get summary information about your own REPL session.

Configuration is needed if you want to connect to a `CIDER-SPY-HUB` to interact with your
colleagues, to view where they are, and to to exchange messaging etc.

### Basic configuration

* Add `CIDER-SPY` middleware to your project:

    :profiles {:dev {:dependencies [[cider-spy/cider-spy-nrepl "0.1.0-SNAPSHOT"]]
                       :repl-options {:nrepl-middleware [cider-spy-nrepl.middleware.cider-spy/wrap-cider-spy]}}}`


### Basic configuration for HUB

* Ensure the HUB and HUB middleware is [configured correctly](https://github.com/jonpither/cider-spy-nrepl).

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
<kbd>a</kbd>| Set `CIDER-SPY-HUB` alias.
<kbd>s</kbd>| Send message to another dev (when cursor is on a dev).
<kbd>d</kbd>| Disconnect from the `CIDER-SPY-HUB`.
<kbd>RETURN</kbd>| Visit section.
<kbd>TAB</kbd>| Toggle section visibility.

# Background

This originally started life as a tool to help users learn about their own REPL behavour, and to function as an uber smart project navigation tool.

Since then it's morphed into being a tool to help developers share information. I.e. to ping code to each other and see where colleagues are.

![Cider Spy Menu](/images/menu.png?raw=true)

## Credits

Thanks to Magnars because unknown to him I used a lot of `magit` patterns and copied the `clj-refactor` build setup. The `CIDER` team also as it's a great tool and the support is terrific.

# License

Copyright © 2014 Jon Pither

Distributed under the GNU General Public License, version 3
