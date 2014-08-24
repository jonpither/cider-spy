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

## Git Integration Rumination

If I could integrate with GIT, then from the team status menu we should a quick summary. I.e. I hit tab on foo-developer, and I can see unpushed commit msgs and changes pending. This is kinda cool.

Technically it could work in 2 ways. Either through the Netty pipe we broadcast all git activity and the server keeps track, or we just do it request/response cycle. The former is hard as any GIT info can quickly become invalidated if people squash/rebase/stash etc. The latter is possibly simpler, just requires a bit of glue to make happen. (In future this could be expanded to do seriously funky git shit, like take a commit from someone, without them having to do anything.

So.. some funkage. Basically first story would be - fetch GIT summary (branch, commits pending, changes pending).

The potential downside is that this is proper invasive :-) I guess we can configure on/off. For this to work, unlike other functionality, it would require cider-spy to cider-spy communication.

    git-index seems to be a way. magit-insert-staged-changes is interesting.
    the wash command works on the output of git-index. Git-index is basically the way.

# License

Copyright © 2014 Jon Pither

Distributed under the GNU General Public License, version 3
