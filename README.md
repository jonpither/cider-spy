_THIS PROJECT IS IN ACTIVE DEVELOPMENT. It's not ready for general use but will be soon._

[![Build Status](https://travis-ci.org/jonpither/cider-spy.svg?branch=master)](https://travis-ci.org/jonpither/cider-spy)

CIDER-SPY has a Multi-Person REPL! Simply install CIDER-SPY so share you REPL with other developers. With CIDER-SPY you can also get visibility on CIDER nREPL sessions and help developers in teams to share information.

Use CIDER-SPY to send code snippets and text exchanges to each other etc, as well as EMACS bookmarks.

See [a recent screencast](https://skillsmatter.com/skillscasts/5714-introducing-cider-spy).

# Installation

## Prerequisites

You need the [`CIDER-SPY-NREPL`](https://github.com/jonpither/cider-spy-nrepl) middleware. See installation instructions there.

## Basic configuration

It's available on [melpa](http://melpa.milkbox.net/):

    M-x package-install cider-spy

You can also install the dependencies on your own, and just dump
clj-refactor in your path somewhere:

 - <a href="https://github.com/magnars/dash.el">dash.el</a>
 - <a href="https://github.com/clojure-emacs/cider">cider</a>
 - <a href="https://github.com/nicferrier/emacs-noflet">noflet</a>

# Setup

    (require 'cider-spy)

All actions in `CIDER-SPY` are triggered from the `CIDER-SPY` summary page. To access the summary page:

    M-x cider-spy-summary

It can be useful to setup a global binding for the summary page for frequent access, such as <kbd>C-c C-s</kbd>.

## Configuration for the HUB

If you want the developer interactivity behavours then you need a run a `CIDER-SPY-HUB`. See the documentation for how to set one up.

# Keyboard Shortcuts

These shortcuts are available on the `CIDER-SPY` summary buffer:

Keyboard shortcut                    | Description
-------------------------------------|-------------------------------
<kbd>g</kbd>| Refresh the `*cider-spy*` buffer.
<kbd>r</kbd>| Reset the tracking data underpinning the `*cider-spy*` buffer.
<kbd>n</kbd>| Goto to next section.
<kbd>p</kbd>| Goto to previous section.
<kbd>a</kbd>| Set `CIDER-SPY-HUB` alias.
<kbd>s</kbd>| Send message to another dev (when cursor is on a dev).
<kbd>m</kbd>| Start a Multi-REPL with another dev's REPL session (when cursor is on a dev).
<kbd>w</kbd>| Watch another devs REPL session (when cursor is on a dev).
<kbd>d</kbd>| Disconnect from the `CIDER-SPY-HUB`.
<kbd>RETURN</kbd>| Visit section.
<kbd>TAB</kbd>| Toggle section visibility.

# Keyboard Shortcuts

These shortcuts are available on the `CIDER-SPY-MSG` buffer:

Keyboard shortcut                    | Description
-------------------------------------|-------------------------------
<kbd>C-c C-b</kbd>| Choose and send bookmark to developer.

# Background

This originally started life as a tool to help users learn about their own REPL behavour, and to function as an uber smart project navigation tool.

Since then it's morphed into being a tool to help developers share information, and to be a Multi-REPL.

![Cider Spy Menu](/images/menu.png?raw=true)

## Credits

Thanks to Magnars because unknown to him I used a lot of `magit` patterns and copied the `clj-refactor` build setup. Thanks also to Bozhidar Batsov and the read of the `CIDER` team.

# License

Copyright © 2016 Jon Pither
Distributed under the GNU General Public License, version 3
