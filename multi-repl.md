Below is a draft issue for the CIDER repo

# 2015-12-5

A good place to start is understanding the current REPL flow.
I.e when you hit return, it sends something to the nREPL server, then it waits for a response. I'd be interested to learn what mirroring setup exists when you do a humble `lein repl`.

cider-repl.el -> cider-repl--insert-prompt
cider-repl-return -> cider-repl--send-input -> cider-repl-handler -> cider-repl-emit-result

At some point ends up calling 'nrepl--eval-request'. I ought to see what the response of that is.

This feels like it could be more of a big emacs Hack. I guess the big things are to change the prompt and intercept - none of this is particulary difficult.

I ought to think more about how one connects to a shared repl? Be pretty cool if you could just eval a FN in the REPL to make it shared? Can two devs occupy the same port? Madness.

Thinking, you have to actively share a session - this means that requests and responses are captured and shared out. There may be a problem with the hub, the way it sends msgs. I.e. we probably want to keep a window of requests and responses, and the local nREPL server basically ensures it's up to date.

Thinking at the meta level, the most important step is the first. Simply write all REPL requests responses to a new CIDER buffer. We can do a simply command that starts capturing EVALs.

TODO differentiate between adhoc repl commands and loading files - this used to be the case but no more. Perhaps something in the metadata can help?

After an initial spike, looks like the only way is going to be intercepting the eval :-), Swapping out the transport.

# The Idea

Hi CIDER devs,

This is a issue to inform/discuss a related project I've been thinking about, and not of immediate concern to CIDER itself. @bbatov encouraged me to post an issue here for others to maybe provide some feedback on.

I'm contemplating the idea of building a multi-REPL. The idea that developers can connected to the same REPl instance. So instead of:

````
<*cider-repl companyx*>
companyx.some-ns>
````

As the prompt, I might see

````
<*shared-cider-repl companyx*>
jp companyx.some-ns>
````

When someone executes a command in the shared repl, then others see the command being entered, and the nREPL response.

I haven't comprehensively thought about what the actual point of this is, other than it would be a lot of fun. Imagine lots of people hacking in the same Overtone REPL, each changing the music being played, this kind of thing. There could also be a singular maintained REPL to a project environment, with the REPL interactions being displayed on a large screen - people could write all kinds of things to display via the REPL to display useful diagnostic/debug info.

To date, I've built cider-spy and cider-spy-nrepl (very badly named projects - immediately off-putting - and they should also exist in a single repo). The project itself was a bit of an experiment, a tool where people can share information on where other developers are doing in the code-base (i.e. what namespace they are eval'ing real time). Right now developers can chat to each other via their nREPL connections, and send to each other Emacs bookmarks to various places in the codebase. I left the project at this point for a year or so, and have only recently got it working again with the latest version of CIDER (although there is a spurious issue happening with the middleware latching on to the tooling session - I need to fix this, as I've fixed it for previous versions).

Anyway, why is this relevant? cider-spy uses a Netty based 'hub', a single running process that various nREPL instances relay data to. I've got the plumbing working so that when you create a CIDER nREPL session, the nREPL middleware establishes a connection to the CIDER SPY hub and exchanges data.

So the Multi-REPL thing could potentially make use of this - a well tested infrastructure for nREPL sessions running in separate processes to exchange data with a centralised hub.

What I'm wondering now is how this would work from a CIDER perspective, what code I should look at for creating a very similar REPL prompt.
