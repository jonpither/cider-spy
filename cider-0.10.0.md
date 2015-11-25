# Getting CIDER SPY working with 0.10.0

This is a journal of myself fixing CIDER-SPY to.... work again basically, with the latest version of CIDER - 0.10.0. I'm a bit intimidated, so I'm writing this journal to bolster my bravery.

## nrepl-send-request

`nrepl-send-request` has gone from taking 2 args to three:

* request
* callback
* connection

Where does the callback passed into `nrepl-send-request` get called?

`nrepl-client-filter` -> `nrepl--dispatch-response` is where the callback gets executed.

There is a cider-client that wraps nrepl-client. So there is a wrapped for `nrepl-send-request`: `cider-nrepl-send-request`. It uses `(cider-current-connection)`, this is the smart nREPL connection hunting algorithm I actually originated created in CIDER. `cider-default-connection` simply returns the default (or 'current') connection. The word `current` has changed somewhat in meaning.

My code is messing around with `(cider-default-connection)`.

## Middleware

The middleware just isn't being called. Difficult to know if it's a client side problem (elisp) or server-side (middleware).

Could be we're passing the wrong buffer through to send-request, i.e. the REPL buffer as oppose to the connection buffer. Not really, seems that's the way CIDER rolls now, `cider-default-connection` just returns the REPL buffer.

Tracked the problem down, the `session` in the message is an atom, not a string. Asking on the CIDER channel.

## Wrecked nREPL sessions

Fire up a project REPL
Do an eval, prove everything works, it should
Then fire up cider-spy-summary
Do an eval -> FAIL

Intermittently, we're seeing JSON wrapped in JSON being returned?

Not sure the JSON thing is related though - I had an occurance where JSON was as expected and the buffer worked perfectly first time. But I still got

````
Exception in thread "nREPL-worker-2" java.lang.ClassCastException: clojure.lang.Keyword cannot be cast to clojure.lang.Var
	at clojure.lang.Var.pushThreadBindings(Var.java:318)
	at clojure.core$push_thread_bindings.invoke(core.clj:1815)
	at clojure.core$with_bindings_STAR_.doInvoke(core.clj:1866)
	at clojure.lang.RestFn.invoke(RestFn.java:425)
	at clojure.tools.nrepl.middleware.interruptible_eval$evaluate.invoke(interruptible_eval.clj:56)
	at clojure.tools.nrepl.middleware.interruptible_eval$interruptible_eval$fn__704$fn__707.invoke(interruptible_eval.clj:191)
	at clojure.tools.nrepl.middleware.interruptible_eval$run_next$fn__699.invoke(interruptible_eval.clj:159)
	at clojure.lang.AFn.run(AFn.java:22)
	at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142)
	at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617)
	at java.lang.Thread.run(Thread.java:745)

````

SOLVED - was the session atom requires vars as keywords.

## Intermittent JSON error

And randomly disapearing stuff being tracking... Hmm - I think this is the good ol' fashioned nrepl session tooling thing?

## Remember CIDER-SPY

`cider-spy-connect-to-hub` is called when an nrepl-connection is established.
