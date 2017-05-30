# circe-actions.el
> Event driven callbacks for Circe (with minimal hair loss)

[Circe][] is an IRC client for emacs sporting what most would call sane defaults. It has lots of features, not least of which is the ability to run arbitrary elisp code on many events.


[circe]: https://github.com/jorgenschaefer/circe

Circe-actions is a convenient interface to building callback-style functions to handle the events emitted by circe. Events can be messages, ctcp actions, nickserv ghosting, even [certain RPL codes][] like RPL_WELCOME and RPL_TOPIC
.

[certain RPL codes]: https://tools.ietf.org/html/rfc2812#section-5

## Table of contents

- [Quick Usage](#quick-usage)
- [Gotchas](#gotchas)
- [Non-callback style registration](#non-callback-style-registration)
- [Event signatures](#event-signatures)
- [Circe-actions internals](#circe-actions-internals)
- [Additional notes](#additional-notes)

## Quick usage

Most every IRC event in Circe has an associated event "hook", a list of functions to be run on each event. These hooks are all located in an internal hash table accessed by `(circe-irc-handler-table)`. 

In all known cases, when an event occurs, elements in the "hook" are called sequentially with this parameter signature:

| Parameter name | Description | Example |
| -------------- | ----------- | -------:|
| server-proc | The circe server processes associated with the event | bouncer.jarmac.org |
| event | The string name of an event | "nickserv.identified", "irc.message", "366" |
| fq-username | The fully qualified username initiating the event  | "tux!~igloo@ip:ad:dr::ess" |
| target | The nick or channel the event is directed at | "#emacs", "fsbot" |
| contents | Depends on the event. An `irc.message` event stores message, a CTCP ping stores round trip time | "Stormtrooper2: These aren't the droids we're looking for." |

Having to write functions with this large parameter signature can be a pain. Circe-actions provides a facility to generate these functions while providing a convenient interface to access them.

``` elisp
(with-circe-actions-closure
  (message "%s said %s to %s!" :fq-username :contents :target))
```

Of course, the expression above doesn't do anything beyond generate a function. In order to have Circe run it, we must register it like so:

``` elisp
(circe-actions-activate-function
  (with-circe-actions-closure
    (message "%s said %s to %s!" :fq-username :contents :target))
  "irc.message")
 ```
 
 `circe-actions-activate-function` is a primitive of circe-actions. It takes a function and an event, and registers it so that it is called by Circe when the event occurs. Once activated in this way, Circe will call the function each time the event occurs.
 
 Most use cases don't call for persistent event handling. Instead, circe-actions is geared towards one-shot callback workflows: register a callback, provoke a specific response, handle the response. For this, use `circe-actions-register`.
 
 Conceptually, we want three things here:
 ``` elisp
 (circe-actions-register
   ;; an event type we want to listen in on
   "irc.message"
   ;; a condition we want satisfied by the event
   (with-circe-actions-closure
     (string-prefix-p "fsbot" :fq-username))
   ;; an action we want done within the context of the event
   (with-circe-actions-closure
     (message "%s sent: %s" :fq-username :contents)))
```

To clear all active event handlers, run `circe-actions-panic` or `circe-actions-disable` Both will remove everything. As of now there is no easy way to remove individual elements from the event table.

## Gotchas

The below will not work if not evaluated with lexical scoping (emacs default is dynamic)
 
 ``` elisp
(let ((user "fsbot"))
 (circe-actions-register
  "irc.message"
  (with-circe-actions-closure
   (string-prefix-p user :fq-username))
  (with-circe-actions-closure
   (message "%s sent: %s" user :contents))))
```
The reason this doesn't work is that once the callback is actually evaluated, `user` is no longer within scope.

Instead, incorporate it within the closure like so:

 ``` elisp
(let ((user "fsbot"))
 (circe-actions-register
  "irc.message"
  (with-circe-actions-closure
   (let ((user user))
    (string-prefix-p user :fq-username)))
  (with-circe-actions-closure
   (let ((user user))
    (message "%s sent: %s" user :contents)))))
```


 
 
 
