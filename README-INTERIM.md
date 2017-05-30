# circe-actions.el
> Event driven callbacks for Circe (with minimal hair loss)
[Circe][] is an IRC client for emacs sporting what most would call sane defaults. It has lots of features, not least of which is the ability to run arbitrary elisp code on many events.

[circe]: https://github.com/jorgenschaefer/circe

Circe-actions is a convenient interface to building callback-style functions to handle the events emitted by circe. Events can be messages, ctcp actions, nickserv ghosting, even [certain RPL codes][] like RPL_WELCOME and RPL_TOPIC.

[certain RPL codes]: https://tools.ietf.org/html/rfc2812#section-5

## Table of contents

- [Quick Usage](#quick-usage)

## Quick usage

Most every IRC event in Circe has an associated event "hook", a list of functions to be run on each event. These hooks are all located in an internal hash table accessed by `(circe-irc-handler-table)`. 

When an event occurs, elements in the "hook" are called sequentially with this parameter signature:

| Parameter name | Description | Example |
| -------------- | ----------- | -------:|
| server-proc | The circe server processes associated with the event | bouncer.jarmac.org |
| event | The string name of an event | "nickserv.identified", "irc.message", "366" (RPL_ENDOFNAMES) |
| fq-username | The fully qualified username initiating the event  | "tux!~igloo@ip:ad:dr::ess" |
| target | The nick or channel the event is directed at | "#emacs", "fsbot" |
| contents | Depends on the event. An `irc.message` event stores message, a CTCP ping stores round trip time | "EXAMPLE NEEDED OF CTCP PING" |

Having to write functions with this giant parameter signature can be a pain. That's why circe-actions provides a macro to access their values easily.

``` elisp
(with-circe-actions-closure
  (message "%s said %s to %s!" :fq-username :contents :target))
```

Of course, the expression above doesn't do anything beyond generate a function. 

``` elisp
(circe-actions-activate-function
 (with-circe-actions-closure
  (message "%s said %s to %s!" :fq-username :payload :target))
 "irc.message")
 ```
 
 `circe-actions-activate-function` is a primitive of circe-actions. It takes a function and an event, and "registers" it so that it is called by Circe when the event occurs. It does this everytime Circe encounters the event. 
 
