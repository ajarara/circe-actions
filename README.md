# circe-actions.el
> IRC callback registration with minimal hair loss.

[Circe][] is an IRC client for emacs sporting what most would call sane defaults. It has lots of features, not least of which is the ability to run arbitrary elisp code on many events.

[circe]: https://github.com/jorgenschaefer/circe

Circe-actions is a convenient interface to building callback-style functions to handle the events emitted by circe, based entirely on circe-irc-handler-table.

Events can be messages, ctcp actions, nickserv ghosting, even [certain RPL codes][] like RPL_WELCOME and RPL_TOPIC.

[certain RPL codes]: https://tools.ietf.org/html/rfc2812#section-5

## Table of contents

- [Walkthrough](#walkthrough)
- [Utility functions](#utility-functions)
- [Internals of circe-actions](#internals-of-circe-actions)
- [circe-actions-plistify](#circe-actions-plistify)
- [Event signatures](#event-signatures)
- [Parameter description](#parameter description)
- [Non-callback-style registration](#non-callback-style-registration)

## Walkthrough

First off, Circe has a hash table accessed by ```circe-irc-handler-table``` that has events as keys and functions as values.

Being an IRC client, Circe is naturally responsible for handling all sorts of IRC events. When it handles an event, it also runs everything in circe-irc-handler-table associated with the type of the event, and passes them arguments. Circe doesn't care about the return values of the statements, all it does is hope that they run without error (because then Circe can't finish processing the event.)

Let's say we want to be notified in the minibuffer when the next activity in a specific channel is. Ignore the fact that tracking.el (distributed with Circe) does this better.

We want to check for the next "irc.message" event in channel "#foo". Conceptually, we have three things here: 
 - a condition we want satisfied
 - an action we want done on that condition being satisfied
 - the event we're concerning ourselves with.

### Condition function
Most of the work, strangely enough, is in remembering the function signature (the list of arguments a function takes) for the event.

``` elisp
(defun activity-in-foo-long (server-proc event fq-username target payload)
   (equal target "#foo"))
```

The byte compiler whines when a function names arguments and doesn't use them, so we could put underscores in the ones we don't use, but already we should be alarmed, this probably isn't the best way to do things. Instead, we can do it like this:


``` elisp
(defun activity-in-foo (&rest args)
    (let ((easy-args (circe-actions-plistify args "irc.message")))
	  (equal (plist-get :target easy-args)
	         "#foo")))
```

Let's tackle some questions you might have. What's circe-actions-plistify?

Circe-actions-plistify makes it easy to get what you want from the arguments passed to the function. There are 5 passed to the function, and it's arduous to remember the order every time. So instead we access them by what we want using plist-get. Yes, it's an O(N) operation but do you want processor cycles or sanity? (If you're staunch about using the first example, check [event signatures](#Event-signatures) for what you want)

Another question: Why is the property called :target, and not :channel?

"irc.message" is also fired when a private message is sent. The :target in this case would be whoever the private message was sent to (so in all real world cases, your nick). So it's trivial to change the above to return true when you get a private message, just change "#foo" to "$nick". (There are less awkward ways to do this, remember, we're just learning here!)

Now that we have the condition function out of the way... 

### Action function
We'd like to alert ourselves in the minibuffer of what was said:

``` elisp
(defun spit-out-payload (&rest args)
  (let ((easy-args (circe-actions-plistify args "irc.message")))
    (message "Activity in #foo: %s" (plist-get :payload easy-args))))
```

Easy enough, we just get the payload (think contents of a message) from easy-args and message it. Here's the equivalent without using plistify:

``` elisp
(defun spit-out-payload (_server-proc _event _fq-username _target payload)
  (message "Activity in #foo: %s" payload))
```

### Registration
Following along so far? Here's the hard part:

``` elisp
(circe-actions-register 'activity-in-foo 'spit-out-payload)
```

That's it. The next time activity-in-foo returns true (or non-nil), spit-out-payload is run with the same arguments.

This only occurs once. If you want it to persist, set the persist flag:

``` elisp
(circe-actions-register 'activity-in-foo 'spit-out-payload t)
```
Notice the "t".

Now _everytime_ someone says something in #foo, the minibuffer'll know about it. To disable all persistent handlers, M-x circe-actions-panic, or M-x circe-actions-disable gets rid of them. (As of now, there is no way to disable specific ones, as there isn't an easy way I can think of to present them to the user)

Of course, there is another way to handle other non-callback use cases, see [non-callback-style registration](#non-callback-style-registration)

## circe-actions-panic
In the case that something is tripping the debugger 3 times a second, you'll probably want this. It iterates through the alist holding all the registered functions and removes them from the handler table (and the alist). This function is also called when you call M-x disable-circe-actions.

# Utility functions
Circe-actions takes the liberty of defining loads of helpful closures, to help you save every follicle in these trying times.

__Note:__ These _return_ functions to be used as predicates, they are not predicates themselves. The whole point is so that you don't have to set up lexical binding in your init file to make these closures without resorting to dynamically scoped alists if you don't want to.

## circe-actions-t
In case you want to capture the next event unconditionally, you may be tempted to use t as a condition function. This won't work. Instead, you must wrap t in a lambda that takes in the correct number of arguments. circe-actions-t is exactly this. In this case, there is no closure. All of the following are closures.


## circe-actions-is-from-p
Usage: (circe-actions-from-p "alphor!~floor13@2604:180:2::10")

Returns a closure that when evaluated with the right arguments, returns true when the event was caused by "alphor!~floor13@2604:180:2::10".

Wait does this mean that you can only reliably target cloaks? Yes. This is more useful for ZNC, when you want to make absolutely sure you got the message from the right entity. But don't worry, my child:

## circe-actions-hippie-is-from-p
Usage: (circe-actions-hippie-is-from-p "alphor!~")

Returns a closure that when evaluated with the right arguments, returns true when the event caused by the sender starts with "alphor!~"

## circe-actions-sent-to-p
Usage: (circe-actions-sent-to-p "alphor!~floor13@2604:140:76::5")

Returns a closure that when evaluated with the right arguments, returns true when the event is targeted at "alphor!~floor13@2604:140:76::5"

## circe-actions-hippie-sent-to-p
Usage: (circe-actions-hippie-sent-to-p "alph")

Returns a closure that when evaluated with the right arguments, returns true when the event is targeted at any nick that starts with "alph", including "alphor", "alph", but not "ALF" the [friendly extraterrestrial][]. He doesn't use IRC these days anyway.

[friendly extraterrestrial]: https://en.wikipedia.org/wiki/ALF_(TV_series)

# Event signatures
Parameters are passed in the order described. If an event is not in this table, assume it follows the same signature as irc.message.

| Event name  | Description | Parameters |
| ----------- | ------------- | ------------:|
| irc.message | Fired on every message or query | server-proc, event, fq-username, channel, contents |


# Internals of circe-actions
This part is long, and completely unnecessary to read if you're just using circe-actions to build your own extensions.

As discussed in the walkthrough, Circe has an event handler table that holds all the events as keys and (possibly empty) lists as values. Circe-actions defines a primitive called ```circe-actions-activate-function``` which takes a function and a key of the handler table, and adds the function to right place in the event handler table. It keeps track of what functions were added in an association list, circe-actions-handlers-alist. When an action is deactivated, it is first looked for in the alist, and based on what key is stored there, it is deactivated in the key of the event handler table.

Thus, it is possible to have the same exact function registered to different events.

Speaking of registration, what goes on in circe-actions-register?

Well, not that much. Circe-actions-register takes the symbols passed to it, and generates a handler function, through the use of the aptly named ```circe-actions-generate-handler-function```, reproduced here:

``` elisp
(defun circe-actions-generate-handler-function
	(condition-p-function action-function symbol event &optional persist)
  (defalias symbol
    (lambda (server-proc event &rest rest-args)
      (let ((args (cons server-proc (cons event rest-args))))
        (when (apply condition-p-function args)
          (unless persist
            (circe-actions-deactivate-function symbol event))
          (apply action-function args))))))
```

In the first case, suppose we have a callback oriented use case, so we do NOT set the persist flag.

The handler generator function takes in the condition function, action function, a (in this case, generated and uninterned) symbol, and the event and constructs a function, returning it. It does NOT activate it. The generated function, when called (ie when on the handler table) applies the condition function to the arguments. When it returns non-nil, it immediately deactivates itself, and _then_ applies the same arguments to the action function. This is in case the action function takes long enough that the same event is emitted twice, causing it to be called again.

The persistence case is exactly the same, except it is never deactivated. It must either be deactivated in the action function (preferably at the beginning to avoid the situation above), or not activated at all. An example is shown in [Non-callback-style registration](#Non-callback-style-registration).

