# circe-actions.el
> Callback registration with minimal hair loss.

## What is circe?
[Circe][] is an IRC client for emacs sporting what most would call sane defaults. It has lots of features, not least of which is the ability to run arbitrary elisp code on many events.

[circe]: https://github.com/jorgenschaefer/circe

## What is circe-actions
A convenient interface to building callback-style functions to handle events emitted by circe.

Events can be messages, ctcp actions, nickserv ghosting, even [certain RPL codes][] like RPL_WELCOME and RPL_TOPIC.

[certain RPL codes]: https://www.alien.net.au/irc/irc2numerics.html

## Table of contents

- [Walkthrough](#Walkthrough)
- [More example usage](#More-example-usage)
- [How this works](#How-this-works)
- [circe-actions-plistify](#circe-actions-plistify)
- [Event signatures](#Event-signatures)
- [Parameter description](#Parameter description)
- [Non-callback-style registration](#Non-callback-style-registration)

## Walkthrough

Circe has an hash table internally, accessed by a function called ```circe-irc-handler-table```.

Being an IRC client, Circe is naturally responsible for handling all sorts of IRC events. When it handles an event, it also runs everything in ```circe-irc-handler-table``` associated with the type of the event, and passes them arguments. Circe doesn't care about the return values of the statements, all it does is make sure that they run without error.

Let's say we want to be notified in the minibuffer when the next activity in a specific channel is. Ignore the fact that tracking.el (distributed with Circe) does this better.

We want to check for the next "irc.message" event in channel "#foo". Conceptually, we have three things here: a condition we want satisfied, an action we want done on that condition being satisfied, and the event we're concerning ourselves with.

### Condition function
In this case, it would be:

``` elisp
(defun activity-in-foo (&rest args)
    (let ((easy-args (circe-actions-plistify args "irc.message")))
	  (equal (plist-get :target easy-args)
	         "#foo")))
```

Let's tackle some questions you might have. What's circe-actions-plistify?

Circe-actions-plistify makes it easy to get what you want from the arguments passed to the function. Afaict, there are 5 passed to the function, and it's arduous to remember. So instead we access them by what we want out. Yes, it's an O(N) operation but do you want processor cycles or sanity? (If you're too far gone, check [event signatures](#Event-signatures) for what you want)

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

Easy enough, we just get the payload (think contents of a message) from easy-args and message it out.

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

Now _everytime_ someone says something in #foo, you'll know about it. To disable all persistent handlers, M-x circe-actions-panic, or M-x circe-actions-disable


