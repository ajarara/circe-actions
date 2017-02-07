# circe-actions.el
> IRC callback registration with minimal hair loss.

## What is circe?
[Circe][] is an IRC client for emacs sporting what most would call sane defaults. It has lots of features, not least of which is the ability to run arbitrary elisp code on many events.

[circe]: https://github.com/jorgenschaefer/circe

## What is circe-actions
A convenient interface to building callback-style functions to handle events emitted by circe.

Events can be messages, ctcp actions, nickserv ghosting, even [certain RPL codes][] like RPL_WELCOME and RPL_TOPIC.

[certain RPL codes]: https://tools.ietf.org/html/rfc2812#section-5

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

Now _everytime_ someone says something in #foo, you'll know about it. To disable all persistent handlers, M-x circe-actions-panic, or M-x circe-actions-disable gets rid of them. (As of now, there is no way to disable specific ones, as there isn't an easy way to present them to the user)


## How this works
As discussed in the walkthrough, Circe has an event handler table that holds all the events as keys and (possibly empty) lists as values. Circe-actions defines a primitive called ```circe-actions-activate-function``` which takes a function and a key of the handler table, and adds the function to right place in the event handler table. It keeps track of what functions were added in an association list, circe-actions-handlers-alist. When an action is deactivated, it is first looked for in the alist, and based on what key is stored there, it is deactivated in the key of the event handler table.

Thus, it is possible to have the same exact function registered to different events.

Speaking of registration, what goes on in circe-actions-register?

Well, not that much. Circe-actions-register takes the symbols passed to it, and generates a handler function, through the use of the aptly named ```circe-actions-generate-handler-function```.

In the first case, suppose we have a callback oriented use case, so we do NOT set the persist flag.

The handler generator function takes in the condition function, action function, a (in this case, generated and uninterned) symbol, and the event and constructs a function, returning it. It does NOT activate it. The generated function, when called (ie when on the handler table) applies the condition function to the arguments. When it returns non-nil, it immediately deactivates itself, and _then_ applies the same arguments to the action function. This is in case the action function takes long enough that the same event is emitted twice, causing it to be called again.

The persistence case is exactly the same, except it is never deactivated. It must either be deactivated in the action function (preferably at the beginning to avoid the situation above), or not activated at all. An example is shown in [Non-callback-style registration](#Non-callback-style-registration).

# Event signatures
Parameters are passed in the order described. If an event is not in this table, assume it follows the same signature as irc.message.
| Event name  | Description | Parameters |
|-------------|-------------|------------:|
| irc.message | Fired on every message or query | server-proc, event, fq-username, channel, contents |
