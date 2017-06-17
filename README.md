# circe-actions.el

[Circe][] is an IRC client for emacs sporting what most would call sane defaults. It has lots of features, not least of which is the ability to run arbitrary elisp code on many events.


[circe]: https://github.com/jorgenschaefer/circe

Circe-actions is a convenient interface to building callback-style functions to handle the events emitted by circe. Events can be messages, ctcp actions, nickserv ghosting, even [certain RPL codes][] like RPL_WELCOME and RPL_TOPIC
.

[certain RPL codes]: https://tools.ietf.org/html/rfc2812#section-5

## Table of contents

- [Quick Usage](#quick-usage)
- [Modifying closure behavior](#modifying-closure-behavior)
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
| contents | Depends on the event. An `irc.message` event stores message, `irc.ctcp.ping` stores round trip time | "Stormtrooper2: These aren't the droids we're looking for." |

This is defined in `circe-actions-default-event-signature`, and of course can be changed.

Having to write functions with this large parameter signature can be a pain. Circe-actions provides a facility to generate these functions while providing a convenient interface to access their arguments.

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

To make the above persistent, simply set the persist flag:

``` elisp
(circe-actions-register
  "irc.message"
  (condition ...)
  (action ...)
  t)
```

This makes it so that everytime condition occurs, action executes for the rest of your emacs session.

To clear all active event handlers, run `circe-actions-panic` or `circe-actions-disable`. Both will remove everything. As of now there is no easy way to remove individual elements from the event table.


## Modifying closure behavior
### :prefix
Under the hood, `with-circe-actions-closure` generates a lambda expression that replaces all keywords (that is, symbols that look like `:this`) with an expression that pulls them out of a plist made available at execution. Namely, _all_ keywords get modified in this way. By default.

To preserve symbols, with-circe-actions-closure itself provides a keyword argument that determines what symbols to transform.

Usage:
``` elisp
(with-circe-actions-closure
 :prefix ":!"
 :expr (list :these :wont :be :transformed :but :!these :!will))
;; yields
(lambda (&rest args)
  (let ((easy-args (circe-actions-plistify args)))
   (list :these :wont :be :transformed :but
     (plist-get easy-args :these)
     (plist-get easy-args :will))))
```
### :signature
Circe-actions can automatically figure out what event is being handled from the context of the execution environment. Sounds impressive, but unfortunately there's no wizardry here, Circe passes the event as the 2nd argument.

In case this 'heuristic' fails, provide :signature to the call of `with-circe-actions-closure`, like so:

``` elisp
(with-circe-actions-closure
 :signature "irc.ctcp.PING"
 :expr (when (> (string-to-number :contents) 200)
         (message "Too slow!")))
```

## Gotchas

The below will not work if not evaluated with lexical scoping (emacs' default is dynamic)
 
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

There isn't a way to return values without resorting to creating another callback, nested inside it. Here is an example of this:

## Non-callback style registration
As mentioned in [quick usage](#quick-usage), circe-actions is designed for callbacks. However it certainly is possible that we want to capture the nth event, or wait for a series of conditions to happen in order before doing something, or some other creative scenario. There are only two functions necessary to use here: circe-actions-activate-function, and circe-actions-deactivate-function.

Also mentioned in the quick usage section, activation of a function with respect to a specific event makes it get called _every time_ the event occurs. This means that we have to handle the deactivation step ourselves (unless we don't want to deactivate the function, of course).

``` elisp
;; we need closures to illustrate this example
(setq lexical-binding t)
(setq event "irc.message")

(defun my-listen-5-times-then-quit-handler ()
  (let ((func-sym (gensym "arbitrary-"))
        (count 0))
    (defalias func-sym
      (with-circe-actions-closure
         (message "%s" :payload)
         (setq count (1+ count)) 
         (when (>= count 4)
           (circe-actions-deactivate-function
            func-sym
            event))))))

(circe-actions-activate-function
 (my-listen-5-times-then-quit-handler)  ; return a new independent closure
 event)
```

Of course this could all be wrapped into a single command, fit for binding to a key:
``` elisp
(setq lexical-binding t)
(setq event "irc.message")  ; this can be pushed into the definition as well

(defun message-five-times-then-quit ()
  (interactive)
  (let ((func-sym (gensym "arbitrary-"))
        (count 0))
    (defalias func-sym
      (with-circe-actions-closure
       (let ((func-sym func-sym))
         (message "%s" count :payload)
         (setq count (1+ count)) 
         (when (> count 4)
           (circe-actions-deactivate-function
            func-sym
            event)))))))


(add-to-hook 'circe-mode
             (lambda () 
               (local-set-key (kbd "C-c i") 
                              `message-five-times-then-quit)))
```


# Event signatures
Parameters for an event are passed in the order listed. Prepend a ":" to get its respective event value. If an event is not in this table, assume it follows the same signature as irc.message.

| Event name  | Description | Parameters |
| ----------- | ------------- | ------------:|
| irc.message | Fired on every message or query | server-proc, event, fq-username, target, contents |

If there is a different signature, please open an issue or a PR, both are welcome!


# Internals of circe-actions
This part is long, and completely unnecessary to read if you're just using circe-actions to build your own extensions.

As discussed in the quick usage, Circe has an event handler table that holds all the events as keys and (possibly empty) lists as values. Circe-actions defines a primitive called `circe-actions-activate-function` which takes a function and a key of the handler table, and adds the function to the right place in the event handler table. It keeps track of what functions were added in an association list, circe-actions-handlers-alist. When an action is deactivated, it is first looked for in the alist, and based on what key is stored there, it is deactivated in the key of the event handler table.

Thus, it is possible to have the same exact function registered to different events.

Speaking of registration, what goes on in circe-actions-register?

Well, not that much. Circe-actions-register takes the symbols passed to it, and generates a handler function, through the use of the aptly named `circe-actions-generate-handler-function`, reproduced here:

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

# Error handling on the handler table
If an error occurs during a callback generated by `circe-actions-generate-handler-function` (used by `circe-actions-register`), circe-actions takes the liberty of deactivating the function immediately after the error, warning the user of what went wrong. To disable this, either do M-x `toggle-debug-on-error` or don't use circe-actions-register to build your callbacks.

```elisp
(circe-actions-register
  "irc.message"
  (with-circe-actions-closure
    (+ 5 "Wait a minute... this isn't javascript"))
  (with-circe-actions-closure
    (message "will never get here")))
;; => Error running event "irc.message" handler circe-actions-gensym-44: (error "Callback failed with error: Wrong type argument: number-or-marker-p, \"Wait a minute... this isn't javascript\"") (args were (#<process jarmac.org> "irc.message" "fsbot!~fsbot@unaffiliated/deego/bot/fsbot" "alphor" "If I want to talk to nerds using obsolete software, there's uum, IRC"))
```


# Additional notes
Now that we have a nice interface to creating predicate and action functions, we should rewrite all of the utilities, and farm them out to circe-actions-utils.el, so that they can be loaded in as needed.

A nicety would be to emulate iptables chains. This cuts down on the problem of 'how to interactively remove errant rules' without removing everything.

You can deactivate functions by equality. That is... 

``` elisp
;; eq tests if the two lisp objects are references to the same obj
(eq (with-circe-actions-closure t)
    (with-circe-actions-closure t))
;; => nil

(with-circe-actions-closure (message "%s" :contents))
;; => (lambda (&rest circe-actions--args) (let ((circe-actions--plistified-args (circe-actions-plistify circe-actions--args nil))) (message "%s" (plist-get circe-actions--plistified-args :contents))))

;; pretty printed:
(lambda (&rest circe-actions--args)
  (let ((circe-actions--plistified-args
         (circe-actions-plistify circe-actions--args nil)))
    (message "%s" (plist-get circe-actions--plistified-args :contents))))


(circe-actions-activate-function
  "irc.message"
  (with-circe-actions-closure
    (message "%s" :contents)))
;; => (circe-actions--gensym-##)
    
;; feel free to pause here and wait for a message
;; to come into the minibuffer to verify it works

;; or trust the package itself:
(circe-actions-is-active-p
  "irc.message"
  (with-circe-actions-closure
    (message "%s" :contents)))
;; => (circe-actions--gensym-##)
   

(circe-actions-deactivate-function
 "irc.message"
 (lambda (&rest circe-actions--args)
   (let ((circe-actions--plistified-args
          (circe-actions-plistify circe-actions--args nil)))
     (message "%s" (plist-get circe-actions--plistified-args :contents))))
;; => nil
    
(circe-actions-is-active-p
  "irc.message"
  (with-circe-actions-closure
    (message "%s" :contents)))
;; => nil
```

One thing that might be a little confusing: these return values don't necessarily all point to the same thing. Depending on the function called, you may be looking at the contents of the alist, or you may be looking at the contents of one bucket on the handler table. In some cases this is necessary, and in some, this is arbitrary.I've outlined the necessary cases in the source comments. Don't use these return values, I'm only using them here for demonstration.
