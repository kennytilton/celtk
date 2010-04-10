#|

 This software is Copyright (c) 2003, 2004, 2005, 2006  Peter Herth <herth@peter-herth.de>
 Parts Copyright (c) 2005 Thomas F. Burdick
 Parts Copyright (c) Cadence Design Systems, GmbH

 Peter Herth grants you the rights to distribute
 and use this software as governed by the terms
 of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html),
 known as the LLGPL.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!             PROMINENT NOTICE      !!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!                                   !!!!!!!!!!!!!!!
!!!!!!!!!!!! This demo was translated to Cells !!!!!!!!!!!!!!!
!!!!!!!!!!!! by ken Tilton on March 22, 2006.  !!!!!!!!!!!!!!!
!!!!!!!!!!!!                                   !!!!!!!!!!!!!!!
!!!!!!!!!!!! Original (ltktest) can be found   !!!!!!!!!!!!!!!
!!!!!!!!!!!! at the end of ltk.lisp            !!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

|#


(in-package :celtk-user)
#|

The comments throughout this source file cover two broad topics:

    How is programming with Celtk different from LTk?

Contrast the code below with the excellent ltktest "classic" in ltk.lisp to 
see how Celtk programming is different. I won't say better, because some people prefer an
imperative approach where they can have all the bricks laid out in front of them
and sequence them manually one by one to get exactly what they want without thinking
very hard. The declarative approach makes one think a little harder but in the end
do less work as the responsibility for getting things to work falls on the engine behind
the declarative interface.

Second topic:

    How is programming with Cells different from without Cells?

Those questions are different because not everything different about Celtk
depends on Cells. 

Note: explanatory comments appear after the explained code.

n.b. The paint is very fresh on Celtk, so if something like the Timer class looks
dumb, well, it may be. Example: the scroller class only scrolls a canvas (well, I have not tried
supplying a frame for the canvas slot, maybe it would work, but the slot name at least is 
certainly wrong (or the class should be canvas-scroller).

|#
#+test-ltktest
(progn
  (cells-reset 'tk-user-queue-handler)
  ;
  ; Tk is fussy about the order in which things happen. It likes:
  ;    - create widgets .x and .y
  ;    - make .x the -textvariable of .y
  ;    - set .x to "Hi, Mom"
  ;
  ; Tk does not like Step 3 going before Step 2. That is, .y will not learn about "Hi, Mom.".
  ; Unfortunately, in a declarative paradigm one does not specify in what order different 
  ; things should happen, one just specifies the things we want to have happen. An underlying
  ; engine then runs around taking care of making that happen, without bothering the developer
  ; about how to do that. That includes deciding in what order to make those things happen. That is 
  ; a big win when it works. When it did not work for Tk, and I could imagine the same thing
  ; coming up again in other situations (Tilton's Law: "The first time you run into something
  ; is just the first time you ran into it"), I added to Cells the concept of a "client queue",
  ; where client-code can store order-sensitive tasks. The client also can specify the handler for
  ; that queue, here 'tk-user-queue-handler. This handler (or the default FIFO handler) gets called 
  ; at just the right time in the larger scheme of state propagation one needs for 
  ; data integrity. What is that scheme?
  ;
  ; Data integrity: when the overall Cells data model gets perturbed by imperative code -- typically in an
  ; event loop -- executing a SETF of some datapoint X, we want these requirements met:
  ;
  ;   - recompute all and (for efficiency) only state computed off X (directly or indirectly through some intermediate datapoint);
  ;
  ;   - recomputations, when they read other datapoints, must see only values current with the new value of X;
  ;
  ;   - similarly, client observers ("on change" callbacks) must see only values current with the new value of X; and
  ;
  ;   - a corollary: should a client observer SETF a datapoint Y, all the above must
  ;     happen with values current with not just X, but also with the value of Y /prior/
  ;     to the change to Y.
  ;
  ; To achieve the above, Cells2 and now Cells3 have taken to using FIFO "unfinished business" queues 
  ; to defer things until The Right Time. Which brings us back to Tk. Inspect the source of
  ; tk-user-queue-handler and search the Celtk source for "with-integrity (:client" to see how Celtk
  ; manages to talk to Tk in the order Tk likes. And hack the function tk-format-now to have
  ; Celtk dump the code it has evaluated by TCL/Tk during initialization, and notice how un-random it looks. You can
  ; then comment out the above specification of a Tk-savvy handler to see (a) the order that would have happened
  ; before Cells3 and (b) the demo collapse in a broken heap. 
  ;
  ; But in short, with Cells3  we just add this requirement:
  ;  
  ;   - Deferred "client" code must see only values current with X and not any values current with some
  ;     subsequent change to Y queued by an observer
  ; 
  (ctk:test-window 'ltktest-cells-inside))

; That is all the imperative code there is to Celtk application development, aside from widget commands, and those
; invariably (?) consist of a single setf. So where does the rest of the state change necessary to keep a GUI
; interface self-consistent get taken care of? 

; Tk handles some of the driving imperative logic -- they call the company ActiveState for a reason -- and Celtk internals 
; handle the rest. The application works via Cells rules reacting to change by computing new state for the application model, 
; which operates on the outside world via observers (on-change callbacks) triggered
; automatically by the Cells engine. See DEFOBSERVER.

(defun ctk::ltktest-ci ()
  (cells-reset 'tk-user-queue-handler)
  (ctk:test-window 'ltktest-cells-inside))

(defmodel ltktest-cells-inside (window)
  ()
  
  (:default-initargs
      :id :ltk-test
    :kids (c?
           ; c? has quite an expansion. Functionally, one gets:
           ;   - a first-class anonymous function with the expected body, which will have access to...
           ;   - lexical variables self and .cache for the instance and prior  computed value, if any
           ;   - guaranteed recomputation when the value of any other cell /used in the most recent computation/ changes
           ;
           ; If the abbreviation c? alarms you, look up c-formula.
           ;
           (the-kids
            ;
            ; Cells GUIs get a lot of mileage out of the family class, which is perfect
            ; for graphical hierarchies. "the-kids" does not do much, btw.
            ;
            (ltk-test-menus) ;; hiding some code. see defun below for deets
            (mk-scroller
             ;
             ; These "mk-" functions do nothing but expand into (make-instance 'scroller <the initarg list>)
             ; and supply the "parent" :initarg necessary in Family trees.
             ;
             ; Where you see, say, mk-button-ex I am (a) poking fun at Microsoft naming of second generation
             ; library code that did not want to break existing code and (b) adding a little more value (just
             ; inspect the macro source to see how).
             ;
             :packing (c?pack-self "-side top -fill both -expand 1")
             ;
             ; Here is an example of how the Family class helps. The above is one of only two packing
             ; statements needed to recreate the ltktest demo. Other packing is handled via two
             ; slots in an inline-mixin class for various family subclasses, kids-layout and
             ; kids-packing. The latter pulls any packing parameters and all kids into one
             ; big pack statement kicked off by an observer on that slot. See the inline-mixin
             ; class to see how this works.
             ;
             ; See the scroller class to see some automation of grids (but this was my first experience
             ; with grids so look for that to get enhanced over time -- and later automation
             ; of the use of PLACE.
             ;
             :canvas (c? (make-kid 'ltk-test-canvas))) ;; hiding some code. see defmodel thereof below
            ;
            ; My bad. Scroller should not assume a canvas is the scrollee. To be refined.
            ;
            
            (mk-row (:packing (c?pack-self "-side bottom"))
              ;
              ; Just expand mk-row to see what is going on. It is pretty neat in one respect: if the
              ; first row parameter is a string, it knows to make a labelframe instead of plain frame)
              ; The other thing it does, by forcing row parameters into a sub-list as the first argument,
              ; is let the programmer then just list other widgets (see next) which are understood to
              ; be kids/subwidgets contained by the frame.
              ;
              (mk-row (:borderwidth 2 :relief 'sunken)
                (mk-label :text "Rotation:")
                ;
                ; As with Ltk Classic, the Tk widget configurations become Lisp widget initializers, so
                ; the Tk doc documents Celtk. The advantage to the developer is that neither LTk nor
                ; Celtk introduce a new API to be mastered, widget-wise.
                ;
                (mk-button-ex ("Start" (setf (moire-spin (fm^ :moire-1)) t)))
                ;
                ; You were warned about mk-button-ex and its ilk above. Just expand or inspect to
                ; see what they do, which is pretty much just hide some boilerplate.
                ;
                ; fm^ is a wicked abbreviation for "search up the Family tree to find the widget
                ; with this ID". ie, The Family tree effectively becomes a namespace of IDs. I have a suite of
                ; routines that search the namespace by name so one widget can operate on or,
                ; more commonly, ask for the value of a slot of some specific widget known to
                ; be Out There somewhere. (Kids know their parents, so the search can reach
                ; anywhere in the tree.)
                ;
                ; OK, now what is going on here? The above command starts the canvas display
                ; spinning, by tweaking (via the (setf moire-spin) defun below) the "repeat" slot of 
                ; an ad hoc "moire" class object created to render the pretty design from
                ; ltktest. How it accomplishes that will be explained below in the moire class
                ; definition.
                ;
                (mk-button-ex ("Stop" (setf (moire-spin (fm^ :moire-1)) nil)))) ;; ditto
              
              (mk-button-ex ("Hallo" (format t "~&Hallo")))
              (mk-button-ex ("Welt!" (format t "~&Welt!")))
              (mk-row (:borderwidth 2 :relief 'sunken)
                (mk-label :text "Test:")
                (mk-button-ex ("OK:" (setf (moire-spin (fm^ :moire-1)) 100))))
              ;
              ; Cells initiata will be surprised to learn the above works twice even if the button is
              ; clicked twice in a row; Cells is about managing state change, and the second time through
              ; there is no change. But the system still reacts! See the Timer class for the shocking 
              ; solution to this riddle.
              ;
              (mk-entry-numeric :id :point-ct
                :value (c-in "42")
                ;
                ; to help motivate "why Cells?" a little more, we deviate from ltktest 'classic" and
                ; start having the widgets take more interesting effect: The entry field now determines the number
                ; of points to generate for the canvas line item, which originally was fixed at 100.
                ; see the moire class for details.
                ;
                :num-parse (c? (eko ("numparse")
                                 ;
                                 ; (eko is a utils-kt debug hack that prints a value along with arbitrary
                                 ; other info before returning the value to the inquirer)
                                 ;
                                 ; Here we supplement the standard entry-numeric parse rule with
                                 ; our own more stringent rule that knows about the moire task ahead.
                                 ;
                                 ; A vital point with this entry-numeric class (invented just now for
                                 ; this demo) is that Cells does not get in the way of CLOS. We are
                                 ; subclassing, using initforms, default-initargs, and, what I suspect is
                                 ; a big reason Cells are such a big win: different instances of the same 
                                 ; class do not need to have the same rules for the same slot. Or even
                                 ; have rules at all; other instances can have a constant or be setffable 
                                 ; from outside the model.
                                 ;
                                 (handler-case
                                     (let ((num (parse-integer (^value))))
                                       (cond
                                        ((< num 2)
                                         (list (format nil "Yo, Euclid, at least two, not: ~a!!" num)))
                                        ((> num 200)
                                         (list (format nil "Bzzt! ~a points will not look so hot." num)))
                                        (t num)))
                                   (parse-error (c)
                                     (princ-to-string c)))))
                :background (c? (if (user-errors (fm! :point-ct)) 
                                    "red"
                                  'SystemButtonFace))) ;; TK won't allow "" as a way of saying "default color"
              ;
              ; As you edit the field, if you key in an invalid (non-digit) character, the background
              ; immediately turns red. Delete it and it reverts to the default.
              ;
              ; The interesting question is, how does the value slot of the Lisp instance stay
              ; current with the text being edited in the Tk entry widget? Here we have a fundamental
              ; difference between Ltk and Celtk. Ltk lets Tk take care of everything, including
              ; storing the data. eg, (text my-entry) is an accessor call that asks Tk the value of
              ; the -text configuration for the Tk instance mirrored by my-entry. There is no text
              ; slot in the Lisp entry instance. Makes for nice, lightweight Lisp instances. But Cells works
              ; by having datapoints watching other datapoints, so we want data in the Lisp domain
              ; changing automatically as it changes on the TK side (such as when the user is actually
              ; typing in the entry widget). See the entry class to see how it uses the TCL "trace write"
              ; mechanism to keep the Lisp value slot abreast of the Tk entry text configuration
              ; keystroke by keystroke. 
              ; 
              ; I added the :user-errors rule above to demonstrate the mechanism in action. Click
              ; on the entry widget and type "123abc", then delete the alpha characters. The background
              ; color (as well as the File\Save menu item state) tracks the typing.
              ;
             
              (mk-button-ex ("Print" (format t "~&User wants to see ~A points" (fm^v :point-ct))))
              ;
              ; (fm^v :point-ct) -> (value (fm^ :point-ct))
              ;
              ; The idea being that every Cells model object has an value slot bearing the value
              ; of the thing being modeled. Here, the entry widget is modelling a place for users
              ; to supply information to an application, and the value slot is a good place to
              ; keep that information.
              ;
              ; Thus each class uses value to hold something different, but in all cases it is
              ; the current value of whatever the instance of that class is understood to hold. 
              ; 
              (mk-button-ex ("Reset" (setf (fm^v :point-ct) "42")))
              ;
              ; Driving home this point again, in Ltk one would SETF (text my-entry) and the
              ; SETF method would communicate with Tk to make the change to the Tk widget -text
              ; configuration. In Celtk, the value slot of the entry gets changed (possibly
              ; triggering other slots to update, which is why we do not just talk to Tk) and
              ; then that value gets propagated to Tk via "set <widget path> <value>". Because
              ; the textVariable for every entry is the entry itself, the text of the entry
              ; then changes. If that sounds weird, what we are actually doing is tapping into
              ; the fact that Tk to a large degree takes the same approach as Cells does with value:
              ; in Cells, we think of model instances as wrapping some model-specific 
              ; value, which is held in the value slot of the model instance. Tk simply
              ; allows a widget path to be a global variable. Furthermore, as the company name
              ; ActiveState suggests, Tk also provides automatic propagation: change the
              ; variable, and anyone with that as its textVariable also changes.
              )))))

(defmodel ltk-test-canvas (canvas)
  ()
  (:default-initargs
      :id :test-canvas
    :background (c? (or (selection (fm! :bkg (^menus)))
                      'SystemButtonFace))
    ;
    ; we are taking the demo a little further to make it a little more real world than just
    ; printing to standard output. A point to make here is the decoupling of the menu from
    ; its application role, namely allowing the user to specify the background color of
    ; the spinning lines. The pop-up is now a radio-group menu that does not know how the
    ; choice it is maintaining will be used. It simply takes care of its business of allowing
    ; the user to choose exactly one color. Changes get propagated automatically by the Cells 
    ; engine to any slot whose rule happens to read the radio-group selection slot. And the coding 
    ; is transparent: just read the value. No need to write explicit code to subscribe, notify,
    ; or unsubscribe.
    ;
    :scroll-region '(0 0 500 400)
    :gridding "-row 0 -column 0 -sticky news"
    ;
    ; As with packing, Celtk tries to simplify life with Tk gridding. But that is achieved partly
    ; by automating things as with the kids-packing and kids-layout slots, and partly by staying
    ; out of the programmer's way and letting them specify actual Tk code to be passed unfiltered
    ; to Tk. The design choice here is to acknowledge that LTk and Celtk users really are still
    ; doing Tk programming; only some automation (and Lispification) is provided.
    ;
    ; This also simplifies Celtk since it just has to pass the Tk code along with "grid <path> "
    ; appended.
    ;    
    :event-handler (c? (lambda (self xe)
                         (case (tk-event-type (xsv type xe))
                           (:virtualevent
                            (trc "canvas virtual" (xsv name xe)))
                           (:buttonpress
                            (trc nil "canvas buttonpress" self (xsv x-root xe)(xsv y-root xe))
                            (pop-up (^widget-menu :bkg-pop) (xsv x-root xe) (xsv y-root xe))))))
    
    :menus (c? (the-kids
                ;
                ; we could just build the menu in the rule above for event-handlers and then close over the variable
                ; bearing the menu's Tk name in the binding callback in the call to pop-up, but I try to decompose
                ; these things in the event that the event-handlers become dynamic over time (esp. such that the rule to generate
                ; the binding list runs repeatedly) so we are not forever regenerating the same pop-up menu.
                ; premature optimization? well, it also makes the code clearer, and should the list of menus become
                ; variable over time this allows us to GC (via Tk "destroy") menus, so this is not so much about
                ; optimization as it is about Good Things happening to well-organized code.
                ;
                (mk-menu
                 :id :bkg-pop
                 :kids (c? (the-kids
                            (mk-menu-radio-group
                             :id :bkg
                             :selection (c-in nil) ;; this will start us off with the Tk default
                             :kids (c? (the-kids
                                        (mk-menu-entry-radiobutton :label "Crimson Tide" :value "red")
                                        (mk-menu-entry-radiobutton :label "Oak Tree Ribbon" :value "yellow")
                                        (mk-menu-entry-radiobutton :label "Sky" :value 'blue)
                                        (mk-menu-entry-radiobutton :label "Factory" :value 'SystemButtonFace)))))))))
    
    :kids (c? (the-kids
               (mk-text-item
                :coords (list 10 10)
                :anchor "nw"
                :text "Ltk Demonstration")
               (make-kid 'moire :id :moire-1)))))
               ;
               ; we give this widget a specific ID so other rules can look it up, as
               ; discussed above when explaining fm^.

(defmodel moire (line)
  ((angle-1 :initarg :angle-1 :accessor angle-1 :initform (c-in 0))
   (point-ct :initarg :point-ct :accessor point-ct
     :initform (c? (num-value (fm^ :point-ct)))))
  (:default-initargs
      :timers (c? (list (make-instance 'timer
                          ;
                          ; it occurred to me that it might be useful to build a timer utility
                          ; around the TCL after command. See the class definition of timer
                          ; for the fireworks (in terms of Cells) that resulted
                          ;
                          :repeat (c-in t)
                          :delay 1 ;; milliseconds since this gets passed unvarnished to TK after
                          :action (lambda (timer)
                                    (declare (ignorable timer))
                                    (trc nil "timer fires!!" timer)
                                    (incf (^angle-1) 0.1)))))
    :coords (c? (let ((angle-2 (* 0.3 (^angle-1)))
                             (wx (sin (* 0.1 (^angle-1)))))
                         (loop for i below (^point-ct)
                             for w = (+ (^angle-1) (* i 2.8001))
                             for x = (+ (* 50 (sin angle-2)) 250 (* 150 (sin w) (1+ wx)))
                             for y = (+ (* 50 (cos angle-2)) 200 (* 150 (cos w)))
                             nconcing (list x y))))))

(defun (setf moire-spin) (repeat self)
  (setf (repeat (car (timers self))) repeat)) ;; just hiding the implementation

(defun ltk-test-menus ()
  ;
  ; The only difference is that the menu structure as seen by the user
  ; is apparent here, which might help some when reorganizing menus.
  ;
  ; Well, another thing which happens not to be visible here... hang on.
  ; OK, I just made the Save menu item contingent upon there being no 
  ; user-errors. As you add/remove all digits (considered invalid for
  ; demonstration purposes) the menu item becomes available/unavailable
  ; appropriately.
  ;
  ; This is the kind of thing that Cells is good for.
  ;
  (mk-menubar
   :kids (c? (the-kids
              (mk-menu-entry-cascade-ex (:label "File")
                (mk-menu-entry-command-ex () "Load" (format t "~&Load pressed"))
                (mk-menu-entry-command-ex (:state (c? (if (user-errors (fm^ :point-ct))
                                                          :disabled :normal)))
                  "Save" (format t "~&Save pressed"))
                (mk-menu-entry-separator)
                (mk-menu-entry-cascade-ex (:id :export :label "Export...")
                  (mk-menu-entry-command-ex () "jpeg" (format t "~&Jpeg pressed"))
                  (mk-menu-entry-command-ex () "png" (format t "~&Png pressed")))
                (mk-menu-entry-separator)
                (mk-menu-entry-command :label "Quit"
                  :accelerator "Alt-q"
                  ;
                  ; check out the observer on the accelerator slot of the class menu-entry-usable
                  ; to see how Celtk fills in a gap in Tk: accelerators should work just by
                  ; declaring them to the menu widget, it seems to me. In Celtk, they do.
                  ;
                  :underline 1
                  :command "destroy .; break"))))))


(defmodel entry-numeric (entry)
  ((num-parse :initarg :num-parse :accessor num-parse
     :initform (c? (eko ("numparse")
                     (handler-case
                         (parse-integer (^value))
                       (parse-error (c)
                         (princ-to-string c))))))
   (num-value :initarg :num-value :accessor num-value
     :initform (c? (if (numberp (^num-parse))
                       (^num-parse)
                     (or .cache 42)))))
  (:default-initargs
      :value "42"
    :user-errors (c? (unless (numberp (^num-parse))
                       (^num-parse)))))
  
 
(defun mk-entry-numeric (&rest iargs)
  (apply 'make-instance 'entry-numeric :fm-parent *parent* iargs))
  