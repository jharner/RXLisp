#-:classes (load "classes")

; On an IBM PC,  ANSI escape sequences probably won't work unless you use 
; NNANSI.SYS because the buffered output used bypasses the BIOS.

; This is a sample XLISP program
; It implements a simple form of programmable turtle for VT100 compatible
; terminals.

; To run it:

;	A>xlisp turtles

; This should cause the screen to be cleared and two turtles to appear.
; They should each execute their simple programs and then the prompt
; should return.  Look at the code to see how all of this works.

; Get some more memory
(expand 1)

; delay a while
#+:times (defun pause (time) 
	   (let ((fintime (+ (* time internal-time-units-per-second)
			     (get-internal-run-time))))
		(loop (when (> (get-internal-run-time) fintime)
			    (return-from pause)))))
#-:times (defun pause () (dotimes (x (* time 1000))))

(defmacro delay () (pause 0.5))


; Clear the screen
(defun clear ()
    (princ "\033[H\033[2J"))

; Move the cursor
(defun setpos (x y)
    (princ "\033[") (princ y) (princ ";") (princ x) (princ "H"))

; Kill the remainder of the line
(defun kill ()
    (princ "\033[K"))

; Move the cursor to the currently set bottom position and clear the line
;  under it
(defun bottom ()
    (setpos *bx* (+ *by* 1))
    (kill)
    (setpos *bx* *by*)
    (kill))

; Clear the screen and go to the bottom
(defun cb ()
    (clear)
    (bottom))


; ::::::::::::
; :: Turtle ::
; ::::::::::::

; Define "Turtle" class
(defclass Turtle ((xpos (setq *newx* (+ *newx* 1))) (ypos 12) (char "*")))

; Message ":display" prints its char at its current position
(defmethod Turtle :display () 
    (setpos xpos ypos)
    (princ char)
    (bottom)
    self)

; When the character is set, we want to redisplay
(defmethod Turtle :set-char (c)
	(setq char c)
	(send self :display))

; Message ":char" sets char to its arg and displays it
(defmethod Turtle :set-char (c) 
    (setq char c)
    (send self :display))

; Message ":goto" goes to a new place after clearing old one
(defmethod Turtle :goto (x y)
    (setpos xpos ypos) (princ " ")
    (setq xpos x)
    (setq ypos y)
    (send self :display))

; Message ":up" moves up if not at top
(defmethod Turtle :up ()
    (if (> ypos 0)
	(send self :goto xpos (- ypos 1))
	(bottom)))

; Message ":down" moves down if not at bottom
(defmethod Turtle :down ()
    (if (< ypos *by*)
	(send self :goto xpos (+ ypos 1))
	(bottom)))

; Message ":right" moves right if not at right
(defmethod Turtle :right ()
    (if (< xpos 80)
	(send self :goto (+ xpos 1) ypos)
	(bottom)))

; Message ":left" moves left if not at left
(defmethod Turtle :left ()
    (if (> xpos 0)
	(send self :goto (- xpos 1) ypos)
	(bottom)))

; :::::::::::::::::::
; :: Circular-List ::
; :::::::::::::::::::


; Define a class to represent a circular list
(defclass Circular-List (prog pc))

; Replace :isnew with something more appropriate
(defmethod Circular-List :isnew (&optional list)
	(setf prog list pc list)
	self)	; return self

; Method to get next item in list
(defmethod Circular-List :next ()
	(when (null pc) (setq pc prog))
	(prog1 (car pc) (setq pc (cdr pc))))


; :::::::::::::
; :: PTurtle ::
; :::::::::::::

; Define "PTurtle" programable turtle class
(defclass PTurtle (prog) () Turtle)

; Message ":program" stores a program
(defmethod PTurtle :program (p)
    (setf prog (send Circular-List :new p))
    self)

; Message ":step" executes a single program step
(defmethod PTurtle :step () 
    (when prog (send self (send prog :next)))
    (delay)
    self)

; Message ":step#" steps each turtle program n times
(defmethod PTurtle :step# (n)
    (dotimes (x n) (send self :step))
    self)


; ::::::::::::::
; :: PTurtles ::
; ::::::::::::::

; Define "PTurtles" class
(defclass PTurtles (Turtles))

; Message ":make" makes a programable turtle and adds it to the collection
(defmethod PTurtles :make (x y &aux newturtle)
    (setq newturtle (send PTurtle :new :xpos x :ypos y))
    (setq Turtles (cons newturtle Turtles))
    newturtle)

; Message ":step" steps each turtle program once
(defmethod PTurtles :step ()
    (mapcar #'(lambda (Turtle) (send Turtle :step)) Turtles)
    self)

; Message ":step#" steps each turtle program n times
(defmethod PTurtles :step# (n)
    (dotimes (x n) (send self :step))
    self)


; Initialize things and start up
(defvar *bx* 0)
(defvar *by* 20)
(defvar *newx* 0)

; Create some programmable turtles
(cb)
(definst PTurtles Turtles)
(setq t1 (send Turtles :make 40 10))
(setq t2 (send Turtles :make 41 10))
(send t1 :program '(:left :left :right :right :up :up :down :down))
(send t2 :program '(:right :right :down :down :left :left :up :up))
(send t1 :set-char "+")
(defun doit () 
	(progn
		(cb)
		(send t1 :step# 8)
		(send t2 :step# 8)
		(send Turtles :step# 8)))
(doit)

