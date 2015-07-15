:- use_module(library(time)).
:- use_module(library(pce)).
:- use_module(library(process)).
:- use_module(library(charsio)).
:- use_module(library(helpidx)).
:- use_module(library(lists)).
:- use_module(library(ctypes)).

:- use_module(library(time)).
:- use_module(library(process)).
	
:- pce_begin_class(evostat, dialog, "PATHE Control Panel").

initialise(Self,Label:[name]) :->
	"Initialise the window and fill it"::
        send_super(Self,initialise(Label)),
	send(Self, size,size(700,400)),
        ( member(N,[1,2,3,4,5]),
          concat_atom(['pump',N],PName),
          free(@PName),
	  new(@PName, pump(PName)),
          (N=1 -> Align=next_row ; Align=right ),
	  send(Self, append(@PName,Align)),
	  writeln(Align),
          fail
        ; true
        ),
	send_super(Self,open).


ok(Self) :->
        send(Self,return(ok)).

cancel(Self) :->
        send(Self,return(notok)),
        halt.

prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W,confirm,Value).

:- pce_end_class.


:- pce_begin_class(pump, dialog_group).
variable(pumpStatus, name, get, "Pump/Valve status").
variable(pumpButton, any, get, "Pump Button").

initialise(Self, Label:[name]) :->
        "Initialise the Pump"::
        send_super(Self, initialise(Label)),
	send(Self, size,size(100,60)),
	send(Self, slot, pumpStatus,'Off / Closed'),

	concat_atom([Label,'b'],Button),
        writeln(Button),
        free(@Button),
	new(@Button,button(Button)),
        send(@Button,label,'Off/Closed'),
        send(@Button,colour,colour(red)),
	send(Self, slot, pumpButton, @Button),
        send(Self, append(@Button)),

	concat_atom([Label,'p'],PStatus),
        new(@PStatus, hotbox(blue)),
	send(Self, append(@PStatus,next_row)),

	concat_atom([Label,'v'],VStatus),
        new(@VStatus, hotbox(red)),
	send(Self, append(@VStatus,right)).

update(Self) :->
        get(Self,slot, pumpStatus, Status),
        get(Self,slot, pumpButton, Button),
	send(Button, label, Status).

shape(Self, W:[int], H:[int]) :->
	send(Self, size, size(W,H)).

font(_Self, _Font:[any]) :-> true.
:- pce_end_class.


:- pce_begin_class(hotbox, box).
initialise(Self, Color:[name]) :->
        "Initialise the lagoon"::
        send_super(Self, initialise(30,10)),
        send(Self,colour,colour(Color)),
        send(Self, fill_pattern, colour(Color)).
:- pce_end_class.


show :-
    free(@gui),
    new(@gui, evostat(evo)),
    send(@gui?frame, icon, bitmap('./open/images/evo.xpm')),
    get(@gui, prompt, Reply),
    (Reply = quit -> halt; true ).

