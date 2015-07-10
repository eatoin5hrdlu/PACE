#!/usr/bin/xpce
:- use_module(library(time)).
:- use_module(library(pce)).
:- use_module(library(process)).
:- use_module(library(charsio)).
:- use_module(library(helpidx)).
:- use_module(library(lists)).
:- use_module(library(ctypes)).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).
	
	
:- [my_pce_utils].

:- pce_begin_class(ebutton, button).
compute(_) :-> true.
:- pce_end_class.

%
% System Configuration
% Dialog Layout
% Communications Information
%
% pathe.config
%
%
% After device discovery process, this evostat/1 term (with addresses)
% will be written to discover.config
%

components(aristotle, [cellstat(ac5, discover),
		       lagoon(  al1, '98:D3:31:70:2B:70'),
		       lagoon(  al2, discover),
		       lagoon(  al3, discover),
		       lagoon(  al4, discover),
		       pump(    ap6, discover),
		       sampler(as7,  discover)]).
	 


% screen(rpi,160,40,point(50,50)).  % tiny

%
% Color code for labeling text (name or parameter values):
%   Red before connections are established
%   Blue when parameter values are low
%   Orange when parameters are high
%   Green when paramaters are in the target range

% Create a dialog for this EvoStat

screen(aristotle, 520, 740, point(50,50),
       [[cellstat(ac5, text(aristotle), size(200,12))],
	[],
	[image(acam, 'opencvlevel.jpg')],
	[],
      
      [button(atc, value(c5,temperature)),
       button(atb, value(c5, turbidity ))  ],
       
      [button(al1,name(l1)),
       button(al2,name(l2)),
       button(al3,name(l3)),
       button(al4,name(l4))],
      
      [button(at1, value(l1,temperature)),
       button(at2, value(l2,temperature)),
       button(at3, value(l3,temperature)),
       button(at4, value(l4,temperature)) ],
      [],
      
      [button(alux1, value(l1,lux)),
       button(alux2, value(l2,lux)),
       button(alux3, value(l3,lux)),
       button(alux4, value(l4,lux)) ],
      [],

      [button(aflow1, value(l1,flow)),
       button(aflow2, value(l2,flow)),
       button(aflow3, value(l3,flow)),
       button(aflow4, value(l4,flow)) ]
]).

screen(darwin, 460, 600, point(700,50),
      [[cellstat(dc5, text(darwin), size(200,12))],
      
      [button(dtc, value(c5,temperature)),
       button(dtb, value(c5, turbidity ))  ],
      [],
       
      [button(dl1,name(l1)),
       button(dl2,name(l2))],
      
      [button(dt1, value(l1,temperature)),
       button(dt2, value(l2,temperature)) ],
      [],
      
      [button(dlux1, value(l1,lux)),
       button(dlux2, value(l2,lux)) ],

      [button(dflow1, value(l1,flow)),
       button(dflow2, value(l2,flow)) ]
]).

resize(Thing) :-
    screen(_,_,Height,_,_),
    BH is Height/16,
    BW is 2*BH,
    send(Thing, size(size(BW,BH))).

row_item(button(Name,_What),@Name) :-
    free(@Name),
    new(@Name,ebutton(Name)),
    send(@Name, accelerator, @nil),
    resize(@Name),
    send(@Name, colour(red)).

row_item(cellstat(Object, text(Evo), size(Width,Height)),@Name) :-
    atom_codes(Object,OCs),
    atom_codes(Evo,[H|_]),
    atom_codes(Name,[H|OCs]), % Unique name
    free(@Name),
    new(@Name,button(Name)),
    send(@Name, colour(red)),
    send(@Name, size, size(Width, Height)).

%row_item(label(Name,_What),@Name) :-
%    free(@Name),
%    new(@Name,button(Name)),
%    send(@Name, colour(red)).

row_item(image(Name,File),@Name) :-
    free(@Name),
    new(@Name, label(Name)),
    new(I, image(File)),
    screen(_,Width,_,_,_),
    NewWidth is Width - Width/10,
    NewHeight is integer(Width*0.675),
    get(I, size, Size),
    send(Size, width(NewWidth)),
    send(Size, height(NewHeight)),
    send(@Name, selection, I).


:- pce_begin_class(evostat, dialog, "PATHE Control Panel").

initialise(W, Label:[name]) :->
          "Initialise the window and fill it"::
          send_super(W, initialise(Label)),
          screen(Label, WinWidth, WinHeight, Location, MItems),
          send(W, size, size(WinWidth, WinHeight)),

% MENU BAR
	  send(W, append, new(MB, menu_bar)),
	  send(MB, append, new(File, popup(file))),
	  send(MB, append, new(Help, popup(help))),
	
		send_list(File, append,
				  [ menu_item(load,
					      message(W, load, @finder?file)),
				    menu_item(save,
					      message(W, save_buffer),
					      condition := W?modified == @on,
					      end_group := @on),
				    menu_item(quit,
					      message(W, destroy))
				  ]),
		about_atom(About),
		send_list(Help, append,
				  [ menu_item(about,
					      message(@display, inform, About))
				  ]),
% Rest of GUI
		
	  initialize_rows(MItems, W),
          send_super(W, open, Location).

c2(W) :->
        "User pressed the L1 button"::
        writeln(ok),
        send(W, return(ok)).

l1(W) :->
        "User pressed the L1 button"::
        writeln(ok),
        send(W, return(ok)).

quit(W) :->
        "User pressed the Quit button"::
        send(W, return(quit)).

ok(W) :->
        "User pressed the Ok button"::
        send(W, return(ok)).

prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W, confirm, Value).

:- pce_end_class.


b :-
  free(@gui),
  new(@gui, evostat(aristotle)),
  send(@gui?frame, icon, bitmap('./open/images/evo.xpm')),
  free(@hui),
  new(@hui, evostat(darwin)),
  send(@hui?frame, icon, bitmap('./open/images/evo.xpm')),
  get(@gui, prompt, Name),
  (Name == quit -> halt; true),
  get(@hui, prompt, N2),
  (N2 == quit -> halt; true).



