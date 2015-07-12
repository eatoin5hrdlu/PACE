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

:- dynamic target_value/2, current_value/4, current_value/2.

% Bug in XPCE, but this button class will resize properly
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
% After device discovery process, the evostat specification
% will be written to evostat.config with verified BT addresses.
% Shutting down should preserve verified bluetooth addresses
% as well as target values for temperature, turbidity, and flow.
%


configure(aristotle, [
  cellstat(c5, discover, temp(37.9), od(0.4)),
  lagoon(l1, '98:D3:31:70:2B:70', temp(36.0), lux(0.1), flow(3.5)),
  lagoon(l2, discover,            temp(36.0), lux(0.1), flow(3.5)),
  lagoon(l3, discover,            temp(36.0), lux(0.1), flow(3.5)),
  lagoon(l4, discover,            temp(36.0), lux(0.1), flow(3.5)),
  pump(  p6, discover ),
  sampler(s7,  discover)]).
	 


% screen(rpi,160,40,point(50,50)).  % tiny

%
% Color code for labeling text (name or parameter values):
%   Red before connections are established
%   Blue when parameter values are low
%   Orange when parameters are high
%   Green when paramaters are in the target range

% Create a dialog for this EvoStat

screen(aristotle, 520, 760, point(50,50),
       [[cellstat(c5, text(aristotle), size(200,12))],
	[button(tc, value(c5,temperature,37.0)),
	 button(tb, value(c5, turbidity,0.4 ))  ],

	[image(acam, 'opencvlevel.jpg')],
      
       
      [button(l1,text(l1)),
       button(l2,text(l2)),
       button(l3,text(l3)),
       button(l4,text(l4))],

      [spacer(blue,8)],
      
      [button(t1, value(l1,temperature,37.0)),
       button(t2, value(l2,temperature,37.0)),
       button(t3, value(l3,temperature,37.0)),
       button(t4, value(l4,temperature,37.0)) ],
      [spacer(green,4)],
      
      [button(lux1, value(l1,lux,0.1)),
       button(lux2, value(l2,lux,0.2)),
       button(lux3, value(l3,lux,0.3)),
       button(lux4, value(l4,lux,0.4)) ],
      [spacer(yellow,8)],

      [button(flow1, value(l1,flow,3.5)),
       button(flow2, value(l2,flow,3.5)),
       button(flow3, value(l3,flow,3.5)),
       button(flow4, value(l4,flow,3.5)) ]
]).

screen(darwin, 460, 600, point(700,50),
      [[cellstat(c5, text(darwin), size(200,12))],
      
      [button(tc, value(c5,temperature,31.0)),
       button(tb, value(c5, turbidity,0.3 ))  ],
      [spacer(blue,8)],
       
      [button(l1,text(l1)),
       button(l2,text(l2))],
      
      [button(t1, value(l1,temperature,31.0)),
       button(t2, value(l2,temperature,31.0)) ],
      [spacer(blue,8)],
      
      [button(lux1, value(l1,lux,0.3)),
       button(lux2, value(l2,lux,0.3)) ],

      [button(flow1, value(l1,flow,2.5)),
       button(flow2, value(l2,flow,2.5)) ]
]).

resize(Thing) :-
    screen(_,_,Height,_,_),
    BH is Height/16,
    BW is 2*BH,
    send(Thing, size(size(BW,BH))).

create_label(value(Device,Parameter,Target),Name) :-
    assert(target_value(Name, Target)),
    assert(current_value(Name, Device, Parameter, 0)).

create_label(text(Label),Name) :-
    assert(current_value(Name, evostat, label, Label)).


row_item(button(Name,What),@Name) :-
    free(@Name),
    new(@Name,ebutton(Name)),
    send(@Name, accelerator, @nil),
    resize(@Name),
    send(@Name, colour(red)),


    new(M, move_gesture),
    new(P, popup_gesture(new(Pop, popup))),
    send_list(Pop, append,
              [ menu_item(reconnect, message(@Name,colour,colour(black)))
                , menu_item(disconnect,message(@Name,colour,colour(white)))
                , menu_item(dark, message(@Name,colour,colour(black)))
                , menu_item(light,message(@Name,colour,colour(white))) 
	      ]),
    new(G, handler_group(M, P)),
    send(@Name, recogniser, G),
    create_label(What, Name).

%    new(H1, message(File, displayed, @on)),

%    new(HIn, message(@Name, colour, colour(brown))),
%    new(Hout, message(@Name, colour, colour(red))),
%    send(@Name, recogniser,handler(area_enter, HIn)),
%    send(@Name, recogniser,handler(area_exit, Hout)),


row_item(cellstat(Name, text(Evo), size(Width,Height)),@Name) :-
    free(@Name),
    new(@Name,button(Name)),
    send(@Name, colour(blue)),
    send(@Name, size, size(Width, Height)),
    create_label(text(Evo),Name).

row_item(spacer(Color,Height), @Spacer) :-
    atom(Color),
    screen(_,Width,_H,_Location,_Mitems),
    NWid is Width - 30,
    new(@Spacer, box(NWid,Height)),
    send(@Spacer, colour, Color),
    send(@Spacer, fill_pattern, colour(Color)).

%row_item(label(Name,_What),@Name) :-
%    free(@Name),
%    new(@Name,button(Name)),
%    send(@Name, colour(red)).

row_item(image(Name,File),@Name) :-
    free(@Name),
    new(@Name, label(Name)),
    new(I, image(File)),
%    screen(_,Width,_,_,_),
%    NewWidth is Width - Width/10,
%    NewHeight is integer(Width*0.675),
%    get(I, size, Size),
%    send(Size, width(NewWidth)),
%    send(Size, height(NewHeight)),
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
					      message(W, save_buffer)),
				    menu_item(quit,
					      message(W, quit))
				  ]),
		about_atom(About),
		send_list(Help, append,
				  [ menu_item(about,
					      message(@display, inform, About))
				  ]),
% Rest of GUI
		
	  initialize_rows(MItems, W),

	  new(Msg1, message(W, update)),
	  free(@ut),
	  send(W, attribute, attribute(timer, new(@ut, timer(3.0, Msg1)))),
	  send(@ut, start),
          send_super(W, open, Location).

c5(_W) :-> "User pressed the CellStat button":: writeln(cellstat).

l1(_W) :-> "User pressed the L1 button"::
  current_prolog_flag(argv,[_,X|_]),
  send(@l1, label, X).

l2(_W) :-> "User pressed the L1 button":: writeln(lagoon).
l3(_W) :-> "User pressed the L1 button":: writeln(lagoon).
l4(W)  :-> "User pressed the L1 button":: writeln(lagoon),send(W,return(ok)).

tb(W)   :-> newvalue(tb,W).
tc(W)   :-> newvalue(tc,W).

t1(W)   :-> newvalue(t1,W).
t2(W)   :-> newvalue(t2,W).
t3(W)   :-> newvalue(t3,W).
t4(W)   :-> newvalue(t4,W).

lux1(W) :-> newvalue(lux1,W).
lux2(W) :-> newvalue(lux2,W).
lux3(W) :-> newvalue(lux3,W).
lux4(W) :-> newvalue(lux4,W).

flow1(W) :-> newvalue(flow1,W).
flow2(W) :-> newvalue(flow2,W).
flow3(W) :-> newvalue(flow3,W).
flow4(W) :-> newvalue(flow4,W).


newvalue(Name,Parent) :-
        get(getValue('New Target Value'), prompt, String),
	catch(atom_number(String,Value),error(type_error(_,_),_),fail),
        retract(target_value(Name,_)),
        assert(target_value(Name, Value)),
        send(Parent,update).


quit(W) :->
        "User pressed the Quit button"::
	send(@ut, stop),     % Shut down the label update timer
	retractall(current_value(_,_,_,_)),
	retractall(current_value(_,_)),
	retractall(target_value(_,_)),
        send(W, return(quit)).

ok(W) :->
        "User pressed the Ok button"::
        send(W, return(ok)).

prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W, confirm, Value).

% Green when within 5% of Target magnitude
range_color(Target, Current, Color) :-
    Delta is Target/20,
    Max is Target + Delta,
    Min is Target - Delta,
    (  Current > Max -> Color = red
     ; Current < Min -> Color = blue
     ;                  Color = green
    ).

bt_update(W) :->
    get(W, graphicals, Chain),
    chain_list(Chain, CList),
    member(Cell, CList),
    Cell =.. [@,Parameter],
    current_value(Parameter, Device, Label, Previous),

    % Get the current value from the Device (or not)
    ( connection(Device, Socket),
      send_bluetooth(Socket, [Parameter], Reply),
      atom_number(Reply, Current),
      retract(current_value(Parameter,Device,Label,Previous)),
      assert( current_value(Parameter,Device,Label,Current))
     ; Previous = Current
     ),

    % Display     
    ( target_value(Parameter, Target) ->
	  range_color(Target, Current, Color),
	  send(Cell, colour(Color)),
	  concat_atom([Parameter,'\n', Target,' / ',Current], Label)
     ; Label = Current
    ),
    send(Cell, label, Label),
    fail.


update(W) :->
    get(W, graphicals, Chain),
    chain_list(Chain, CList),
    member(Cell, CList),
    Cell =.. [@,Name],
    current_value(Name, _Device, Parameter, Current),
    ( target_value(Name, Target) ->
	  range_color(Target, Current, Color),
	  send(Cell, colour(Color)),
	  concat_atom([Parameter,'\n', Target,' / ',Current], Label)
     ; Label = Current
    ),
    send(Cell, label, Label),
    fail.
    

:- pce_end_class.


b :-
    ( current_prolog_flag(argv,[_,Name|_]) ; Name = darwin ),
    b(Name).

b(Name) :-
  free(@gui),
  new(@gui, evostat(Name)),
  send(@gui?frame, icon, bitmap('./open/images/evo.xpm')),
  get(@gui, prompt, Reply),
  (Reply = quit -> send(@gui, destroy); true ),
  halt.

%  free(@hui),
%  new(@hui, evostat(darwin)),
%  send(@hui?frame, icon, bitmap('./open/images/evo.xpm')),
%  get(@hui, prompt, N2),
%  (N2 == quit -> halt; true).

righton :-
    retract(current_value(t2,_,_,_)),
    assert(current_value(t2,l2,temperature,36)).

raise :-
    current_value(t2,_,_,T),
    retract(current_value(t2,_,_,T)),
    NewT is T + 2,
    assert(current_value(t2,l2,temperature,NewT)).

lower :-
    current_value(t2,_,_,T),
    retract(current_value(t2,_,_,T)),
    NewT is T - 2,
    assert(current_value(t2,l2,temperature,NewT)).


:- pce_begin_class(getValue, dialog, "Change a Value").

initialise(W, Label:[name]) :->
        "Initialise the window and fill it"::
        send_super(W, initialise(Label)),
        send(W, append(text_item(name))),
        send(W, append(button(ok))),
        send(W, append(button(cancel))),
        send(W, default_button(ok)).
        
ok(W) :->
        "User pressed the OK button"::
        get(W, member(name), NameItem),
        get(NameItem, selection, Typed),
        send(W, return, Typed).

cancel(W) :->
        "User pressed the Cancel button"::
        send(W, return(@nil)).

prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W, confirm, Value),
        free(W).

:- pce_end_class.

