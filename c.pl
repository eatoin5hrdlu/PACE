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
	
	
:- [gbutton].

:- dynamic target_value/2, current_value/4, current_value/2.
:- dynamic component/2.

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
% Color code for labeling text (name or parameter values):
%   Red before connections are established
%   Blue when parameter values are low
%   Orange when parameters are high
%   Green when paramaters are in the target range

% Create a dialog for this EvoStat

screen(aristotle, 680, 800, point(50,50)).
screen(darwin, 680, 900, point(50,50)).

resize(Thing) :-
    screen(_,_,Height,_),
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
    screen(_,Width,_H,_Location),
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
%    screen(_,Width,_,_),
%    NewWidth is Width - Width/10,
%    NewHeight is integer(Width*0.675),
%    get(I, size, Size),
%    send(Size, width(NewWidth)),
%    send(Size, height(NewHeight)),
    send(@Name, selection, I).

freeall :-
    catch( get(@gui, graphicals, Chain),
	   ( chain_list(Chain, CList), freeall(CList) ),
	    writeln(firsttime)).
	   
freeall([]).
freeall([H|T]) :- writeln(free(H)), free(H), freeall(T).


:- pce_begin_class(evostat, dialog, "PATHE Control Panel").

initialise(W, Label:[name]) :->
          "Initialise the window and fill it"::
          send_super(W, initialise(Label)),
          screen(Label, WinWidth, WinHeight, Location),
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
	 call(Label, Components),
         findall(_,(component(_,Obj),free(Obj)),_), % Clear out previous
	 maplist(create(@gui), Components),
	 new(Msg1, message(W, update10)),
	 free(@ut),
	 send(W, attribute, attribute(timer, new(@ut, timer(4.0, Msg1)))),
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
%	send(@ut, stop),     % Shut down the label update timer
	retractall(current_value(_,_,_,_)),
	retractall(current_value(_,_)),
	retractall(target_value(_,_)),
        send(W, return(quit)).

load(W, File:[file]) :->
        "User pressed the Load button"::
%	send(@ut, stop),     % Shut down the label update timer
	retractall(current_value(_,_,_,_)),
	retractall(current_value(_,_)),
	retractall(target_value(_,_)),
        send(W, return(File)).

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

update10(W) :->
    get(W, graphicals, Chain),
    chain_list(Chain, CList),
    member(Object, CList),
    component(_,Object),        % If one has been created
    send(Object, update),
    fail.
    
:- pce_end_class.

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

aristotle([
  cellstat(c5, right,
   [ btaddr('98:D3:31:70:2B:70'), temp(37.9), od(0.4), shape(200,80)]),
  snapshot(c9, right, [ shape(640,480) ]),
  lagoon(l1,  next_row,
   [ btaddr('98:D3:31:70:2B:70'), temp(37.9), od(0.4), shape(60,30)]),
  lagoon(l2,  right,
   [ btaddr('98:D3:31:70:2B:70'), temp(37.9), od(0.4), shape(60,30)]),
  pumps(p6, right,
   [ btaddr('98:D3:31:40:1D:A4') ])
]).

buffon([
  ebutton(c5, right,
   [ btaddr('98:D3:31:70:2B:70'), shape(40,20)]),
  snapshot(c9, right,   [ shape(640,480) ]),
  ebutton(l1,  right,
   [ btaddr('98:D3:31:70:2B:70'), shape(200,12)]),
  ebutton(l2,  right,
   [ btaddr('98:D3:31:70:2B:70'), shape(200,12)]),
  ebutton(p6, next_row,
   [ btaddr('98:D3:31:40:1D:A4') ])
]).

darwin([
 cellstat(cellstat,    below, [ shape(240,110),font(font(times,roman,22))]),
 pumps(      pumps, next_row, [ btaddr('98:D3:31:40:1D:A4'), shape(300,50),LF]),
 spacer(        x1, next_row, [color(blue)]),
 snapshot(     cam, next_row, [ shape(640,480),image('opencvlevel.jpg')]),
 spacer(        x2, next_row, []),
 lagoon(   lagoon1, next_row, [ btaddr('98:D3:31:70:2B:70'),
                             temp(37.9), od(0.4), LS, LF]),
 lagoon(   lagoon2, right, [ temp(37.9), od(0.4), LS, LF]),
 lagoon(   lagoon3, right, [ temp(37.9), od(0.4), LS, LF]),
 lagoon(   lagoon4, right, [ temp(37.9), od(0.4), LS, LF]),
 spacer(        x3, next_row, [color(green)]),
 sampler(autosampler, next_row, [ shape(400,40),font(font(times,roman,20)) ])
]) :-
 LS = shape(135,90),
 LF = font(font(times,roman,18)).

% Initializers are extra arguments to the constructor
% Data is a list of messages to continue initializing the object

create(Dialog, Component) :-
	Component =.. [Type, Name, Position, Data],
	free(@Name),
	Class =.. [Type,Name],
	new(@Name, Class),
	maplist(send(@Name), Data), % Process all before appending
	send(Dialog, append(@Name, Position)),
        assert(component(Name,@Name)).

about_atom(About) :-
        open('evostat.about', read, Handle),
	read_pending_input(Handle,FileContent,[]),
	atom_chars(About,FileContent).

c :- current_prolog_flag(argv,[_,Name|_]) -> c(Name) ; c(darwin).

c(Name) :-
    free(@gui),
    new(@gui, evostat(Name)),
    send(@gui?frame, icon, bitmap('./open/images/evo.xpm')),
    get(@gui, prompt, Reply),
    (Reply = quit -> send(@gui, destroy); true ),
    halt.

