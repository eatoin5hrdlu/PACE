#!/usr/bin/xpce
:- use_module(library(time)).
:- use_module(library(pce)).
:- use_module(library(process)).
:- use_module(library(charsio)).
:- use_module(library(helpidx)).
:- use_module(library(lists)).
:- use_module(library(ctypes)).

:- dynamic turbidostat/6. % t(Name, ID, Port, PID, In, Out)
:- dynamic turbidostats/1.
:- dynamic identified/2.
:- dynamic tt_reading/4.
:- dynamic measurement/3. % Name, {temperature,turbidity}, Value
:- multifile measurement/3.
%:- use_module(library('R')).
:- use_module(library(apply)).

%assert_list([]).
%assert_list([H|T]) :- assert(H), assert_list(T).


%---------------------------------------------------------------
% BEGIN SITE AND MACHINE SPECIFIC ITEMS
% WHICH MAY BE MODIFIED IF NECESSARY
%---------------------------------------------------------------
%

% :- r_bin('C:/cygwin/R/bin/i386/Rterm.exe').

lcd([ monitor(lcd),
      pacesize(1500, 650),
      paceMenuWidth(320),
      paceLocation(point(10,10)),
      pacefont(small, font(courier, normal, 22)),
      pacefont(medium,font(courier, bold, 48)),
      pacefont(large, font(courier, bold, 68)),
      pacefont(huge,  font(courier, bold, 98)),
      pad_value(0,_,[' 0  '], darkgrey),
      (pad_value(1,0,[' 0  '], darkgrey) :- !),
      pad_value(1,V,['  ',V,'  '],black),
      pad_value(2,V,['  ',V,' '],black),
      pad_value(3,V,[' ',V,' '],black),
      pad_value(4,V,[V],black)]).

hdmi([ monitor(lcd),
      pacesize(1500, 650),
      paceMenuWidth(320),
      paceLocation(point(10,10)),
      pacefont(small, font(courier, normal, 22)),
      pacefont(medium,font(courier, bold, 48)),
      pacefont(large, font(courier, bold, 68)),
      pacefont(huge,  font(courier, bold, 98)),
      pad_value(0,_,[' 0  '], darkgrey),
      (pad_value(1,0,[' 0  '], darkgrey) :- !),
      pad_value(1,V,['  ',V,'  '],black),
      pad_value(2,V,['  ',V,' '],black),
      pad_value(3,V,[' ',V,' '],black),
      pad_value(4,V,[V],black)]).


ubuntu([monitor(dell),
	pacesize(1500, 650),
	paceMenuWidth(320),
	paceLocation(point(10,10)),
	pacefont(small, font(courier, normal, 22)),
	pacefont(medium,font(courier, bold, 48)),
	pacefont(large, font(courier, bold, 68)),
	pacefont(huge,  font(courier, bold, 98)),
	pad_value(0,_,[' 0  '], darkgrey),
	(pad_value(1,0,[' 0  '], darkgrey) :- !),
	pad_value(1,V,['  ',V,'  '],black),
	pad_value(2,V,['  ',V,' '],black),
	pad_value(3,V,[' ',V,' '],black),
	pad_value(4,V,[' ',V],black)]).


toshiba([monitor(toshiba),
	 pacesize(1300,650),
	 paceMenuWidth(300),
	 paceLocation(point(50,50)),
	 pacefont(small, font(courier, normal, 28)),
	 pacefont(medium,font(courier, bold, 42)),
	 pacefont(large, font(courier, bold, 68)),
	 pacefont(huge,  font(courier, bold, 98)),
	 pad_value(0,_,['  0  '],darkgrey_),
	 (pad_value(1,0,['  0  '],darkgrey) :- !),
	 pad_value(1,V,['   ',V,'  '],black),
	 pad_value(2,V,['  ',V,'  '],black),
	 pad_value(3,V,[' ',V,' '],black),
	 pad_value(4,V,[V],black)]).

:- ( current_prolog_flag(windows,true) -> toshiba(A)
   ; current_prolog_flag(arch,armv6l) -> lcd(A) % hdmi(A)?
   ; ubuntu(A)
   ),
   maplist(assert,A).

turbidostats([aristotle, buffon, cuvier, darwin
%            , erasmus,  ford,  gregor, huxley
             ]).


measurement( aristotle, temperature,  10).
measurement( aristotle, turbidity,   290).
measurement( buffon,    temperature,  0).
measurement( buffon,    turbidity,    0).
measurement( cuvier,    temperature,  8).
measurement( cuvier,    turbidity,   50).
measurement( darwin,    temperature, 37).
measurement( darwin,    turbidity,  800).
measurement( erasmus, temperature,  10).
measurement( erasmus, turbidity,   290).
measurement( ford,    temperature,  0).
measurement( ford,    turbidity,    0).
measurement( gregor,    temperature,  8).
measurement( gregor,    turbidity,   50).
measurement( huxley,    temperature, 37).
measurement( huxley,    turbidity,  800).

threshold(temperature, 0,   24,  blue  ).
threshold(temperature, 24,  37,  green ).
threshold(temperature, 37,  100, red   ).
threshold(turbidity,    0,  300, yellow).
threshold(turbidity,  300,  700, orange).
threshold(turbidity,  700, 1024, brown ).

%
%---------------------------------------------------------------
% END SITE AND MACHINE SPECIFIC ITEMS
% WHICH MAY BE MODIFIED IF NECESSARY
%---------------------------------------------------------------
%


% Ask turbidostat for ID (Okay if it isn't 'z')
getID(In, Out, ID) :-
	format(In,'i~n',[]),
        read_line_to_codes(Out, [ID]),
        read_line_to_codes(Out, "end_of_data"),
        ID =\= 0'z,
	!,
	writeln('Good ID (no reset)').

% 'z' is an unconfigured turbostat, so do restore, then get ID
getID(In, Out, ID) :- % Needs restore(reset)
	writeln('Resetting...'),
        format(In,'r~n',[]),
        read_line_to_codes(Out, "end_of_data"),
	format(In,'i~n',[]),
	sleep(1),
        read_line_to_codes(Out, [ID]),
        read_line_to_codes(Out, "end_of_data"),
	!.

%     write( 'Press Return to continue...' ), nl,
%     read_line_to_codes( user_input, _ ),
%     r_print( 'dev.off()' ),
%     r_close.

sturb(Name) :-  send(@tmenu,selection,Name).


evoconnect(Setup) :-
        ( current_prolog_flag(windows,true)
	-> Python = 'C:/cygwin/Python27/python.exe',
            Filter = 'C:/cygwin/home/peter/src/PACE/ipcam.py',
           Cwd = 'C:\\cygwin\\home\\peter\\src\\PACE'
        ;  Python = '/usr/bin/python',
           Filter = '/home/peter/src/PACE/ipcam.py',
           Cwd = '/home/peter/src/PACE'
        ),
        add_to_editor(buffon,"Connecting..."),
        add_to_editor(buffon,"\n"),
        process_create(Python,
		       [ '-u', Filter ],
		       [ stdin(pipe(In)), stdout(pipe(Out)), cwd(Cwd), process(PID)]),
        set_stream(In, buffer(false)),
	format(In, '~a~n', [Setup]), % Tell Python which configuration
        sleep(1),
	read(Out, Reply),
	Reply = online(Setup),
	assert(camera(In,Out,PID)).

id :-
        ( current_prolog_flag(windows,true)
	-> Python = 'C:/cygwin/Python27/python.exe',
            Filter = 'C:/cygwin/home/peter/arduino/examples/01.Basics/myio/pyship.py',
           Cwd = 'C:\\cygwin\\home\\peter\\arduino\\examples\\01.Basics\\myio',
	   DeviceList = [4,5,6,7,8,9,10,11,12,13,14,15,16] % number is N-1 for comN:
        ;  Python = '/usr/bin/python',
           Filter = '/home/peter/src/PACE/pyship.py',
           Cwd = '/home/peter/src/PACE',
	   DeviceList = [0,1,2,3,4] % /dev/ttyUSBN
        ),
        member(Dev, DeviceList),
	number_chars(Dev, DevChs),
	atom_chars(Device, DevChs),
%	format('calling ~w with arg ~q~n', [Filter, Device]),
        add_to_editor(buffon,DevChs),
        add_to_editor(buffon,"\n"),
        process_create(Python,
		       [ '-u', Filter ],
		       [ stdin(pipe(In)), stdout(pipe(Out)), cwd(Cwd), process(PID)]),
        set_stream(In, buffer(false)),
	format(In, '~a~n', [Dev]), % Tell python program which serial device
        sleep(1),  % Wait for Python to open Arduino
	read_line_to_codes(Out, Codes),
	( Codes = [0'c]
          -> true
         ; catch( format(In,'x~n',[]), _, fail ), fail
        ), % 'x' kills process
        sleep(1),     % Arduino takes a while to begin processing input
	getID(In, Out, C),
	( memberchk(C, "abcdefgh") ->
	  retract(turbidostat(Name, C,       _, _,  _,   _)),
	  assert( turbidostat(Name, C,  Device, PID, In, Out)),
          catch( send(@tmenu,selection,Name),
                  _,
		  (writeln('no graphics yet, delaying selection'), true) ),
	  writeln(online(Name))
	;
	  format('Unnamed Turbidostat(~c) on Port: ~a~n',[C, Dev]),
          format(In,'x~n',[]), % Shut down python process
          close(In),
          close(Out)
	),
	fail.

id :- writeln('Turbidostat Identification done').

online(Name)         :-    processID(Name, _).
offline(Name)        :- \+ online(Name).

processID(Name, PID) :-
         turbidostat(Name, _,  _, PID, _, _),
	 nonvar(PID).

shut :-  writeln('               Shutting Turbidostats down'),
         turbidostat(Name, _,  _, PID, In, _),
         processID(Name, PID),
	 format(In, 'x~n', []),
	 flush_output(In),
	 process_wait(PID, Status),
	 writeln(terminated(Name, Status)),
         retract(turbidostat(Name, C,  _D, PID,_,_)),
	 assert(turbidostat(Name, C, _, _, _, _)),
	 fail.
shut :-  writeln('               Turbidostats shutdown').

delayed_startup :-
        auto, % Bring turbidostats online
	new(Msg1, message(@prolog, record_temp_turb)),
        new(@dtimer, timer(2.0, Msg1)),
%	send(W, attribute, attribute(timer, @dtimer)),
	send(@dtimer, start),
	writeln(delayed_initialization_completed).

delayed_execution(Time, Goal) :-
	new(Msg, message(@prolog, (Goal,send(@ex1,stop),free(@ex1)))),
        new(@ex1, timer(Time, Msg)),
	send(@ex1, start),
	writeln(created_delayed_execution(Goal)).


:- pce_begin_class(name_asker, dialog, "Quad-PACE Apparatus Control Panel").

initialise(W, Label:[name]) :->
        "Initialise the window and fill it"::
        send_super(W, initialise(Label)),
        pacesize(WinWidth,WinHeight),
        send(W, size, size(WinWidth, WinHeight)),
	pacefont(small, Small),  new(F3, Small),
	pacefont(medium,Medium), new(F1, Medium),
	pacefont(large, Large),  new(F2, Large),
        pacefont(huge, Huge),    new(F4, Huge),

	new(@plotbut, button(plot)),
        send(@plotbut, alignment, left),
	send(W, append(@plotbut)),
	new(@ok, button(ok)),
	send(W, append(@ok, right)),
        send(W, append(button(auto),right)),
        send(W, append(button(quit),right)),
        send(W, append(button(cold),right)),
        send(W, append(button(warm),right)),
        send(W, append(button(hot),right)),
        send(W, append(button(ds),right)),
        send(W, append(button(good),right)),
        send(W, append(button(browse),right)),
        send(W, append(button(update),right)),
%        send(W, append(button(pump1on),right)),
%        send(W, append(button(pump1off),right)),
        send(W, append(button(rtm),right)),
        send(W, append(button(rtb),right)),
        send(W, append(button(red),right)),
        new(@edbut, button(edit)),
        send(W, append(@edbut,right)),
	get(W, graphicals, Chain),
	chain_list(Chain, CList),
	fix_fonts(CList, F3),

% A,B,C,D
        new(@tmenu, menu(turbidostats, choice)),
	turbidostats(NameList),
        send_list(@tmenu, append, NameList),
        send(@tmenu, alignment, center),
        send(@tmenu, layout, horizontal),
%       send(@tmenu, border, 20),
%       send(@tmenu, area, area(100,100,540,500)),
        send(@tmenu, value_font,  F1),
        send(@tmenu, margin, 12),
        paceMenuWidth(PMWidth),
        send(@tmenu, value_width, PMWidth),
        send(@tmenu, show_label, @off),
        send(@tmenu, label_font, F2),
        send(@tmenu, format, center),
        send(W, append, @tmenu),
	tt_labels(NameList, temperature, F4, [A|As]),
	send(W, append(A, next_row)),
	send_list(W, append, As),
	tt_lagoons(NameList, 4, F3, [L|Ls],[]),
	send(W, append(L, next_row)),
	send_list(W, append, Ls),
	tt_labels(NameList, turbidity, F4, [B|Bs]),
	send(W, append(B,next_row)),
	send_list(W, append, Bs),
	tt_editors(NameList, [E|Es]),
	send(W, append(E,next_row)),
	right_append(Es, W),
        paceLocation(Location),

% Temporary: Set Darwin active when entering 'Plot' button
% to ensure that we read darwin.tdata file every time.
%
        new(Code1, message(@tmenu, selection, darwin)),
        send(@plotbut, recogniser, handler(area_enter, Code1)),

% Fire up editor on this source file 'a.pl', attach to 'edit' button
%
        new(Code2, message(@prolog, emacs, 'a.pl')),
        send(@edbut, recogniser, click_gesture(left,'',single,Code2)),

        send_super(W, open, Location).

ok(W) :->
        "User pressed the OK button"::
        writeln(ok),
        send(W, return(ok)).

%        get(W, member(name), NameItem),
%        get(NameItem, selection, Typed),
%	r_print('dev.off()').

auto(_) :->
        "Connect to Turbidostats and Collector"::
        auto.


quit(W) :->
        "User pressed the Quit button"::
        send(W, return(quit)).

prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W, confirm, Value).

plot(_)   :->
	get(@tmenu, selection, Sel),
	showr(Sel).

matrix(_) :->    plot_matrix.

cold(_)   :-> new_value( darwin, temperature, 20).
warm(_)   :-> new_value( darwin, temperature, 37).
hot(_)    :-> new_value( darwin, temperature, 42).
ds(_)     :-> delayed_startup.
good(_)   :-> (create_files;true),
              add_to_editor(buffon, "wrote data").
browse(_) :-> manpce.

pump1on(_) :-> talkblue('p11\n').
pump1off(_) :-> talkblue('p10\n').

rtm(_) :-> talkblue(['t\n']).
rtb(_) :-> talkblue(['b\n']).
red(_) :-> colorlagoon(aristotle,2,red).


identify :-
    blueteeth(Ss),
    identify(Ss).

identify([]).
identify([S|Ss]) :-
    writeln(idsocket(S)),
    bt_converse(S, 'i\n', Reply),
    writeln(reply(Reply)),
    add_to_editor(buffon, Reply),
    atom_codes(Reply,RChs),
    RChs = [H|_],
    writeln(H),
    append("Lagoon",[H],NameChs),
    atom_codes(Name,NameChs),
    assert(identified(Name,S)),
    identify(Ss).

talkblue(Cmd) :-
    blueteeth([S|_]),
    writeln(socket(S)),
    bt_converse(S, Cmd, Reply),
    writeln(reply(Reply)),
    add_to_editor(buffon, Reply).

talkblue(Who, Cmd) :-
    identified(Who,S),
    writeln(socket(S)),
    bt_converse(S, Cmd, Reply),
    writeln(reply(Reply)),
    add_to_editor(buffon, Reply).


text_fred :- new_value( darwin, turbidity,  800),
              text_from_editor(@aristotle_editor, Text),
              show_slots(Text),
              writeln(got(Text)).

new_value(Name,Type,Value) :-
        retractall(measurement(Name, Type, _)),
	assert(measurement(Name, Type, Value)),
	recolor,
        update_values.

update(_W) :->
        recolor,
        update_values.

% Return pdate display and return latest Temp/Turbidity
% tt(?, -, -)
tt(Device, Temp, Turb) :-
        online(Device),
        send_receive(Device,[tt],[OneLine]),
        atom_codes(OneLine, Codes),
        % Parse Temperature and Turbidity
        parseatom(TempCs, Codes, NextCodes),
        number_codes(Temp, TempCs),
        parseatom(TurbCs, NextCodes, []),
        number_codes(Turb, TurbCs).

parseatom([])    --> [32], !.
parseatom([H|T]) --> [H],  !, parseatom(T).
parseatom([])    --> [].

list_to_stream([H,I|T], Stream) :-
       format(Stream, '~w ', [H]),
       list_to_stream([I|T], Stream).
list_to_stream([H], Stream)     :-
       format(Stream, '~w~n', [H]).

send_receive(Device, Command, Result) :-
        turbidostat(Device, _ID, _Port, _PID, In, Out),
        nonvar(In),
        list_to_stream(Command, In),
	read_line_to_codes(Out, Codes),
        atom_codes(Atom, Codes),
        receive(Atom, Out, Result,Device).

receive('end_of_data', _, [], _) :- !.
receive(Line, Out, [Line|Lines], Name) :-
	read_line_to_codes(Out, Codes),
        add_to_editor(Name,Codes),
        atom_codes(Atom, Codes),
        receive(Atom, Out, Lines, Name).

retrieveRdata(Name) :-
        turbidostat(Name, _ID, _Port, _PID, In, Out),
        nonvar(In),
	format(In, 'd~n', []),
	read_turbidostat_data(Name, Out).

read_turbidostat_data(Name, S) :-
	read_line_to_codes(S, Codes),
        next_turbidostat_data(Codes, Name, S).

next_turbidostat_data("DATA", Name, S) :-
	!,
        turbidostat(Name, _, _, _,_,S),
	concat_atom([Name,'.rdata'],Filename),
        writeln('looking for data...'),
	catch( open(Filename,write,File), Except, (writeln(Except),fail)),
        writeln('found file with Rdata.'),
	read_line_to_codes(S,Codes),
	rdataToFile(Codes, S, File).

next_turbidostat_data(     _, Name, S) :-
	read_turbidostat_data(Name, S).

rdataToFile("end_of_data", _S, File) :-
	close(File).
rdataToFile(         Data, S, File) :-
	format(File, '~s~n', [Data]),
	read_line_to_codes(S, Codes),
	rdataToFile(Codes, S, File).

right_append([], _).
right_append([E|Es], W) :-
        send(W, append, E, right),
	right_append(Es, W).

tt_labels([],          _,    _, [] ).
tt_labels([Name|T], Type, Font, [@Temp|As]) :-
        concat_atom([Name,'_',Type],Temp),
        free(@Temp),
        new(@Temp,  button(Temp)),
        send(@Temp, label_font, Font),
        send(@Temp, shadow, 12),  % Not on Windows
        send(@Temp, radius, 8),   % No effect on Windows
%        send(@Temp, border, 10),
        send(@Temp, show_focus_border, @off),
	send(@Temp, colour(darkgrey)),
        send(@Temp, label, '     '),
        get(@Temp, area, Area),
	send(Area, set( width := 300) ),
	tt_labels(T, Type, Font, As).

% [@Temp|As]) :-
tt_lagoons([],          _,    _) --> [].
tt_lagoons([Name|T], Num, Font ) --> 
        spacer(Name),
	n_lagoons(0,Num,Name,Font),
        spacer(Name),
        tt_lagoons(T,Num,Font).

spacer(Name) --> [@Temp],
   {    concat_atom([Name,'_spacer'],Temp),
        free(@Temp),
        new(@Temp,  button(Temp)),
        send(@Temp, shadow, 6),  % Not on Windows
        send(@Temp, radius, 4),   % No effect on Windows
        send(@Temp, label, ' '),
        get(@Temp, area, Area),
	send(Area, set( width := 4) ) }.

n_lagoons(Num, Num,     _,    _) --> !, [].
n_lagoons(Num, Last, Name, Font) --> [@Temp],
   {    
	NNum is Num + 1,
	concat_atom([Name,'_',NNum],Temp),
	concat_atom(['L',NNum],LLabel),
        free(@Temp),
        new(@Temp,  button(Temp)),
        send(@Temp, label_font, Font),
        send(@Temp, shadow, 12),  % Not on Windows
        send(@Temp, radius, 8),   % No effect on Windows
        send(@Temp, show_focus_border, @off),
	send(@Temp, colour(green)),
        send(@Temp, label, LLabel),
        get(@Temp, area, Area),
	send(Area, set( width := 60) ) },
	n_lagoons(NNum, Last, Name, Font).

tt_editors([], []).
tt_editors([Name|T], [@TempEd|Es]) :-
        concat_atom([Name,'_editor'], TempEd),
        concat_atom([Name,'_text'], TempTxt),
        free(@TempEd),
        free(@TempTxt),
        new(@TempTxt, text_buffer(Name)),
        new(@TempEd,  editor(@TempTxt)),
        get(@TempEd, area, Area),
	send(Area, set( width := 300, height := 200) ),
	tt_editors(T, Es).

update_values :-
        turbidostats(List),
        update_labels,
        update_values(List,temperature),
        update_values(List,turbidity).

update_labels :-
	get(@tmenu, members, Chain),
	chain_list(Chain, CList),
        update_labels(CList).

update_labels([]).
update_labels([L|Ls]) :-
	get(L, label, Label),
	downcase_atom(Label,Lower),
	online(Lower),
        !,
	send(L, colour, colour(black)),
	update_labels(Ls).
update_labels([L|Ls]) :-
	send(L, colour, colour(darkgrey)),
	update_labels(Ls).


update_values([],_).
update_values([Name|Names],Type) :-
        offline(Name),
        concat_atom([Name,'_',Type],Temp),
        padded_value(0,Padded),
        send(@Temp, label, Padded),
        send(@Temp, colour, colour(darkgrey)),
        update_values(Names,Type).

update_values([Name|Names],Type) :-
        online(Name),
	measurement(Name, Type, Value),
        padded_value(Value,Padded),
        concat_atom([Name,'_',Type],Temp),
        send(@Temp, label, Padded),
        update_values(Names,Type).


fix_fonts([], _).
fix_fonts([H|T], Font) :-
	send(H, label_font, Font),
	fix_fonts(T, Font).

padded_value(Value, Padded) :-
        number_codes(Value, Codes),
        length(Codes, Length),
        pad_value(Length, Value, Atoms, _Color),
        concat_atom(Atoms, Padded).

record_temp_turb :-
        tt(Device, Temp, Turb),  % succeeds for each online device
        new_value(Device, temperature, Temp),
        new_value(Device, turbidity,   Turb),
        get_time(TimeStamp),
        assert(tt_reading(darwin, TimeStamp, Temp, Turb)),
        fail. % Repeat for all online devices

:- pce_end_class.

g :- notrace, nodebug, reconsult(a), go.

go :-
  freeall,
  new(@GUI, name_asker('TurbidoStatus')),
  send(@GUI?frame, icon, bitmap('./open/images/evo.xpm')),
  get(@GUI, prompt, Name),
  (Name == quit -> halt; true).

% Ignore stuff before 'DATA', then parse into c(x,y,z) lines
getRdata(List, RData) :-
	getRdata(List,AtomList,[]),
	concat_atom(AtomList,RData).

getRdata([])          --> [].
getRdata(['DATA'|Ls]) -->  !, list_to_rdata(Ls).
getRdata([ _    |Ls]) --> getRdata(Ls).

list_to_rdata( [] )   --> [].
list_to_rdata([L|Ls]) -->  ['c(', L, ')\n'], list_to_rdata(Ls).

print_list([]).
print_list([H|T]) :- writeln(H), print_list(T).

a(Cmd) :- cmd(aristotle,Cmd).
b(Cmd) :- cmd(buffon,Cmd).
c(Cmd) :- cmd(cuvier,Cmd).
d(Cmd) :- cmd(darwin,Cmd).

cmd(Name, Cmd) :-
	writeln('Sending Command to Device'(Cmd, Name)),
        send_receive(Name, Cmd, Result),
        length(Result,Length),
        writeln('Lines of data returned'(Length)),
        print_list(Result).

restore([]).
restore([Name|Names]) :-
	atom_codes(Name,[ID|_]),
	retractall(turbidostat(Name,_,_,_,_,_)),
	assert(turbidostat(Name,ID,_,_,_,_)),
	restore(Names).



auto :-	shut,
        restore([ aristotle, buffon, cuvier, darwin]),
	writeln('Identifying Turbidostats...'),
	id,
	writeln('update_values'),
	update_values,
	writeln('recolor'),
	recolor.

recolor :-
        color(Name, Type, _Value, Colour),
	concat_atom([Name,'_',Type],Label),
	send(@Label, colour(Colour)),
	get(@Label, area, A),
	send(A, set(width:=300)),
        fail ; true.

color(Name, Type, Value, Colour) :-
	( online(Name)
         -> measurement(Name, Type, Value),
            threshold(Type, Low, High, Colour),
            Low < Value,
            Value =< High
        ; Colour = colour(darkgrey),
          turbidostats(List),
          member(Name, List),
          member(Type, ['temperature','turbidity'])
        ).

freevalues :-
        turbidostats(List),
        member(Name, List),
        member(Type,[temperature, turbidity]),
        concat_atom([Name,'_',Type],Temp),
        free(@Temp),
        fail.
freevalues.

freeall :-
        freevalues,
        free(@tmenu),
        free(@plotbut),
        free(@ok),
        free(@tt),
        free(@tm),
        free(@tml),
        free(@tb),
        free(@tbl),
        free(@tbright),
        free(@edbut),
        free(@nm).

text_from_editor(E, Text) :-
        send(E?text_buffer, append, string("foobar")),
        get(E?text_buffer?contents, sub, 0, E?text_buffer?size, String),
        get(String, value, Text).

add_to_editor(Name, Codes) :-
        concat_atom([Name,'_editor'], E),
        send(@E?text_buffer, append, string(Codes)).

colorlagoon(Name,Num,Color) :-
        concat_atom([Name,'_',Num], E),
	send(@E, colour, colour(Color)).

%        get(@E?text_buffer?contents, sub, 0, @E?text_buffer?size, String)
%        send(String,append,string(Codes)).

% styrofoam:     906 mg/cm3
% aerographene: .16 mg/cm3
create_files :-
        online(Name),
        findall(ttt(Ts,Tm,Tb),tt_reading(Name,Ts,Tm,Tb),List),
        write_sequence(Name, List),
        fail.


write_sequence(Name, [ttt(Time,Tm,Tb)|Rest]) :-
	Offset is integer(Time)-1,
	concat_atom([Name,'.tdata'],Filename),
	tell(Filename),
        writeln('Time,Temperature,Turbidity'),
        write_sequence([ttt(Time,Tm,Tb)|Rest],1, Offset),
	told.

write_sequence([],_,_).
write_sequence([ttt(Time,Tm,Tb)|Rest],Sequence,Offset) :-
	RelTime is integer(Time)-Offset,
	format('~d,~d,~d,~d~n',[Sequence,RelTime,Tm,Tb]),
	NextSequence is Sequence + 1,
	write_sequence(Rest, NextSequence, Offset).
