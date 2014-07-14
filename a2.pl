#!/usr/bin/xpce
:- use_module(library(time)).
:- use_module(library(pce)).
:- use_module(library(process)).
:- use_module(library(help_message)).
:- use_module(pyio).
:- dynamic turbidostat/6. % t(Name, ID, Port, PID, In, Out)
:- dynamic turbidostats/1.
:- dynamic current_turbidostat/1.
:- dynamic tt_reading/4.
:- dynamic lastfile/2.
:- dynamic measurement/3. % Name, {temperature,turbidity}, Value
:- dynamic(valve_state/1).
:- dynamic(override/0).

:- multifile measurement/3.
:- use_module(library('R')).
:- use_module(library(apply)).

%assert_list([]).
%assert_list([H|T]) :- assert(H), assert_list(T).

initial_upcase_atom(In, Out) :-
	atom_codes(In, [NH|NT]),
	code_type(UH, to_upper(NH)),
	atom_codes(Out, [UH|NT]).

%---------------------------------------------------------------
% BEGIN SITE AND MACHINE SPECIFIC ITEMS
% WHICH MAY BE MODIFIED IF NECESSARY
%---------------------------------------------------------------
%

:- r_bin('C:/cygwin/R/bin/i386/Rterm.exe').

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
        pacefont(tiny, font(courier, normal, 14)),
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
	 pacesize(1330,730),
	 paceMenuWidth(300),
	 paceLocation(point(230,20)),
	 pacefont(tiny, font(courier, normal, 14)),
	 pacefont(small, font(courier, normal, 24)),
	 pacefont(medium,font(courier, bold, 44)),
	 pacefont(large, font(courier, bold, 68)),
	 pacefont(huge,  font(courier, bold, 92)),
%	 pacefont(small, font(courier, normal, 10)),
%	 pacefont(medium,font(courier, bold, 10)),
%	 pacefont(large, font(courier, bold, 10)),
%	 pacefont(huge,  font(courier, bold, 10)),
	 pad_value(0,_,['  0  '],black),
	 (pad_value(1,0,['  0  '],black) :- !),
	 pad_value(1,V,['   ',V,'  '],black),
	 pad_value(2,V,['  ',V,'  '],black),
	 pad_value(3,V,[' ',V,' '],black),
	 pad_value(4,V,[V]),black]).

:- ( current_prolog_flag(windows,true) -> toshiba(A)
   ; current_prolog_flag(arch,armv6l) -> lcd(A) % hdmi(A)?
   ; ubuntu(A)
   ),
   maplist(assert,A).

turbidostats([aristotle, buffon, cuvier, darwin
%             , erasmus,  ford,  gregor, huxley
             ]).


measurement( aristotle, temperature,  10).
measurement( aristotle, turbidity,   290).
measurement( aristotle, level,   2.0).
measurement( buffon,    temperature,  0).
measurement( buffon,    turbidity,    0).
measurement( buffon, level,   2.0).
measurement( cuvier,    temperature,  8).
measurement( cuvier,    turbidity,   50).
measurement( cuvier, level,   2.0).
measurement( darwin,    temperature, 37).
measurement( darwin,    turbidity,  800).
measurement( darwin, level,   2.0).
measurement( erasmus, temperature,  10).
measurement( erasmus, turbidity,   290).
measurement( erasmus, level,   2.0).
measurement( ford,    temperature,  0).
measurement( ford,    turbidity,    0).
measurement( ford, level,   2.0).
measurement( gregor,    temperature,  8).
measurement( gregor,    turbidity,   50).
measurement( gregor, level,   2.0).
measurement( huxley,    temperature, 37).
measurement( huxley,    turbidity,  800).
measurement( huxley, level,   2.0).

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

showr(Name) :-
        lastfile(Name, Filename),
	( current_r_session(R) -> r_close(R) ; true ),
	r_open,
	concat_atom(['read.csv("',Filename,'",header=TRUE,sep=",")'],Read),
	catch( r_in(d <- Read), Except, (write('['), write(Except), writeln(']'),fail) ),
	r_print('dev.new(width=6,height=8,xpos=40,ypos=40)'),
	r_in(par(mfrow=c(2,1))),
	r_in( plot('d$Time','d$Turbidity',
		   xlim=c(0,max('d$Time')),
		   ylim=c(min('d$Turbidity')-20,max('d$Turbidity')+50)) ),
%	r_print('dev.new(width=6,height=3.5,xpos=700,ypos=40)'),
	r_in( plot('d$Time','d$Temperature',
		   xlim=c(0,max('d$Time')),
		   ylim=c(min('d$Temperature')-20,max('d$Temperature')+50)) ).


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

% os_dependent(Python, Filter, Cwd)

os_dependent('C:/Python27/python.exe',
             'C:/cygwin/home/peter/srclab/PACE/pyship.py',
             'C:\\cygwin\\home\\peter\\srclab\\PACE') :-
        current_prolog_flag(windows,true),!.

os_dependent('/usr/bin/python',
	     '/home/peter/src/PACE/pyship.py',
	     '/home/peter/src/PACE').


% number is N-1 for comN:
os_dependent_portlist([2,3,4,5]) :-
        current_prolog_flag(windows,true),!.
% number is N in /dev/ttyUSBN
os_dependent_portlist([0,1,2,3,4]).  

id :-	os_dependent_portlist(PortList),
        member(Port, PortList),
	number_chars(Port, PortChs),
	atom_chars(PortAtom, PortChs),
%	format('calling ~w with arg ~q~n', [Filter, PortAtom]),
        add_to_editor(buffon,PortChs),
        add_to_editor(buffon,"\n"),
	connect_turbidostat(PortAtom),
	fail.

id :- writeln('Turbidostat Identification done').

connect_turbidostat(Port) :-  % Port is atom, not number
        os_dependent(Python, Filter, Cwd),
	writeln(process(Python)),
        process_create(Python,
		[ '-u', Filter ],
		[ stdin(pipe(In)), stdout(pipe(Out)), cwd(Cwd), process(PID)]),
	writeln(after(PID)),
        set_stream(In, buffer(false)),
	format(In, '~a~n', [Port]), % Tell python program which serial device
        sleep(1),  % Wait for Python to open Arduino
	read_line_to_codes(Out, Codes),
	( Codes = [0'c] % 'c' for "connected"
          -> true
         ; catch( format(In,'x~n',[]), _, fail ), fail
        ), % 'x' kills process
        sleep(1),     % Arduino takes a while to begin processing input
	getID(In, Out, C),
	( memberchk(C, "abcdefgh") ->
	  retract(turbidostat(Name, C,       _, _,  _,   _)),
	  assert( turbidostat(Name, C,  Port, PID, In, Out)),
	  retractall(current_turbidostat(_)),
	  assert(current_turbidostat(Name)),
%          set_stream(Out, timeout(4)), % Linux only?
	  writeln(online(Name))
	;
	  format('Unnamed Turbidostat(~c) on Port: ~a~n',[C, Port]),
          format(In,'x~n',[]), % Shut down python process
          close(In),
          close(Out),
          fail
	).


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
	send(@turbidostat, colour(darkgreen)),
	send(@turbidostat, label('Return')),
        send(@override, colour(darkgreen)),
	send(@override, label('Normal')),

	assert(previous(darwin)),
        assert(lastfile(darwin, 'darwin.tdata')),

	new(Msg1, message(@prolog, record_temp_turb)),
	new(@dtimer, timer(6.0, Msg1)),  % Sample every 5 minutes (300)

%	new(@dtimer, timer(600.0, Msg1)),  % Sample every 10 minutes
%	new(@dtimer, timer(10.0, Msg1)),   % Sample every 10 seconds

	new(Msg2, message(@prolog, show_temp_turb)),
        new(@stimer, timer(4.0, Msg2)), % 2sec practically hangs the machine!

	new(Msg3, message(@prolog, checkTime)),
	new(@ctimer, timer(600.0, Msg3)),

%	send(W, attribute, attribute(timer, @dtimer)),
%	send(W, attribute, attribute(timer, @stimer)),
%	send(W, attribute, attribute(timer, @ctimer)),
	send(@dtimer, start),
	send(@stimer, start),
	send(@ctimer, start),
	writeln(delayed_initialization_completed).

delayed_execution(Time, Goal) :-
	new(Msg, message(@prolog, (Goal,send(@ex1,stop),free(@ex1)))),
        new(@ex1, timer(Time, Msg)),
	send(@ex1, start),
	writeln(created_delayed_execution(Goal)).

add_tmenu(W,Name,Help) :-
	free(@Name),
        new(@Name, button(Name)),
        send(@Name, help_message, tag, Help),
        send(W, append(@Name,right)).

spacer(W,Length) :- 
	new(L, label('')),
	send(L, length, Length),
	send(W, append(L,right)).

:- pce_begin_class(name_asker, dialog, "Quad-PACE Apparatus Control Panel").

initialise(W, Label:[name]) :->
        "Initialise the window and fill it"::
        send_super(W, initialise(Label)),
        pacesize(WinWidth,WinHeight),
        send(W, size, size(WinWidth, WinHeight)),
        add_tmenu(W, quit, 'Exit Turbidostat Control Panel'),
        spacer(W,4),
% UNCOMMENT NEXT LINE FOR DEBUGGING
	add_tmenu(W, ok,'Exit to Prolog Prompt (for tracing)'),
        add_tmenu(W, turbidostat, 'Toggle Valve On/Off (yellow LED indicates ON)'),
        add_tmenu(W, override,    'Take/Return control to Turbidostat'),
        add_tmenu(W, connect,     'Find all available Turbidostats'),
        add_tmenu(W,'Write Data', 'Save current data points into a file'),
        add_tmenu(W, plot,        'Display Graph of last data saved'),
        spacer(W,4),
        add_tmenu(W, edit, 'Edit the main source file'),
        add_tmenu(W, save, 'Save state'),
        fix_fonts(W),
        writeln('after fix_fonts'),
	turbidostats(NameList),
        tgroups(NameList, W, next_row),
        paceLocation(Location),
        send_super(W, open, Location).

% Temporary: Set Darwin active when entering 'Plot' button
% to ensure that we read darwin.tdata file every time.
%
%        new(Code1, message(@prolog, set_turbidostat, darwin)),
%        send(@plot, recogniser, handler(area_enter, Code1)),

%
% Fire up editor on this source file 'a.pl', attach to 'edit' button
%
%        new(Code2, message(@prolog, emacs, 'a.pl')),
%        send(@edit, recogniser, click_gesture(left,'',single,Code2)),

edit(_) :-> emacs('a.pl').

save(_) :-> qsave_program('tmp.exe',[goal=true,toplevel=prolog,init_file=none]).

ok(W) :->
        writeln(ok),
        send(W, return(ok)).

%        get(W, member(name), NameItem),
%        get(NameItem, selection, Typed),
%	 r_print('dev.off()').

% browse(_) :-> manpce.


prompt(W, Value:name) :<-
        "Open it, destroy it and return the result"::
        get(W, confirm, Value).

turbidostat(_)  :-> toggle_valve.
override(_)     :-> toggle_override.

quit(W)         :-> "User pressed the Quit button"::
                    send(W, return(quit)).

plot(_)         :-> current_turbidostat(Sel), showr(Sel).
matrix(_)       :-> plot_matrix.
connect(_)      :-> delayed_startup.
'Write Data'(_) :-> create_files, add_to_editor(buffon, "wrote data").


toggle_valve :-
	current_turbidostat(Sel),
	writeln(start_toggle_valve),
	(valve_state(Sel) ->
	    retractall(valve_state(Sel))
	;
	    assert(valve_state(Sel))
	),
	writeln(end_toggle_valve).


toggle_override :-
	writeln(start_toggle_override),
	(override ->
	    retract(override),
	    send(@override, colour(darkgreen)),
	    send(@override, label('Normal'))
	;
	    assert(override),
	    send(@override, colour(red)),
	    send(@override, label('Override'))
	),
	writeln(end_toggle_override).


text_fred :- new_value( darwin, turbidity,  800),
              text_from_editor(@aristotle_editor, Text),
              show_slots(Text),
              writeln(got(Text)).

new_values :-
	recolor,
        update_values.

%update(_W) :->
%        recolor,
%        update_values.

% Return temp limit

%
% get_parameter(darwin, ch, -TempLimit)
% and update the display.
%
get_parameter(Device, Parameter, Value) :-
	get_parameter_value(Device, Parameter, Value),
        concat_atom([Device,'_',Parameter], Temp),
	send(@Temp, selection, Value).

get_parameter_value(Device, Parameter, Value) :-
	online(Device),
        send_receive(Device,[Parameter],[OneLine]),
        atom_codes(OneLine, Codes),
        parseint(TempCs, Codes, _),
        number_codes(Value, TempCs).

% Return pdate display and return latest Temp/Turbidity
% tt(?, -, -)
tt(Device, Temp, Turb) :-
        online(Device),
        send_receive(Device,[tt],[OneLine]),
	writeln(got_this(OneLine)),
        atom_codes(OneLine, Codes),

%       update_measurements([temperature, turbidity],Codes,[]).
%       update_measurements([temperature, turbidity, level],Codes,[]).
 
        % Parse Temperature and Turbidity
        parseatom(TempCs, Codes, NextCodes),
        number_codes(Temp, TempCs),
        retractall(measurement(Device, temperature, _)),
        (Temp > 90 ->
	   assert(measurement(Device, temperature, 'N/A'))
	;  assert(measurement(Device, temperature, Temp))
        ),
        parseatom(TurbCs, NextCodes, []),
        number_codes(Turb, TurbCs),
	writeln(measurement(Device, turbidity, Turb)),
        retractall(measurement(Device, turbidity, _)),
	assert(measurement(Device, turbidity, Turb)).

update_measurements([],_)          --> [].
update_measurements([Type|Types],Device) -->
	parseatom(Chars),
	{ number_codes(Value, Chars),
          retractall(measurement(Device, Type, _)),
	  assert(measurement(Device, Type, Value))
        },
	update_measurement(Types,Device).

parseint([0'-|T]) --> [0'-], !, parseint(T).
parseint([H|T]) --> [H], { code_type(H,digit)}, !, parseint(T).
parseint([])    --> [].

parseatom([])    --> [32], !.
parseatom([H|T]) --> [H],  !, parseatom(T).
parseatom([])    --> [].

list_to_stream([H,I|T], Stream) :-
       format(Stream, '~w ', [H]),
       list_to_stream([I|T], Stream).
list_to_stream([H], Stream)     :-
       format(Stream, '~w~n', [H]).

send_receivex(Device, Command, Result) :-
         send_receive0(Device, Command, Result).

%         ( send_receive0(Device, Command, Result) -> true
%         ; sr_recovery(Device, Command, Result)
%         ).

%        catch(send_receive0(Device, Command, Result),
%              Except,
%              (writeln(Except),halt)).
%              sr_recovery(Device, Command, Result)).

:- dynamic recovery_tries/1.
recovery_tries(1).

sr_recovery(Device, Command, Result) :-
        sleep(2),
        recovery_tries(N),
        (N > 3 ->
               writeln('Quitting after 3 attempts to reconnect with '),
               writeln(Device),
               halt
        ; 
          write(N),
          write(' attempt(s) reconnecting with '),
          writeln(Device),
          retract(recovery_tries(M)),
          MM is M + 1,
          assert(recovery_tries(MM))
        ),
        turbidostat(Device, _ID, Port, _PID, _In, _Out),
        connect_turbidostat(Port),
        send_recieve(Device, Command, Result).

send_receive0(Device, Command, Result) :-
        turbidostat(Device, _ID, _Port, _PID, In, Out),
        nonvar(In),
        list_to_stream(Command, In),
%	wait_for_input([Out],_X,3),
	read_line_to_codes(Out, Codes),
        atom_codes(Atom, Codes),
        receive(Atom, Out, Result,Device).

receive('end_of_data', _, [], _) :- !.
receive(Line, Out, [Line|Lines], Name) :-
%	wait_for_input([Out],_X,3),
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

tt_texts([],          _,    _, [] ).
tt_texts([Name|T], Type, Font, [@Temp|As]) :-
        concat_atom([Name,'_',Type],Temp),
        free(@Temp),
        new(@Temp,  text_item(Temp,0)),
        send(@Temp,  type, int),
        send(@Temp,  length, 4),
%        send(@Temp, font, Font),
%	send(@Temp, colour(darkgrey)),
%        send(@Temp, selection, '100'),
	tt_texts(T, Type, Font, As).

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
        turbidostats(List),
        update_labels(List).

update_labels([]).
update_labels([Name|Names]) :-
        concat_atom([Name,'_main'], Main),
	( online(Name) ->
	    send(@Main, colour, colour(black))
	;   send(@Main, colour, colour(darkgrey))
        ),
	update_labels(Names).


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

update_limits :-
        turbidostats(List),
        update_limits(List).

update_limits([]).
update_limits([Name|Names]) :-
	online(Name),
	!,
	get_parameter(Name, ch, _), % Side effect to update display
	get_parameter(Name, th, _),
	update_limits(Names).
update_limits([_|Names]) :-
	update_limits(Names).

fix_fonts(W) :-
	get(W, graphicals, Chain),
	chain_list(Chain, CList),
	pacefont(small, Small),
        new(F3, Small),
	findall(_, ( member(G,CList), send(G,label_font,F3)), _).

padded_value(Value, Padded) :-
        atom_codes(Value, Codes),
        length(Codes, Length),
        pad_value(Length, Value, Atoms, _Color),
        concat_atom(Atoms, Padded).

record_temp_turb :-
	writeln(record_temp_turb),
        tt(Device, Temp, Turb,Level),  % succeeds for each online device
        new_values,
        get_time(TimeStamp),
        assert(tt_reading(Device, TimeStamp, Temp, Turb, Level)),
        fail. % Repeat for all online devices

show_temp_turb :-
	writeln(show_temp_turb),
	findall(D, tt(D,_,_,_),_),
	new_values,
	show_level,
	show_valve.

show_level :-
	current_turbidostat(Sel),
        send_receive(Sel, [l], [L]),
        ( L = '0' -> true
        ; 
	concat_atom([Sel,'_turbidity'],Temp),
	send(@Temp, colour(red))
        ).

show_valve :-
	current_turbidostat(Sel),
        ( override ->         send_receive(Sel, [n], _)
        ;                     send_receive(Sel, [o], _)
        ),
        ( valve_state(Sel) ->
%            send_receive(Sel, [vt], _),
	    send(@turbidostat, colour(red)),
	    send(@turbidostat, label('Turbidostat'))
        ;  % send_receive(Sel, [vr], _),
	    send(@turbidostat, colour(darkgreen)),
	    send(@turbidostat, label('Return'))
        ).


tgroups([], _, _).
tgroups([Name|Names], W, Where) :-
	concat_atom([Name,'Group'],GName),
        free(@GName),
        new(@GName, ttgroup(Name)),
        send(W, append, @GName, Where),
        tgroups(Names, W, right).

:- pce_end_class.

g :- notrace, nodebug, reconsult(a), go.

go :-
  freeall,
  new(@GUI, name_asker('TurbidoStatus')),
  send(@GUI?frame, icon, bitmap('./open/images/evo.xpm')),
  get(@GUI, prompt, _Name).

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
	trace,
	update_values,
	update_limits,
	recolor,
	writeln('Identified.').

plot_matrix :-
	r_print('dev.new()'),
	r_in(x<-seq(0.01,1,0.01)),
	r_in(par(mfrow=c(3,2))),
	r_print('plot(x, sin(x), type="l")'),
	r_print('lines(x, log(x), type="l", col="red")'),
	r_print('plot(x, exp(x), type="l", col="green")'),
	r_print('lines(x, log(x), type="l", col="green")'),
        r_print('plot(x, tan(x), type="l", lwd=3, col="yellow")'),
	r_print('lines(x, exp(x), col="green", lwd=3)'),
	r_print('plot(x, sin(x*x), type="l")'),
	r_print('lines(x, sin(1/x), col="pink")').

recolor :-
        color(Name, Type, _Value, Colour),
	concat_atom([Name,'_',Type],Label),
	send(@Label, colour(Colour)),
	get(@Label, area, A),
	send(A, set(width:=300)),
        fail ; true.

color(Name, Type, Value, Colour) :-
	( online(Name) ->
            measurement(Name, Type, Value),
	    number(Value),
            threshold(Type, Low, High, Colour),
            Low < Value,
            Value =< High
        ;
            turbidostats(List),
            member(Name, List),
            member(Type,[temperature, turbidity]),
            Colour = colour(darkgrey)
        ).

freevalues :-
        turbidostats(List),
        member(Name, List),
        member(Type,[temperature, ch, turbidity, th]),
        concat_atom([Name,'_',Type],Temp),
        free(@Temp),
        fail.
freevalues.

freeall :-
        free(@override),
        free(@turbidostat),
        free(@dtimer),
        free(@stimer),
        free(@ctimer),
        free(@plot),
        free(@ok),
        free(@tt),
        free(@tm),
        free(@tml),
        free(@tb),
        free(@tbl),
        free(@tbright),
        free(@edit),
        free(@nm),
        freevalues.

text_from_editor(E, Text) :-
        send(E?text_buffer, append, string("foobar")),
        get(E?text_buffer?contents, sub, 0, E?text_buffer?size, String),
        get(String, value, Text).

add_to_editor(_, "end_of_data") :-
	!,
        get_time(TimeStamp),
        atom_codes(TimeStamp,TimeCodes),
	append(_,[X],TimeCodes),
	add_to_editor(aristotle,[X]).

add_to_editor(Name, Codes) :-
        concat_atom([Name,'_editor'], E),
        send(@E?text_buffer, append, string(Codes)).

%        get(@E?text_buffer?contents, sub, 0, @E?text_buffer?size, String)
%        send(String,append,string(Codes)).

% styrofoam:     906 mg/cm3
% aerographene: .16 mg/cm3
create_files :-
	get_time(TS),
	convert_time(TS,_Y,Mon,Day,Hour,_,_,_),
        online(Name),
        findall(ttt(Ts,Tm,Tb,Tl),tt_reading(Name,Ts,Tm,Tb,Tl),List),
	concat_atom([Name,'_',Day,'_',Mon,'_',Hour],Filename),
        write_sequence(Filename, List),
        retractall(lastfile(Name,_)),
        assert(lastfile(Name,Filename)),
        fail.
create_files.

write_sequence(Filename, [ttt(Time,Tm,Tb,Tl)|Rest]) :-
	Offset is integer(Time)-1,
	tell(Filename),
        writeln('Time,Temperature,Turbidity,Level'),
        write_sequence([ttt(Time,Tm,Tb,Tl)|Rest],1, Offset),
	told.

write_sequence([],_,_).
write_sequence([ttt(Time,Tm,Tb,Tl)|Rest],Sequence,Offset) :-
	RelTime is integer(Time)-Offset,
	format('~d,~d,~d,~d,~f~n',[Sequence,RelTime,Tm,Tb,Tl]),
	NextSequence is Sequence + 1,
	write_sequence(Rest, NextSequence, Offset).

%
% Every four hours, save all data into a file
%
:- dynamic(lastCheck/1).

checkTime :-
	get_time(Now),
	( lastCheck(LastTime) ->
	    Delta is Now - LastTime,
	    FourHours is 60*60*4,
	    (Delta > FourHours ->
		create_files,
		retractall(tt_reading(_,_,_,_)),
		retract(lastCheck(_)),
		assert(lastCheck(Now))
	    ; true
	    )
	; assert(lastCheck(Now))
        ).

:- pce_begin_class(ttgroup, device).
:- pce_global(@ttgroup_format, make_ttgroup_format).

make_ttgroup_format(F) :-
 new(F, format(horizontal, 1, @on)),
 send(F, adjustment, vector(center)),
 send(F, row_sep, 2).

initialise(Group, Name) :->
   send_super(Group, initialise),
   send(Group, format, @ttgroup_format),
   pacefont(tiny, Tiny),  new(F0, Tiny),
   pacefont(small, Small),  new(F1, Small),
   pacefont(large, Large),  new(F3, Large),

   button_thing(Name, main, F3, Main),
   initial_upcase_atom(Name,Label),
   send(Main, label, Label),

   new(Code1, message(@prolog, set_turbidostat, Name)),
   send(Main, recogniser, click_gesture(left,'',single,Code1)),

   send(Group, display, Main, point(0,0)),

   button_thing(Name, temperature, F3, Temp), 
   send(Temp, label, '   37   '),
   send(Group, display, Temp, point(0,1)),

   int_thing(Name, ch, F1, 35, TmLimit),
   send(TmLimit, label, 'Temp Limit'),
   send(Group, display, TmLimit, point(0,2)),

   button_thing(Name, turbidity,  F3, Turb),
   send(Turb, label, '   650   '),
   send(Group, display, Turb, point(0,3)),

   int_thing(Name, th, F1, 500, TbLimit),
   send(TbLimit, label, 'Turbidity Limit'),
   send(Group, display, TbLimit, point(0,4)),

   int_thing(Name, lv, F1, 2.1, TbLevel),
   send(TbLevel, label, 'Liquid Level'),
   send(Group, display, TbLevel, point(0,10)),

   new(Code2, message(@prolog, set_empty_level, Name)),
   send(TbLevel, recogniser, click_gesture(left,'',double,Code2)),

   edit_thing(Name, F0, 300, 200, Editor),
   send(Group, display, Editor, point(0,5)).

button_thing(Name, Type, Font, @Temp) :-
        concat_atom([Name,'_',Type],Temp),
        free(@Temp),
        new(@Temp,  button(Temp)),
        send(@Temp, label_font, Font),
        send(@Temp, shadow, 12),  % Not on Windows
        send(@Temp, radius, 8),   % No effect on Windows
        send(@Temp, show_focus_border, @off),
	send(@Temp, colour(darkgrey)),
        get(@Temp, area, Area),
	paceMenuWidth(Width),
	send(Area, set( width := Width) ).

int_thing(Name, Type, Font, Value, @Temp) :-
        concat_atom([Name,'_',Type],Temp),
        free(@Temp),
        new(Handler, message(@prolog,turbidostat_setting,Name,Type)),
        new(@Temp, text_item(Type,Value,Handler)),
        get(@Temp, value_text, Text),
        send(Text, format, right),
        send(@Temp, type, int),
        send(@Temp,  length, 5),
        send(@Temp,  label_font, Font),
        send(@Temp,  font, Font),
        send(@Temp,  value_font, Font).

edit_thing(Name, _Font, Width, Height, @TempEd) :-
        concat_atom([Name,'_text'], TempTxt),
        free(@TempTxt),
        new(@TempTxt, text_buffer(Name)),

        concat_atom([Name,'_editor'], TempEd),
        free(@TempEd),
        new(@TempEd,  editor(@TempTxt)),

        get(@TempEd, area, Area),
	send(Area, set( width := Width, height := Height) ).

:- pce_end_class.

turbidostat_setting(Name, Type) :-
        online(Name),
        !,
        concat_atom([Name,'_',Type],Temp),
        get(@Temp, selection, Value),
        set_turbidostat_parameter(Type, Name, Value),
	send_receive(Name, [s], _).

turbidostat_setting(Name, Type) :-
        concat_atom([Name,'_',Type],Temp),
        get(@Temp, selection, Value),
        write(Name),
        write(' is not online. Value for '),
        write(Temp),
        write(' was not set to '),
	write(Value),
        writeln('.').

set_turbidostat_parameter(Parameter, Name, Value) :-
	send_receive(Name, [Parameter,Value], _).

set_turbidostat(Name) :-
        online(Name),
	retractall(current_turbidostat(_)),
	assert(current_turbidostat(Name)).

% :- consult(level).
