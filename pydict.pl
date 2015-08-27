% To create your own configuration edit a copy of template.pl named
%  <hostname>.pl (or <evostatname>.pl if more than one on the same computer).
% The 'evostat' program reads this file and creates <hostname>.settings
% for the the Python programs (ipcam.py, level.py, fluor.py, pH.py)
%
% You can edit the .settings file to test/debug the Python programs
% but it will be overwritten whenever you run: 'evostat'
%
:- dynamic config/1.       % Loaded configuration
:- dynamic supress/1.      % Terms to exclude from dictionary (see grammar below)

:- use_module(library(lists), [ flatten/2 ] ).

check_file(Root) :-   % consult(foo) will work for files named foo or foo.pl
	( exists_file(Root)
        -> true
        ; concat_atom([Root,'.pl'],File),
	  exists_file(File)
        ).

% config_name(-Root)

config_name(Root) :-
	current_prolog_flag(argv,Args),  % Command-line argument
	member(Root,Args),
	check_file(Root),
	!.

config_name(Root) :-
	gethostname(Name),    % <HOSTNAME>.pl configuration file
	atom_chars(Name,Cs),
	( append( RCs,['.'|_],Cs ) % Could be full domain name ('x.y.com')
        -> atom_chars(Root,RCs)
        ;  Root = Name
        ),
	check_file(Root).

% Convert Prolog term to Python dictionary

pl2PythonDict(Data, PyAtom) :-       % Generate a Python dictionary string
	pl2py(Data, PythonDict, []),
	flatten(['{\n',PythonDict,'}\n'], Flat),
	concat_atom(Flat, PyAtom).

quote_atom(false,'None') :- !.
quote_atom([A],Q) :- quote_atom(A,Q).
quote_atom(N,N)   :- number(N).
quote_atom(A,Q)   :- atom(A), concat_atom(['''',A,''''],Q).

% DCG versions
quote_atom(false) --> ['None'].  % None is Python's 'false'
quote_atom([A])   --> quote_atom(A).
quote_atom(N)     --> {number(N)},[N].
quote_atom(A)     --> {atom(A)}, ['''',A,''''].

quote_atoms([],[], _).
quote_atoms([A],[Q],_) :- quote_atom(A,Q).
quote_atoms([A|T],[Q,Spacer|QT],Spacer) :- quote_atom(A,Q), quote_atoms(T,QT,Spacer).

%DCG versions
quote_atoms([],_)         --> [].
quote_atoms([A],_)        --> quote_atom(A).
quote_atoms([A|T],Spacer) --> quote_atom(A), [Spacer], quote_atoms(T,Spacer).

:- dynamic tabs/1.
tabs(1).

tabin  --> { retract(tabs(N)), NN is N + 1, assert(tabs(NN)) }.
tabout --> { retract(tabs(N)), NN is N - 1, assert(tabs(NN)) }.

indent     --> { tabs(N) }, indent(N).
indent(N)  --> { N<1},  !.
indent(N)  --> { N>0, NN is N-1}, indentation, indent(NN).

indentation --> ['       '].

pl2py([])    --> !, [].
pl2py(Term)  --> { supress(Term)  }, !, [].
pl2py(A)     --> { quote_atom(A,Q)}, !, [Q].
pl2py([H])   --> !, pl2py(H), [' \n'].
pl2py([H|T]) --> !, indent, pl2py(H), pl2py(T).

pl2py(Term) --> { Term =.. [F|[A]],   % SINGLE f(a)
	          quote_atom(F,QF),
	          quote_atom(A,QA),
		  !
                },
		[QF, ' :  ', QA, ',\n'].

pl2py(Term) --> { Term =.. [F|[A|As]],    % TUPLE   f(a,b,...)
	          quote_atom(F,QF),
	          quote_atoms([A|As], Str, ','),
                  !
                },
	        [QF,' : (', Str, '),\n' ].

pl2py(Term) --> { Term =.. [F|Args],  % NESTED DICTIONARY  f(g(a),...)  
	          quote_atom(F, QF)
	        },
		[QF, ' :  {'],
		pl2py(Args),
		['      }'].
                 
% Configuration <name> is either a command-line argument or <hostname>
% The corresponding Prolog file ( <name>.pl ) must exist.

% Load configuration and generate Python .settings (dictionary) file. 

xx :-	config_name(Root),          %  Find out configuration name
	consult(Root),              % Consult it
	config(List),                         % Get the configuration data
	assert(supress(layout(_))),           % Leave this out of the dictionary
	assert(supress(screen(_,_,_))),       %     '' 
	pl2PythonDict(List, PyString),        % Convert to Python Dictionary
	concat_atom([Root,'.settings'],File), % Write it out
	tell(File),
	write(PyString),
	told.










