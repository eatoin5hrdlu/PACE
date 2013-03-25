% SWI-Prolog httpd handler for Python scripts.
% Turn a Prolog Request into a Python dictionary and
% pass it to the Python nexus.

:- use_module(library(process)).

pl2py(method,     'REQUEST_METHOD', X, X).
pl2py(port,       'SERVER_PORT',    X, X).
%pl2py(user_agent, 'USER_AGENT',     X, X).
pl2py(protocol,   'SERVER_PROTOCOL',X, X).
pl2py(path,       'PATH_INFO',      X, X).
pl2py(request_uri,'QUERY_STRING', In, Query) :- append(_,[0'?|Query],In).

trans(Prolog, Python) :-
    Prolog =.. [PlAttr, Value],
    atom(Value),
    atom_codes(Value, VChs),
    pl2py(PlAttr, PyAttr, VChs, NewChs),
    atom_codes(NewValue, NewChs),
    Python =.. [PyAttr, NewValue].

print_dictionary(Stream, List) :-
    format(Stream, '{', []),
    format(user_error, '{', []),
    ( member(Item, List),
      trans(Item, Trans),
      Trans =.. [Attr,Value],
      format(Stream, '''~w'' : ''~w'',', [Attr,Value]),
      format(user_error, '''~w'' : ''~w'',~n', [Attr,Value]),
      fail
    ; 
      format(Stream, '}',[]),
      format(user_error, '}~n',[])
    ).
% Turn a Prolog Request list into a Python dictionary
% and pass it to the Python script named in the Request.

server_root('/home/peter/Prolog/mysite/').

jang(Request) :-
    ( member(X, Request),format(user_error,'~q~n',[X]),fail ; true ),
    member(path(Program), Request),
    atom_codes(Program, PChs),   % Make sure that the URL
    append(Pre,".pl",PChs),        % refers to a Python script
    append("/secure/",Pre2,Pre),
    append(Pre2,".py",PyChs),
    atom_codes(PyProgram, PyChs),
    server_root(Root),
    concat_atom([Root,PyProgram], Path), % Get Program's location
    format(user_error, '~q~n', [Path]),

    process_create('/cygwin/bin/python2.6',
       ['C:\\cygwin\\home\\peter\\Prolog\\mysite\\jang.py'],
       [ stdin(pipe(In)), stdout(pipe(Out)) ] ),
    print_dictionary(In, Request),
    close(In),
    read_stream_to_codes(Out, Result),
    format('~s',[Result]).



