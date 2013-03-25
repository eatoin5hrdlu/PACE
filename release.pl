%
% These definitions relocate the shared object
% locations to create a distribution.
%

:-  retractall(library_directory(_)),
    asserta(library_directory('/home/peter/Prolog/Biologic/bin/library')).

file_search_path(swi, '/home/peter/Prolog/Biologic/bin/').
file_search_path(library, swi(library)).

release :-
    ( exists_directory(bin)
      -> true
      ;  make_directory(bin)
    ),
    current_foreign_library(foreign(Lib),_),
    libfile(Lib, Src, Dest),
    format(user_error,'foreign:~q~n',[Lib]),
    copy_file(Src,Dest),
    fail.

release :-
    current_prolog_flag(windows,true),
    qsave_program('bin/ilpd', [ emulator(swi('bin/xpce-stub.exe')),
				stand_alone(true),
				goal(start)
				]).
release :-
    current_prolog_flag(unix,true),
    qsave_program('bin/ilpd', [ stand_alone(true), goal(start) ]).

libfile(Lib, Src, Dest) :-
    current_prolog_flag(windows,true),
    concat_atom(['bin/',Lib,'.dll'], Dest),
    concat_atom(['C:/cygwin/swi/',Dest], Src).

libfile(Lib, Src, Dest) :-
    current_prolog_flag(unix,true),
    current_prolog_flag(arch,Arch),
    concat_atom(['/usr/lib/swi-prolog/lib/',Arch,'/',Lib,'.so'], Src),
    concat_atom(['bin/',Lib,'.so'], Dest).

