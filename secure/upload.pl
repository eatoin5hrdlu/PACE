upload(Request) :-
    format(user_error,'~q~n',[Request]),
    member(method(post), Request),           % This is an HTTP "POST"
    member(input(Stream), Request),          % Get the Post data input stream
    set_input(Stream),
    separator(Request, Boundary, Separator), % and Multipart file separators

    scan_to(Boundary),           % Search for the first Boundary
    scan_to("filename=\""),      % Search for: filename="
    copy_to("\"", atom(File)),   % Get the filename
    scan_to([13,10,13,10]),      % Then two blank lines (e.g. after 'Content-type')

    upload_filepath(File, Path), % Construct local pathname for our copy

    open(Path, write, Out, [type(binary),encoding(octet)]),  % Binary
	copy_to(Separator, Out),    % Copy the contents into the file
    close(Out),
    reply_html_page(biologic,title('Thanks'),center(h1([File,' Uploaded']))).

upload(_) :-
    reply_html_page(biologic,title('Sorry'),center(h1(['Upload Failed.']))).

copy_to(Pattern, Stream) :- % Copy data to Stream until Pattern
	get_code(C),
	with_output_to(Stream, copy_to(C,Pattern,Pattern,Collect,Collect)).

scan_to(Pattern) :-         % Call copy_to/2 (above) but discard input
	open_null_stream(Null),
        copy_to(Pattern, Null),
	close(Null).

copy_to(C, [C], _, _, _) :- !.
copy_to(C, [C|Partial], Pattern, [C|CT], Collect) :-
    !,
    get_code(NC),
    copy_to(NC, Partial, Pattern, CT, Collect).

copy_to(C, _, Pattern, [C], Collected) :- % Didn't match the pattern, but the
    append([X|Xs],[Some|Tail],Collected),    % longest non-trivial tail of collected+C
    append([Some|Tail], Rest,Pattern),       % matches beginning of Pattern
    !,                                       
    append([Some|Tail],NewCT,NewCollection), % so we restart from there after 
    format('~s', [ [X|Xs] ]),                % copying out the non-matching part
    get_code(NC),
    copy_to(NC, Rest, Pattern, NewCT, NewCollection).

copy_to(C, _, Pattern, [C], Collected) :-
    format('~s', [Collected]),

    [P|_] = Pattern, % Use first character of pattern for fast copy
    repeat,
       get_code(NC),
    (NC =:= P -> true ; (put_code(NC),fail)),

    copy_to(NC, Pattern, Pattern, Collect, Collect).

% Path for upload file:  <BIOLOGIC-HOME>/uploads/<USER>/<File>
upload_filepath(File, Path) :-
    biologic_directory(BD),
    biologic_user(User),
    concat_atom([BD,'/uploads/',User], Dir),
    ( exists_directory(Dir) -> true ; make_directory(Dir) ),
    concat_atom([Dir,'/',File], Path).

%
% Get the unique multipart-file separator from the Request.
% the Boundary is that unique string prepended with '--'
% the Separator includes a blank line "\r\n" before the pattern.
%
separator(Request, Boundary, Separator) :-
    member(content_type(MultiPartAtom), Request),
    atom_codes(MultiPartAtom, MultiPartCs),
    append("multipart/form-data; boundary=", Unique, MultiPartCs),
    append("--",   Unique,   Boundary),
    append("\r\n", Boundary, Separator).
