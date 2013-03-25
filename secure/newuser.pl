
show_properties :-
       current_input(I),
       stream_property(I, Prop),
       format(user_error,'STREAM PROPERTY: ~q~n',[Prop]),
       fail.
show_properties.

newuser(Request) :-
    message_list(Request),   % debug
    member( user(UserEncoded), Request),
    uri_iri(User, UserEncoded),  % Convert hex codes (e.g. %40 -> '@')

    member(password1(Password), Request),
    ( member(password2(Password), Request) -> true
    ; errorPage(Request, 'Passwords did not match, try again')
    ),

    ( userNameExists(User)
     -> errorPage(Request, ['User name ',User, ' already in use.'])
     ; true
    ),
    uri_iri(TypedPassword, Password),
    newUser(User, TypedPassword),
    js_return(Request, Function, Flyback),
    reply_html_page( biologic,
		[ title('BioLogic Account Created'),
		  script([ language(javascript) ],[ Function ])],
		  body([ onLoad(Flyback) ],
	          center(h1(['Account for [',User,'] successfully created.'])))
		   ).

newuser(Request) :-
    errorPage(Request, 'Unknown error occurred creating new user').
  

