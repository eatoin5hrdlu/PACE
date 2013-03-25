
createNewExperiment(Request) :-
    js_return(Request, Function, TimeReturn),
    get_parameters(Request, Parameters),
%   format(user_error, 'PARAMETERS[~q]~n', [Parameters]),
    member( ename(ExpNameEncoded),  Parameters),
    uri_iri(ExpName, ExpNameEncoded),
    member( user(UserEncoded), Parameters),
    uri_iri(User, UserEncoded),
    pbio:createExperiment(User, ExpName, ENum),
   
    reply_html_page( biologic,
		     [ title(['Experiment ',ExpName,' #',ENum,' Created']),
		       script([language(javascript)],[Function])],
		     body([onLoad(TimeReturn)],
	             center(h1([ExpName,' created as Experiment #',ENum,
                               ' for ', User])))
		   ).

createNewExperiment(_)    :-
    reply_html_page( biologic, [ title('Failed the create Experiment') ],
		     center(h1('Failed')) ).


