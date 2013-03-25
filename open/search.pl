%
% Generate a page of predicate/descriptions from a pattern.
%
% /search.html?pattern=<string>
%
%  search/1
%
:- ensure_loaded(library(helpidx)).

search(Request) :-
    message('handling search~n',[]),
    parameter(Request,pattern,Kwd),
    !,
    findall(Pred, apropos_predicate(Kwd, Pred), Matches),
    phrase(apropos(Kwd, Matches), Body),
    reply_html_page(biologic, title(['Predicates for ',Kwd]), Body).

% ERROR: Missing search pattern
search(_) :-
    reply_html_page(biologic, title('Bad request'),
     center(h1(' This URL requires the parameter: ?pattern=<search-term>'))).

apropos(Kwd, Matches) -->

             [ h2(align(center),
                  ['Predicates for ', Kwd]),
               table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th('Predicate'),
                            th('Summary')
                          ])
                     | \apropos_rows(Matches)
                     ])
             ].

apropos_rows([]) -->   [].
apropos_rows([pred(Name, Arity, Summary)|T]) -->
        html([ tr([ td(\predicate_reference(Name/Arity)),
                    td(em(Summary))
                  ])
             ]),
        apropos_rows(T).

predicate_reference(Name/Arity) -->
        { www_form_encode(Name, Encoded),
          sformat(Href,
		'http://www.swi-prolog.org/pldoc/man?predicate=~w%2f~w',
                  [Encoded, Arity])
        },
        html(a(href(Href), [Name, /, Arity])).

apropos_predicate(Pattern, pred(Name, Arity, Summary)) :-
        predicate(Name, Arity, Summary, _, _),
        (   '$apropos_match'(Pattern, Name)
        ->  true
        ;   '$apropos_match'(Pattern, Summary)
        ).

parameter(Request,Parameter,Value) :-
        member(search(X),Request),
        member('='(Parameter,Value), X).





