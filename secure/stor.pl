stor(Request) :-
    member( method(post), Request),
    member( input(Stream), Request),
    repeat,
      read(Stream, Term),
    (Term = end_of_file -> true ; assert(Term), fail),
    reply_html_page(biologic,title('Thanks'),center(h1('Asserted Terms'))).

stor(_) :- reply_html_page(biologic,title('Sorry'),center(h1('Failed.'))).





