%
% INDEX.HTML
%

indx(_Request) :-
	message('handling index~n',[]),
	reply_html_page(
			title(foo),
			[h1('foo'),h2('bar') | \scholar]).

scholar -->
	html(center(h2(a([ style('"visibility: hidden;"'),
	    id('gscholar'),
	    href('http://scholar.google.com/scholar?num=50&hl=en&lr=&q=author%3A%22P+Reintjes%22+OR+%22P+B+Reintjes%22+OR+%22Peter+Reintjes%22%20-author%3A%22GP%20Reintjes%22%20-author%3A%22Geo+P+Reintjes%22%20-assignee%3A%22George+P.+Reintjes%22%20-author%3A%22George+Peter+Reintjes%22&btnG=Search'),
	    onmouseover('flashOn(this)'),
	    onmouseout('flashOff(this)')],
	   'Google Scholar Listing')))).

