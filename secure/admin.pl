
admin(_Request) :-
    message('handling admin~n',[]),
    reply_html_page(biologic,  title('Biologic Account Control'),
	[h1('Administrative Tools'),
		  h2(font([color=green],'Unit Tests')),
		  ul([li(a([href='/secure/incrtest'],'IncrementalHtml')),
		      li(a([href='/secure/sqlTest'], 'Check mySQL BioLogic Database')),
		      li(a([href='/secure/truenameTest'],'TrueName Test')),
		      li(a([href='/secure/truename?user=1'], 'TrueName Unit Test')),
		      li(a([href='/secure/bioutiltest'], 'BioUtil tester')),
		      li(a([href='/secure/sdlocal'], 'Shutdown Server'))]),
		  form([id=newuser,method=post,action='/secure/newuser'],
		       [label([for='email'],'New user Email:'),
			input([type=text,id=email,name='user',value='goedel@cicada.com',size=16]),
			label([for=pw1],'Password:'),
			input([type=password,id=pw1,name=password1,value='xyzzy',size=10]),
			label([for=pw2],'Retype Password:'),
			input([type=password,id=pw2,name=password2,value='xyzzy',size=10]),
			input([type=submit,value='Create New Account'])])
		 ]).



