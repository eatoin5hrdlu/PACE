
upload_page(_) :-
   reply_html_page(biologic, 
        [ title('Upload File'),
	  style([type='text/css'],
	['body {position:absolute; top:50%; margin-left:20%}'])
	], 
	[
	 div( ['background-image'='url(/open/images/genericAA.png)'],
	      [center([h1('Upload File'),
	       form([ action='/secure/upload.pl',
		      method='post',
		      enctype='multipart/form-data'],
		  [input([type='file',size=40,name='datafile']),
		   input([type='text',size=40,name='myname',value='peter']),
		   input([type='submit',name='Submit',value='UpLoad File'])
		  ])])])
	]).


