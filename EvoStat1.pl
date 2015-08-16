% Edit this file to change appearance and EvoStat configuration.
% Number of Lagoons is also in <hostname>.settings
% Maybe the main process consulting this fill can create <hostname>.settings
% E.g.: the number of lagoons and their shapes are here.
%

screen('Evostat1', 680, 900, point(50,0)).

'Evostat1'([
 cellstat(cellstat,  below, [ od(0.4), temp(37.0), shape(240,60),font(font(times,roman,18))]),
% pumps( pumprail, next_row,   [  mac('98:D3:31:70:2B:70')]),
 pumps( pumprail, next_row,   [  ]),
 spacer(        x1, next_row, [color(blue)]),
 snapshot(     cam, next_row, [ shape(650,420),image('mypic1.jpg')]),
 spacer(        x2, next_row, []),
 lagoon( lagoon1, next_row, [temp(35.0), LS, LF]),
 lagoon( lagoon2, right,    [temp(35.0), LS, LF]),
 lagoon( lagoon3, right,    [temp(35.0), LS, LF]),
 lagoon( lagoon4, right,    [temp(35.0), LS, LF]),
 spacer(        x3, next_row, [color(darkgreen)]),
 sampler(autosampler, next_row, [ shape(400,30),font(font(times,roman,20)) ])
]) :-
 LS = shape(142,60),
 LF = font(font(times,roman,14)).
