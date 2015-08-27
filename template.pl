% The hostname is the default
% pumps( pumprail, next_row,   [  mac('98:D3:31:70:2B:70')]),
%   [ mac('98:D3:31:70:2B:70'), temp(37.9), od(0.4), shape(200,80)]),
%   [ mac('98:D3:31:70:2B:70'), temp(37.9), od(0.4), shape(60,30)]),
%   [ mac('98:D3:31:70:2B:70'), temp(37.9), od(0.4), shape(60,30)]),
%   [ mac('98:D3:31:40:1D:A4') ])
%]).

config( [
	 numLagoons(4),
	 lagoonRegion(290,240,380,550),  % Area containing lagoons
	 lagoonHeight(60),    % divisor for levelScale
	 lagoonWidth(20),
	 levelScale(100),   % 100 gives level as percentage of lagoonHeight
	 camera(indoor),
	 rotate(false),
	 mac('c4:d6:55:34:8d:07'),  % belongs in snapshot
	 defaultIP('172.16.3.136'),  % belongs in snapshot
	 userpwd('&user=scrapsec&pwd=lakewould'),
	 brightness(100), % 0-240 for indoor camera
	 brightnessCmd('/camera_control.cgi?param=1&value='),
	 contrast(4),
	 contrastCmd('/camera_control.cgi?param=2&value='),
	 picCmd('/snapshot.cgi?resolution=32&user=admin&pwd=lakewould'),
	 screen(680, 840, point(750,0)),
	 layout([
		 cellstat(cellstat,below,[od(0.4),temp(37.0), shape(240,60),CF]),
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
		 sampler(autosampler, next_row, [ shape(400,30),SF])
                ])
	 ]) :-
 LS = shape(142,60),
 LF = font(font(times,roman,14)),
 CF = font(font(times,roman,18)),
 SF = font(font(times,roman,20)).
