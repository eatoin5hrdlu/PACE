#!/cygdrive/c/Python27/python
#
# observe: Return (display) observed liquid levels in
#          Turbidostat and four Lagoons
#
# API:
#	initialize(FILE,CAM) - FILE: coordinate file  CAM: index(0..N)
#	getLevelString()     - returns a string of level percentages
#	getLevels()          - returns a dictionary of levels
#
# show = True to display a live window of the processed image
show = True
#import win32gui, win32api, win32con

def video(on_off) : # video(False) for non-interactive mode
	global show
	show = on_off

import cv2, cv, time
import numpy as np

# Optional image processing before feature extraction

alpha   = np.array([  2.1 ])
beta    = np.array([ -120 ])

def contrast(image) :
	return cv2.add(cv2.multiply(image,alpha),beta)

element = cv2.getStructuringElement(cv2.MORPH_RECT,(2,2))

cntr = 0  # Debugging

# Logitech Camera
# Y Parameters are: 1 floor of main vessel
#                   1 max level of turbidostat
#                   1 floor of lagoons
#                   1 max level of lagoons
# X Parameters are: 5 centers of vessels
#

# Global Database (Dictionary of dictionaries)
# for bottle coordinates, camera info (light levels,contrast), etc.
gdb = {}  
def load(name, file, default_dict) :
	try:
		gdb[name] = eval(open(file).read())
	except:
		print file + " not found: Using default coordinates"
		gdb[name] = default_dict

# Callback function for findWindowHandle
#def windowEnumerationHandler(hwnd, resultList):
#	resultList.append((hwnd,win32gui.GetWindowText(hwnd)))

#def findWindowHandle(string):
#	topWindows = []
#	win32gui.EnumWindows(windowEnumerationHandler,topWindows)
#	for window in topWindows:
#		if string in window[1]: return window[0],window[1]
#	return None, None

def savebottles(f) :
	bfile = open(f,'w')
	for k in gdb['bottles'].keys() :
		gdb['bottles'][k][2] = []
	print >> bfile,  gdb['bottles']
	bfile.close()

def slide(jar, x, y) :
	gdb['bottles'][jar][0][0] = gdb['bottles'][jar][0][0] + x
	gdb['bottles'][jar][1][0] = gdb['bottles'][jar][1][0] + x
	gdb['bottles'][jar][0][1] = gdb['bottles'][jar][0][1] + y
	gdb['bottles'][jar][1][1] = gdb['bottles'][jar][1][1] + y

def dimension(jar, x, y) :
	gdb['bottles'][jar][0][0] = gdb['bottles'][jar][0][0] + x
	gdb['bottles'][jar][0][1] = gdb['bottles'][jar][0][1] + y
	
def initialize(file, camera) :
	global show
	global capture
	print "Show is equal to " + str(show)
	# A warning is printed when default coordinates are used
	load('bottles', file, { # Default coordinates
		'Turbidostat' : [[190,210], [380,260], []],
		'Lagoon    4' : [[290,10],  [480,50], []],
		'Lagoon    3' : [[290,100],  [470,140], []],
		'Lagoon    2' : [[280,280],  [470,320], []],
		'Lagoon    1' : [[290,410], [480,450],[]] })
	if (show) :
		cv2.namedWindow("PACE Level Monitor", cv.CV_WINDOW_NORMAL)
		#(hdl, text) = findWindowHandle("PACE")
	        #win32gui.SetWindowPos(hdl,win32con.HWND_TOPMOST,0,0,640,480,0)
	else :
		print "Did not create image window"
	capture = cv2.VideoCapture(camera)
	capture.set( cv.CV_CAP_PROP_FRAME_WIDTH, 640 );
	capture.set( cv.CV_CAP_PROP_FRAME_HEIGHT, 480 );

def draw_bottle(image, b) :
	color = (128,80,200) # Purple
	cv2.line(image,(b[0][0],b[0][1]),(b[1][0],b[0][1]),color,3)
	cv2.line(image,(b[1][0],b[1][1]),(b[1][0],b[0][1]),color,3)
	cv2.line(image,(b[1][0],b[1][1]),(b[0][0],b[1][1]),color,3)

def in_bottle(line, bottle) :
	if  (  (line[0] > bottle[0][0] and line[0] < bottle[1][0])
	   and (line[2] > bottle[0][0] and line[2] < bottle[1][0])
	   and (line[1] > bottle[0][1] and line[1] < bottle[1][1])
	   and (line[3] > bottle[0][1] and line[1] < bottle[1][1]) ):
		return True
	return False

def level(img, bottle) :
	x = 1000
	topline = None
	for l in bottle[2] :  # Find the highest line in the bottle
		if ( l[0] < x ) :
			x = l[0]
			topline = l
	if (topline != None) :
		maxx = max(topline[0],topline[2])
		cv2.line(img,(topline[0],topline[1]),(topline[2],topline[3]),(0,0,255),2)
		return float(maxx-bottle[1][0])/float(bottle[0][0]-bottle[1][0])
	return 0.0
	
average = {'Turbidostat' : 4.5,
	   'Lagoon    4' : 4.5,
	   'Lagoon    3' : 4.5,
	   'Lagoon    2' : 4.5,
	   'Lagoon    1' : 4.5 }

def find_levels(img, bottles, lines) :
	global cntr
	result = ''
	for b in sorted(bottles.keys()) :
		for l in lines :
			if (in_bottle(l,bottles[b])) :
				bottles[b][2].append(l)
		lev = level(img, bottles[b])
		if ( lev > 0.0 ) :
			lev = (average[b] + lev)/10.0
			average[b] = lev * 9.0
		else :
			lev = average[b]/9.0
		result += str(b) + ': ' + str(int(100*lev)) + '%  '
	return result

def draw_bottles(image, bottles) :
	for k in bottles.keys():
		draw_bottle(image, bottles[k])

def rotateImage(img, angle):
	center = (img.shape[1]/2.0,img.shape[0]/2.0)
	rotate = cv2.getRotationMatrix2D(center, angle, 1.0)
	return cv2.warpAffine(img, rotate, (img.shape[1], img.shape[0]))

def putText(img, text, pos):
	fontScale = 1
	font = cv2.FONT_HERSHEY_PLAIN
	
	cv2.putText(img, text, pos, font, fontScale, (0,255,0))

def getLevelString() :
	global show
	global cntr
	global capture
	result = None
	cntr = cntr + 1
	(retval, orig) = capture.read()
	img = rotateImage(orig, 90)
	gray = cv2.cvtColor(img, cv.CV_RGB2GRAY)
#	edges = cv2.Canny(gray, 80, 120)
	edges = cv2.Canny(gray, 90, 100)
#	lines = cv2.HoughLinesP(edges, 10, 3.1415926, 1, None, 12, 1);
	lines = cv2.HoughLinesP(edges, 2, 3.1415926, 1, None, 16, 4);

	draw_bottles(img, gdb['bottles'])


	for jar in gdb['bottles']:
		x = gdb['bottles'][jar][0][0]
		y = gdb['bottles'][jar][0][1]
		putText(img, jar, (x,y))

	for b in gdb['bottles'].keys():
		gdb['bottles'][b][2] = []
	if (lines != None) :
		result = find_levels(img, gdb['bottles'], lines[0])
	if (show) :
		final = rotateImage(img, -90)		

		cv2.imshow("PACE Level Monitor", final)
	k = cv2.waitKey(10)
	if (k == 102 or k == 70): # 'F' or 'f'
		exit()
	return result

def calibrate():
	pass

def getFlowRate(input_pump, output_pump):
	"""
	input:
	pump -- int
	"""
	# turn off input pump
	
	# turn on lines
	pass
