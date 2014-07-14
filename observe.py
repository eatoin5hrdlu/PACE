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
gray = None
edges = None
gimg = None
orig = None
fluor = None
rotated = None
rotation = None
turbidity = 10000
# import resource
import gc
from os  import popen
import os

#import win32gui, win32api, win32con

def video(on_off) : # video(False) for non-interactive mode
	global show
	show = on_off

import cv2, time
import numpy as np

# Optional image processing before feature extraction

alpha   = np.array([  1.2 ])
beta    = np.array([ -60 ])

def contrast(image) :
	(ret,img) = cv2.threshold(
	    cv2.add(cv2.multiply(
		cv2.add(cv2.multiply(
			cv2.add(cv2.multiply(image,2.0),-60)
				,2.0),-60)
					,2.1),-100), 127,255,cv2.THRESH_BINARY)
	return img

lement = cv2.getStructuringElement(cv2.MORPH_RECT,(2,2))

cntr = 0  # Debugging
imgcntr = 0  # Debugging
snaptime = 0

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
def windowEnumerationHandler(hwnd, resultList):
	resultList.append((hwnd,win32gui.GetWindowText(hwnd)))

def bbye() :
	global capture
	del(capture)

def findWindowHandle(string):
	topWindows = []
	win32gui.EnumWindows(windowEnumerationHandler,topWindows)
	for window in topWindows:
		if string in window[1]: return window[0],window[1]
	return None, None

def savebottles(f) :
	bfile = open(f,'w')
	for k in gdb['bottles'].keys() :
		gdb['bottles'][k][2] = []
#	print >> bfile,  gdb['bottles']
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
	global imgcntr
	with popen('ls -1 /home/peter/srclab/PACE/images') as pipe :
		imgcntr = 0
		for f in pipe :
			imgcntr = imgcntr + 1
		print str(imgcntr) + " Images"
#	print "Show is equal to " + str(show)
	# A warning is printed when default coordinates are used
	load('bottles', file, { # Default coordinates
		'Turbidostat' : [[190,210], [380,260], []],
		'Lagoon    4' : [[290,10],  [480,50], []],
		'Lagoon    3' : [[290,100],  [470,140], []],
		'Lagoon    2' : [[280,280],  [470,320], []],
		'Lagoon    1' : [[290,410], [480,450],[]] })
	if (show) :
		cv2.namedWindow("PACE Level Monitor", cv2.WINDOW_NORMAL)
		#(hdl, text) = findWindowHandle("PACE")
	        #win32gui.SetWindowPos(hdl,win32con.HWND_TOPMOST,0,0,640,480,0)
	else :
		print "Did not create image window"
	capture = cv2.VideoCapture(camera)
	if (False) : #hasattr(cv2, CAP_PROP_FRAME_WIDTH)) :
		capture.set( cv2.CAP_PROP_FRAME_WIDTH, 640 )
		capture.set( cv2.CAP_PROP_FRAME_HEIGHT, 480 )
	else :
		capture.set( cv2.cv.CV_CAP_PROP_FRAME_WIDTH, 640 )
		capture.set( cv2.cv.CV_CAP_PROP_FRAME_HEIGHT, 480 )

def draw_bottle(image, b) :
	color = (128,80,200) # Purple
#	print b
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
	
b2i = {	'Turbidostat' : 0,
	'Lagoon    4' : 4,
	'Lagoon    3' : 3,
	'Lagoon    2' : 2,
	'Lagoon    1' : 1 }

def find_levels(img, bottles, lines) :
	global cntr
	result = 'levels([ '
	for b in bottles.keys() :
		for l in lines :
			if (in_bottle(l,bottles[b])) :
				bottles[b][2].append(l)
		lev = level(img, bottles[b])
		if ( lev > 0.0 ) :

			result += "level(" + str(b2i[b]) + ", " + str(int(100*lev)) + "),"
	term = result[:-1] + "])."
	return term

def draw_bottles(image, bottles) :
	for k in bottles.keys():
		draw_bottle(image, bottles[k])

def rotateImage(img, angle):
	global rotation
	global rotated
	center = (img.shape[1]/2.0,img.shape[0]/2.0)
	rotate = cv2.getRotationMatrix2D(center, angle, 1.0)
	rotated = cv2.warpAffine(img, rotate, (img.shape[1], img.shape[0]))
	del rotate
	return rotated


def getLevelString() :
	global show
	global cntr
	global imgcntr
	global capture
	global snaptime
	global gray
	global edges
	global gimg
        global orig
        global fluor
	global turbidity
	result = None
	cntr = cntr + 1
# 55-58 50
# 59-62 80
# 63-66 120
#        print 'getLevelString() memory: %s (kb)' % resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
	(retval, orig) = capture.read()   # MEMORY LEAK (600)
	if (cntr % 120 == 1) :
		if (fluor != None) :
			print str(imgcntr) + " Images"
			basefile = 'C:/cygwin/home/peter/srclab/PACE/images/baseline.jpg'
			if (os.path.exists(basefile)) :
				baseline = cv2.split(cv2.imread(basefile))[1]
				cv2.imwrite("C:/cygwin/home/peter/srclab/PACE/images/fl_"+str(imgcntr)+".jpg", cv2.subtract(fluor,baseline))
			else :
				cv2.imwrite(basefile, fluor)
				print "Created a baseline heat image"
#			exit()
#		fluor = cv2.split(orig)[1] # Get the green
		(bl, gr, fluor) = cv2.split(orig)
#		fluor = cv2.subtract(rd/10,cv2.add(bl/10,gr/10))
#	else :
#		(bl, gr, rd) = cv2.split(orig)
#		fluor = contrast(cv2.subtract(rd,cv2.add(bl/3,gr/3)))
#		gray = fluor.copy()
#		contours, _ = cv2.findContours(gray,cv2.RETR_LIST,cv2.CHAIN_APPROX_SIMPLE)
#		maxblob = None
#		blob = -1
#		for c in contours:
#			rect = cv2.boundingRect(c)
#			if rect[2] < 5 or rect[3] < 5: continue
#			if (cv2.contourArea(c) > blob) :
#				blob = cv2.contourArea(c)
#				maxblob = c
#		turbidity = turbidity + blob - (turbidity/10)
#		(_,_,w,h) = cv2.boundingRect(maxblob)
#		print "Turbidity "+str((int)(turbidity/100))+" ("+str(w)+", "+str(h)+")"

#        print '  capture: %s (kb)' % resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
	gimg = rotateImage(orig, 90)    # MEMORY LEAK  (800)
	del orig
	del retval
#        print '  rotate: %s (kb)' % resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
	gray = cv2.cvtColor(gimg, cv2.COLOR_RGB2GRAY)
#        print '  cvt: %s (kb)' % resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
#	edges = cv2.Canny(gray, 80, 120)
	edges = cv2.Canny(gray, 90, 100) # MEMORY LEAK (2300)
	del gray
#        print '  canny: %s (kb)' % resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
#	lines = cv2.HoughinesP(edges, 10, 3.1415926, 1, None, 12, 1);
	lines = cv2.HoughLinesP(edges, 2, 3.1415926, 1, None, 16, 4);
	del edges

	draw_bottles(gimg, gdb['bottles'])
	for b in gdb['bottles'].keys():
		gdb['bottles'][b][2] = []
	if (lines != None) :
		result = find_levels(gimg, gdb['bottles'], lines[0])
	if (show) :
		final = rotateImage(gimg, -90)
		cv2.imshow("PACE Level Monitor", final)
		k = cv2.waitKey(1)
		if (k == 102 or k == 70): # 'F' or 'f'
			exit()
	del gimg
# Save web image if all five levels register and 10 seconds have elapsed
	if (result and len(result)>63 and time.time() > snaptime + 10) :
		cv2.putText(final,time.strftime("%d %b %Y"),(0,20),cv2.FONT_HERSHEY_PLAIN,0.8,(240,80,200),2)
		cv2.putText(final,time.strftime("%H:%M:%S"),(2,40),cv2.FONT_HERSHEY_PLAIN,1.1,(240,80,200),2)
		cv2.imwrite("/home/peter/src/PACE/open/level.jpg", final)
		snaptime = time.time()
	k = cv2.waitKey(50)
	if (k == 102 or k == 70): # 'F' or 'f'
		exit()
#        print '   finished: %s (kb)' % resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
	gc.collect()
        del lines
	del final
#        print '     collected: %s (kb)' % resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
	return result


def getTurbidity() :
	global turbidity
	(retval, orig) = capture.read()
	(bl, gr, rd) = cv2.split(orig)
	fluor = contrast(cv2.subtract(rd,cv2.add(bl/3,gr/3)))
	gray = fluor.copy()
	contours, _ = cv2.findContours(gray,cv2.RETR_LIST,cv2.CHAIN_APPROX_SIMPLE)
	maxblob = None
	blob = -1
	for c in contours:
		rect = cv2.boundingRect(c)
		if rect[2] < 5 or rect[3] < 5: continue
		if (cv2.contourArea(c) > blob) :
			blob = cv2.contourArea(c)
			maxblob = c
	turbidity = turbidity + blob - (turbidity/10)
	(_,_,w,h) = cv2.boundingRect(maxblob)
	if (show) :
		final = rotateImage(fluor, -90)
		cv2.imshow("PACE Level Monitor", final)
		k = cv2.waitKey(1)
		if (k == 102 or k == 70): # 'F' or 'f'
			exit()
	return tuple([(int)(turbidity/100), w, h])
