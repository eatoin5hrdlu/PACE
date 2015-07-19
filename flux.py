#!/Python27/python
import sys, time, cv2, gc
from os  import popen
import cv2
import numpy as np

capture = None
gray = None
orig = None
fluor = None
rotated = None
rotation = None

cntr = 0  # Debugging
imgcntr = 0  # Debugging
snaptime = 0

basefile = './baseline.jpg'
fluxfile = './flux

gdb = {}

def load(name, file, default_dict) :
	try:
		gdb[name] = eval(open(file).read())
	except:
		print file + " not found: Using default coordinates"
		gdb[name] = default_dict

def bbye() :
	global capture
	del(capture)

def saveflux(f) :
	bfile = open(f,'w')
`>	for k in gdb['bottles'].keys() :
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
	load(layout, "layout", { 'cellstat' : (10,10,100,100),
	                         'lagoon1' :  (10,10,200,200),
	                         'lagoon2' :  (10,10,200,200),
	                         'lagoon3' :  (10,10,200,200),
	                         'lagoon4' :  (10,10,200,200) })
        
	load(flux, "layout", {   'cellstat' : -1,
	                         'lagoon1' :  -1,
	                         'lagoon2' :  -1,
                                 'lagoon3' :  -1,
	                         'lagoon4' :  -1) }

	capture = cv2.VideoCapture(camera)

	if (False) : #hasattr(cv2, CAP_PROP_FRAME_WIDTH)) :
		capture.set( cv2.CAP_PROP_FRAME_WIDTH, 640 )
		capture.set( cv2.CAP_PROP_FRAME_HEIGHT, 480 )
	else :
		capture.set( cv2.cv.CV_CAP_PROP_FRAME_WIDTH, 640 )
		capture.set( cv2.cv.CV_CAP_PROP_FRAME_HEIGHT, 480 )


def getLevelString(frames) :
	global show
	global imgcntr
	global cntr
	global capture
	global snaptime
	global gray
	global edges
	global gimg
        global orig
        global fluor
	global turbidity
	result = None
	cntr = 0
	if (os.path.exists(basefile)) :
	    baseline = cv2.split(cv2.imread(basefile))[1]
        else :
            print "Run baseline to create dark heat image file"

        (retval, orig) = capture.read() 
        fluor = orig[:,:,1]               # FIRST GREEN IMAGE
        while(cntr < frames) :
            (retval, orig) = capture.read()
	    (bl, gr, rd) = cv2.split(orig)
	    fluor = cv2.add(fluor, cv2.subtract(gr,cv2.add(bl/3,rd/3)))

        if (argv[0] == 'base.py') :
                  cv2.imwrite(basefile, fluor)
        else :
                  fluor = cv2.subtract(fluor,baseline)

        ffile = open(fluxfile,'w')
        for k in region.keys() :
            (x1,y1,x2,y2) = region[k]
            cv.SetImageROI(fluor,region[k])
            print k + "(" + str(cv2.mean(fluor)) + ")."
	close(ffile)
        
