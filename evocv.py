import os, sys, time, traceback
import numpy as np
import cv2
import cv2.cv as cv

class EvoCv(object):
    """Utilities for EvoStat visual feedback
    Status images for Web access:
           Normal lighting
           Meniscus lighting
           Dark (bio-luminescence)
       Liquid Level Detection:
           Blob detection,
              Single-color emphasis (default green),
              Camera Settings,
              Parameterized cycles of Contrast, Erode, Dilation
           Lagoon Outlines
           Horizontal line (liquid level) detection
       BioLuminescence:
           Multiple Image addition: Sum(Green - (Blue+Red)/2)
           Saturation detection"""

    def __init__(self, color=1, minsize=24, maxsize=160):
        """Optional color, and min/max dimension setting"""
	self.color = color   # default color of interest is Green
        self.maxWidth = 60   # All lagoon areas will be reduced to a strip this wide
	self.minDim = minsize
	self.maxDim = maxsize
        self.kernal = np.ones((5,5),np.uint8)
        self.alpha   = np.array([  1.2 ])
        self.beta    = np.array([ -60 ])
	self.theta = 1
	self.phi = 1
	self.maxIntensity = 255.0

    def erodeDilate(self,img,iter=1,erode=1,dilate=1) :
        for i in range(iter):
            img = cv2.erode(img,self.kernal,iterations=erode)
            img = cv2.dilate(img,self.kernal,iterations=dilate)
        return img
                
    def set_minsize(self, minsize) :
	self.minDim = minsize

    def set_maxsize(self, maxsize) :
	self.maxDim = maxsize

    def rotateImage(self, img, angle=90):
        """+-90 degree rotations are fast and dont crop"""
        if (angle == 90) :
            return(cv2.flip(cv2.transpose(img),flipCode=0))
        elif (angle == -90) :
            return(cv2.flip(cv2.transpose(img),flipCode=1))
        else :
            center = (img.shape[1]/2.0,img.shape[0]/2.0)
            rotate = cv2.getRotationMatrix2D(center, angle, 1.0)
            return cv2.warpAffine(img, rotate, (img.shape[1], img.shape[0]))

    def contrast(self, image, iter=1, scale=2.0, offset=-100) :
        if (image == None) :
            print "contrast called with null Image"
        for i in range(iter) :
            if (image == None) :
                print "contrast loop: Image is None"
            else :
                image = cv2.add(cv2.multiply(image,scale),offset)
        if (image == None) :
            print "image is None after add/mulitply in contrast!"
        self.showUser(image,4000)
        (ret,img) = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
        if (ret == False) :
            print "Thresholding failed?"
            return None
        if (img == None) :
            print "img is None after binary threshold in contrast"
        self.showUser(img,1000)
        # ret value is threshold (127.0) not True - False
        return img

    def emphasis(self, img, scale=2, fraction=0.5) :
        """Return monochrome image with doubled(scale) selected color
           minus half of the other two colors(/fraction) added together.
           Where color is Blue (0), Green (1-default), or Red (2)"""
        return cv2.subtract(cv2.multiply(img[:,:,self.color],scale),
                            cv2.multiply(cv2.add( img[:,:,(self.color+1)%3],
                                                  img[:,:,(self.color+2)%3]),fraction))
    
    def showUser(self, image, pause=0) :
        if (image != None) :
            if (pause != 0) :
                cv2.imshow("camera",image)
                if cv.WaitKey(pause) == 27:
                    exit()
        else :
            print "showUser called with null image (None)"
            traceback.print_stack()


    def level(self, img, pause=0) :
        """Return the uppermost horizontal line in the image (e.g. liquid level)
           Returns:   -1 when there is a problem with the data
                    1000 when there is no line within proper range"""
        if (img == None) :
            print "Level detector called with invalid image"
            return -1
        (h,w) = img.shape
        if (h == 0 or w == 0) :
            print "Level called with degenerate image SHAPE" + str(img.shape)
            return -1
        img = self.contrast(img)
        if (img==None) :
            print "Contrast returned None  (shape =" + str(img.shape)
            return -1
        edges = cv2.Canny(img, 90, 100)
        if (edges == None) :
            print "Bad Canny output so not calling HoughLinesP in level()"
            return -1
        alllines = cv2.HoughLinesP(edges, 2, 3.1415926/2.0, 1, 16, 4)
        if (alllines == None) :
            print "No horizontal lines found in image"
            return -1
        topline = 1000
        for lines in alllines:
            for l in lines : # Find the highest (minY) line (not on the edge)
                if (l[1] == l[3]) : # Horizontal?
                    if ( l[1] < topline and l[1] > 5 and l[1] < h-5) :
                        topline = l[1]
        return topline

    def blobs(self, img, pause=0) :
        """IP cameras like 2X(Erode->Dilate->Dilate) erodeDilate(img,2,1,2)
           USB camera likes single erode->dilate cycle erodeDilate(img,1,1,1)
           TODO: Automate variation of these parameters to get a good reading"""
        debug = ''
	emp = self.emphasis(img)
        self.showUser(emp,pause)
        con = self.contrast(emp,iter=1)
        self.showUser(con,pause)
	gray = self.erodeDilate(con, 1, 1, 1)
	gray2 = cv2.adaptiveThreshold(gray,255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C,cv2.THRESH_BINARY,11,2)
        self.showUser(gray2,pause)
        contours, _ = cv2.findContours(gray2, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)
	if (pause != 0) :
            debug = debug + str(len(contours)) + " contours ( "
        toosmall = 0
        toolarge = 0
	bbs = []
	for c in contours:
		rect = cv2.boundingRect(c)
		if rect[2] < self.minDim or rect[3] < self.minDim:
                    toosmall += 1
                    continue
		elif    rect[2]>self.maxDim or rect[3]>self.maxDim or rect[2]<self.minDim or rect[3]<self.minDim:
                    toolarge += 1
                    continue
		else :
                    if (rect[2] > self.maxWidth) :  # Limit width and center
                        margin = (rect[2]-self.maxWidth)/2
                        debug = debug + "WIDTH/CENTER ADJUSTED " + str(rect) + "\n"
                        bbs.append((margin+rect[0],rect[1],self.maxWidth,rect[3]))
                    else :
                        debug = debug + "SIZE OKAY   " + str(rect) + "\n"
                        bbs.append(rect)

        if (pause != 0) :
            debug = debug + ") "+str(toosmall)+" too small "+str(toolarge)+" too large\n"
            pen = (255,255,255) # White
            for r in bbs:
                cv2.rectangle(img,(r[0],r[1]),(r[0]+r[2],r[1]+r[3]),pen,2)
            self.showUser(img,pause)
        print ">>>>>>blobs>>>>>>>\n" + debug + ">>>>>" + str(len(bbs)) + ">>>>>>>>>>>>>"
	return bbs


