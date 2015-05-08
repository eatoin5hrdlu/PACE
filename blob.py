#!C:\Python27\python -u
import numpy as np
import cv2
import cv2.cv as cv

class Blob(object):
    """Find blobs of a particular color (Blue=0,Green=1,Red=2) (openCV => BGR)
       returns list of bounding rectangles which should be the lagoons"""
 
    def __init__(self, color, minsize=24, maxsize=160):
	self.color = color
	self.minDim = minsize
	self.maxDim = maxsize
	self.theta = 1
	self.phi = 1
	self.maxIntensity = 255.0

    def erodeDilate(self,img,iter=1,erode=1,dilate=1) :
        ker = np.ones((5,5),np.uint8)
        for i in range(iter):
            img = cv2.erode(img,ker,iterations=erode)
            img = cv2.dilate(img,ker,iterations=dilate)
        return img
                
    def set_minsize(self, minsize) :
	self.minDim = minsize

    def set_maxsize(self, maxsize) :
	self.maxDim = maxsize

    def contrast(self, image, iter=1, scale=2.0, offset=-100) :
        for i in range(iter) :
            img = cv2.add(cv2.multiply(image,scale),offset)
        (ret,img) = cv2.threshold(img, 127,255,cv2.THRESH_BINARY)
        return img

    def emphasis(self, img, color) :
        """Create a monochrome image by doubling the selected color
           and subtracting half of the other two colors added together.
           Where color is Blue (0), Green (1), or Red (2)"""
        return cv2.subtract(cv2.multiply(img[:,:,color],2),
                            cv2.multiply(cv2.add( img[:,:,(color+1)%3]/3,
                                                  img[:,:,(color+2)%3]/3),0.5))

    def lagoonWidth(bb) :
        return (bb[0], bb[1], bb[2], bb[3])

    def blobs(self, img, pause=1000) :
        """IP cameras like 2X(Erode->Dilate->Dilate) erodeDilate(img,2,1,2)
           USB camera likes single erode->dilate cycle erodeDilate(img,1,1,1)"""
        print str(self.color) + " should be green 1"
	emp = self.emphasis(img, self.color)
        cv2.imshow("camera",emp)
        if cv.WaitKey(pause) == 27:
                exit()
        con = self.contrast(emp,iter=1)
#        con = emp
        cv2.imshow("camera",con)
        if cv.WaitKey(pause) == 27:
                exit()
	gray = self.erodeDilate(con, 1, 1, 1)
#	gray = self.erodeDilate(self.emphasis(img, self.color), 3, 1, 3)
#	gray = self.erodeDilate(self.emphasis(img, self.color))

# Some things used in the past for different light conditions
#       cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
#	gray = (maxIntensity/phi)*(gray/(maxIntensity/theta))**0.5
#	gray = cv2.blur(gray, (16,16))
	gray2 = cv2.adaptiveThreshold(gray,255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C,cv2.THRESH_BINARY,11,2)
        cv2.imshow("camera", gray2)
        if cv.WaitKey(pause) == 27:
                exit()
	contours, _ = cv2.findContours(gray2, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)
	print str(len(contours)) + " contours ( ",
	toosmall = 0
	toolarge = 0
	bbs = []
	for c in contours:
		rect = cv2.boundingRect(c)
		if rect[2] < self.minDim or rect[3] < self.minDim:
                    toosmall += 1
#                    contours.remove(c)
                    continue
		elif rect[2] > self.maxDim or rect[3] > self.maxDim or rect[2] < self.minDim or rect[3] < self.minDim:
                    toolarge += 1
#                    contours.remove(c)
                    continue
		else :
                    if (rect[2] > 50) :
                        bbs.append((rect[0],rect[1],50,rect[3]))
                    else :
                        bbs.append(rect)

	print ") " + str(toosmall) +  " too small " + str(toolarge) + " too large"
# Show the selected bounding boxes in white
#	pen = (255,255,255) # White
#	for r in bbs:
#		cv2.rectangle(img,(r[0],r[1]),(r[0]+r[2],r[1]+r[3]),pen,2)
	return bbs


