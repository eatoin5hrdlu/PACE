#!/usr/bin/python -u
#!C:/Python27/python -u
import os, time, subprocess, re
import base64, urllib2
 
import numpy as np
import cv2
import cv2.cv as cv

#LagoonRegion = (300,0,480,260)
lagoonHeight = 80
Lagoon = {}
Levels = {}

configFile = "evo.settings"

#
# IPCamera knows about different IP cameras (as well as USB cams) and
# can find the IP address from a MAC address (requiring linux and superuser and time)
# This process will report the IP address which should be edited into evo.settings file
# along with with lagoon locations and other things which won't be changing.
#
# Fluorescence module does color integration (returns saturation cycle)
# Blob detection module ( find lagoon/cellstat coordinates)
# import ipcam, fluor, blob, level

import blob, level

class ipCamera(object):
    """Color can be Blue=0, Green=1, or Red=2 (openCV => BGR, not RGB)
       Image represents integration of [ selected color - 1/2(unselected colors) ]"""

    def __init__(self, location):
        global settings
        self.params = settings[location]
        self.camType = self.params['camera']
        self.defaultIP = self.params['defaultIP']
        self.usbcam = None
        if isinstance(self.params['MAC'],int) :
            print "MAC indicates that we are using a USB camera"
            self.usbcam = cv2.VideoCapture(self.params['MAC'])
            self.ip = None
        else :
            self.ip = self.ValidIP(self.params['MAC'])
            if (self.ip == None) :
                print self.params['MAC'], " is not a valid IP/MAC for a Camera"
                exit(1)
            self.url = "http://" + self.ip + self.params['picCmd']
            print "Using URL: " + self.url
            self.req = urllib2.Request(self.url)
        self.blobDet = blob.Blob(1)              # Create a blob detector for green(1) blobs
        self.levelDet = level.Level()          # Create a level (horizontal line) detector for monochrome images

    def contrast(self, level):
        if (self.ip != None) :
            self.cmdToCamera("http://" + self.ip + self.params['contrastCmd']+str(level)+self.params['userpwd'])

    def brightness(self, level):
        if (self.ip != None) :
            self.cmdToCamera("http://" + self.ip + self.params['brightnessCmd']+str(level)+self.params['userpwd'])

    def ValidIP(self,s):
        """Uses regular expressions for valid MAC and IP addresses and then
           calls arp-scan (must be on Linux and a superuser) to find IP from MAC"""
        ip = None
        part = '(2[0-4]|1[0-9]|[0-9])?[0-9]|25[0-5]'
        res =re.search(r'(^| )((%s)\.){3}(%s)' %(part,part), s,re.I )
        if res:
            print "Good IP ", res.group().strip()
            ip = res.group().strip()
        else:
            macres = re.search(r'([a-fA-F0-9]{2}[:|\-]?){6}', s,re.I )
            if macres:
                print "Finding IP from MAC ", macres.group().strip()
                ip = self.Mac2IP(macres.group().strip())
        return ip

    def Mac2IP(self, mac) :
        if os.name == 'nt' :
            return self.defaultIP
        if not os.geteuid() == 0 :
            print "Superuser required to find IP from MAC, using default IP."
            return(self.defaultIP)
#        cmd = 'arp-scan --interface=wlan0 --localnet | grep ' + mac
        cmd = 'arp-scan --interface=eth0 --localnet | grep ' + mac
        ret = subprocess.Popen(cmd,shell=True,stdout=subprocess.PIPE).stdout.readline()
        if (ret) :
            ipstr = ret.split()[0]
            print "Camera is at IP address: " + ipstr
            if not ipstr == self.defaultIP:
                print "please change defaultIP("+self.defaultIP+") in ipcam.py to ", ipstr
            return ipstr
        return None
 
    def grab(self):
        if (self.usbcam != None) :
            try :
                (rval, im1) = self.usbcam.read()
                if (rval) :
                    return im1
                else :
                    print "Usb camera read failed"
                    return None
            except :
                print " Failed to grab image from USB camera"
        else :
            try :
                return cv2.imdecode(np.asarray(bytearray(urllib2.urlopen(self.req).read()), dtype=np.uint8), 1)
            except urllib2.URLError, msg :
                print msg, " Failed to get image from camera at ", self.ip
        return None

    def lagoonImage(self):
        (x1,y1,x2,y2) = ipcam.params['LagoonRegion']
        image = self.grab()
        if (image == None) :
            print "no image from camera."
            exit()
        return image[x1:x2,y1:y2,:] # cropped for lagoons

    def cmdToCamera(self, cmd) :
        print "HTTP: " + cmd
        try:
            urllib2.urlopen(urllib2.Request(cmd))
        except urllib2.URLError, msg :
            print msg, " Failed sending command to camera at ", self.ip

    def showThisColor(color) :
        frame = ipcam.grab()
        picked = frame[:,:,color]  # Start with selected color image
        while True:
            temp =  ipcam.grab()
            halfothers = cv2.addWeighted(temp[:,:,(color+1)%3], 0.5, temp[:,:,(color+2)%3], 0.5, 0 )
#        picked=cv2.addWeighted(picked,0.9, cv2.subtract(temp[:,:,color],halfothers), 0.95, 0)
            picked=cv2.add(picked,cv2.subtract(temp[:,:,color],halfothers))
            if not frame == None :
                labelImage(picked,color)
                cv2.imshow("camera", picked)
            if cv.WaitKey(10) == 27:
                return

    def labelImage(self, img, color) :
        colors = {0:"blue", 1:"green", 2:"red" }
        cv2.putText(img,colors[color],(10,80),cv2.FONT_HERSHEY_PLAIN,4.0,(240,80,200),2)

    def updateLevels(self,pause=1000) :
        """Levels are a percentage of the lagoon height (specified at the top of this file)
        To use mL as our standard unit of liquid level, we should add scaling param to evo.settings"""
        global Lagoons
        goodRead = 0
        while (goodRead != 4) :
            goodRead = 0
            frame = self.lagoonImage()   # Grab a cropped image centerend on the lagoons
            for k in Lagoon.keys():
                bb = Lagoon[k]   # Bounding box relative to cropped 'lagoonImage'
                subi = frame[bb[1]:bb[1]+bb[3], bb[0]:bb[0]+bb[2],1]
                lvl = self.levelDet.level(subi)
                if (lvl == None or lvl == 1000) :
                    print "level detection failed"
                if (lvl > 0 and lvl < bb[3]) : # Level is in range
                    Levels[k] = (100 * (self.params['lagoonHeight']-lvl))/self.params['lagoonHeight']
                    cv2.line(frame,(bb[0],bb[1]+lvl),(bb[0]+bb[2],bb[1]+lvl), (0,0,255),1)
                    goodRead = goodRead + 1
                    self.drawLagoons(frame)
                    cv2.imshow("camera", frame)
                    if cv.WaitKey(pause) == 27:
                        exit()
                else :
                    print str(lvl) + " out of range :" + str(bb)
                    return None
        print "Levels " + str(Levels)
        return Levels

    def updateLagoons(self,pause=1000) :
        """Blob detection to locate Lagoons. Must be called before updateLevels()."""
        frame = self.lagoonImage()   # Grab a cropped image centerend on the lagoons
        print "Frame shape:" + str(frame.shape)
        bbs = self.blobDet.blobs(frame,pause)    # Find the green blobs
        sbbs = self.blobs2lagoons(bbs)     # Sort them left to right and interpret as lagoon rectangles
        if (len(sbbs) >= 4) :         # Check to see that we have a reasonable number of lagoons
            for i in range(4) :
                Lagoon['Lagoon'+str(i+1)] = sbbs[i]
                print 'Lagoon'+str(i+1) + "   " + str(sbbs[i])
        else :
            print "Need at least four bbs, but got " + str(len(sbbs))

    def blobs2lagoons(self,bbs) :
        """The bottom edges of identified blobs should line up.
        These are the actual bottoms of the lagoons.  The tops will vary
        because they represent the liquid levels, so we create a set of
        outlines to include maxiumum fill levels.  These are the only 
        regions of interest for our horizontal line (liquid level) detection"""
        sbbs = [b for a,b in sorted((tup[0], tup) for tup in bbs)]
        lagoons = []
        ln = 0
        for bb in sbbs :
            if len(lagoons) == 0 :
                lagoons.append(bb)
                ln = ln + 1
            else :
                pbb = lagoons[ln-1]
                if bb[0] > pbb[0]+(pbb[2]/2) :
                    lagoons.append(bb)
                    ln = ln + 1
        outlines = []
        for l in lagoons:
            outlines.append((l[0],l[1]-(self.params['lagoonHeight']-l[3]), l[2],self.params['lagoonHeight']))
        return outlines

    def drawLagoons(self, image, pause=200) :
        global Lagoon
        cler = [cv.Scalar(0,0,255,255),cv.Scalar(0,255,255,255),cv.Scalar(255,0,0,255),cv.Scalar(255,0,255,255)]
        i = 0
        for bb in Lagoon.values():
            cv2.rectangle(image,(bb[0],bb[1]),(bb[0]+bb[2],bb[1]+bb[3]),cler[i%4],1)
            cv2.circle(image,(bb[0],bb[1]),5,cler[(i+1)%4],2)
            i = i + 1
        cv2.imshow("camera", image)
        if cv.WaitKey(pause) == 27:
            return

    def bioBlobs(self, color, (x1,y1,x2,y2)) :
        """Bio-luminescence detection. Sum images until MAXFRAMES and note saturation points"""
        frame = None
        while(frame == None) :
            frame = ipcam.grab()
        picked = frame[:,:,color]  # Start with selected color image
        cycle = 0
        while True:
            cycle = cycle + 1
            temp = None
            tries = 0
            while (temp == None and tries < 10) :
                temp =  ipcam.grab()
                if (temp == None) :
                    print "Failed to get image from camera"
                    tries = tries + 1
                    time.sleep(1000)
            if (temp == None) :
                print "Giving up on camera connection"
                return None
                
            halfothers = cv2.addWeighted(temp[:,:,(color+1)%3], 0.5, temp[:,:,(color+2)%3], 0.5, 0 )
            picked=cv2.addWeighted(picked,0.9, cv2.subtract(temp[:,:,color],halfothers), 0.9, 0)
            sat = 0
            total = (x2-x1)*(y2-y1)
            lit = cv2.countNonZero(cv2.subtract(picked[y1:y2,x1:x2], 128))
            sat = cv2.countNonZero(cv2.subtract(picked[y1:y2,x1:x2], 250))
            print str(sat) + " saturated " + str(lit) + " detected out of " + str(total) + " at cycle " + str(cycle)
            cv2.rectangle(picked,(x1,y1),(x2,y2),255)
            if not frame == None :
                self.labelImage(picked,cler)
                cv2.imshow("camera", picked)
            if cv.WaitKey(10) == 27:
                return

# End of ipCamera Class

def write_settings():
    global settings
    f = open(configFile, 'w')
    f.write(str(settings))
    f.close()

def read_settings():
    global settings
    f = open(configFile, 'r')
    settings = eval(f.read())
    f.close()

def setupCamera() :
    cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
    if cv2.__dict__['moveWindow'] != None :
        cv2.moveWindow("camera", 100, 200)
    else :
        print "cv2 does not contain moveWindow. Update your OpenCV installation."
    cam = ipCamera('usb')
#   cam = ipCamera('museum')
    cam.brightness(cam.params['brightness'])
    cam.contrast(cam.params['contrast'])
    return cam

if __name__ == "__main__" :
    print "OpenCV version = " + str(cv2.__version__)
    read_settings()               # Read evo.settings
    ipcam = setupCamera()         # Initialize Camera
    retry = True
    while(retry) :
        retry = False
        ipcam.updateLagoons(4000) # blob contours shown for 4 seconds
        for i in range(20) :
            if ( ipcam.updateLevels(pause=10) == None) :
                print "Go back to blob detection and try again"
                retry = True
                break
    print "Final Levels: " + str(Levels)
    if cv.WaitKey(5000) == 27:
        exit()

#    ipcam.bioBlobs(2,lagoon_position['Lagoon1'])
#    ipcam.bioBlobs(1,lagoon_position['Lagoon2'])
#    ipcam.bioBlobs(1,lagoon_position['Lagoon3'])
#    ipcam.bioBlobs(0,lagoon_position['Lagoon4'])
#    bioBlobs(1)
#    showThisColor(0)
#    showThisColor(1)
#    showThisColor(2)

