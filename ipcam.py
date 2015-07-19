#!C:/cygwin/Python27/python -u
#!C:/Python27/python -u
#!/usr/bin/python -u
import sys, os, time, subprocess, re
import base64, urllib2
from os  import popen

import numpy as np
import cv2
import cv2.cv as cv
import evocv

gdb = { 'layout' : None  }

Lagoon = {}
Levels = {}

#
# IPCamera knows about different IP cameras (as well as USB cams) and
# can find the IP address from a MAC address (requiring linux and superuser and time)
# This process will report the IP address which should be edited into evo.settings file
# along with with lagoon locations and other things which won't be changing.
#
# Fluorescence module does color integration (returns saturation cycle)
# Blob detection module ( find lagoon/cellstat coordinates)
# import ipcam, fluor, blob, level

class ipCamera(object):
    """USB, Wired Ethernet or WiFi IP camera discovery and control.
       This object contain the configuration information and knows about
       lagoon dimensions, lighting, angle of camera, etc."""

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
        self.evocv  = evocv.EvoCv(1,30,300)  # Detection of green(1) blobs 30-300 pixels wide

    def rotateImage(self, img, angle):
        return(cv2.flip(cv2.transpose(img),flipCode=0))

#        cv.Transpose(img,timg)
#cv.SaveImage("rotated_clockwise.jpg", timg)
#        center = (img.shape[1]/2.0,img.shape[0]/2.0)
#        rotate = cv2.getRotationMatrix2D(center, angle, 1.0)
#        rotated = cv2.warpAffine(img, rotate, (img.shape[1], img.shape[0]))
#        return rotated

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
                img1 = urllib2.urlopen(self.req).read()
                if (img1 == None) :
                    print "urlopen->read failed"
                img1 = bytearray(img1)
                if (img1 == None) :
                    print "bytearray failed"
                img1 = np.asarray(img1, dtype=np.uint8)
                if (img1 == None) :
                    print "numpy conversion failed"
            except urllib2.URLError, msg :
                print msg, " Failed to get image from camera at ", self.ip

            if (self.params['rotate']) :
                return self.rotateImage(cv2.imdecode(img1,1), self.params['rotate'])
            else :
                return(cv2.imdecode(img1, 1))
        return None

    def lagoonImage(self):
        (x1,y1,x2,y2) = ipcam.params['LagoonRegion']
        image = self.grab()
        if (image == None) :
            print "no image from camera."
            exit()
        return image[x1:x2,y1:y2,:] # cropped for lagoons
#        return image

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
                if (picked != None) :
                    cv2.imshow("camera", picked)
                    if cv.WaitKey(10) == 27:
                        return
                else :
                    print "picked slice was None"

    def labelImage(self, img, color) :
        colors = {0:"blue", 1:"green", 2:"red" }
        cv2.putText(img,colors[color],(10,80),cv2.FONT_HERSHEY_PLAIN,4.0,(240,80,200),2)

    def updateLevels(self,pause=100) :
        """Levels are a percentage of the lagoon height (specified at the top of this file)
        To use mL as our standard unit of liquid level, we should add scaling param to evo.settings"""
        global Lagoon
        goodRead = 0
        while (goodRead != 4) :
            goodRead = 0
            frame = self.lagoonImage()   # Grab a cropped image centerend on the lagoons
            for k in Lagoon.keys():
                bb = Lagoon[k]   # Bounding box relative to cropped 'lagoonImage'
                subi = frame[bb[1]:bb[1]+bb[3], bb[0]:bb[0]+bb[2],1]
                print k + "   SHAPE " + str(frame.shape)
                print bb
                print "SHAPE " + str(subi.shape)
                lvl = self.evocv.level(subi)
                if (lvl == None or lvl == 1000) :
                    print "level detection failed"
                if (lvl > 0 and lvl < bb[3]) : # Level is in range
                    Levels[k] = (100 * (self.params['lagoonHeight']-lvl))/self.params['lagoonHeight']
                    cv2.line(frame,(bb[0],bb[1]+lvl),(bb[0]+bb[2],bb[1]+lvl), (0,0,255),1)
                    goodRead = goodRead + 1
                    self.drawLagoons(frame)
                    if (frame != None) :
                        cv2.imshow("camera", frame)
                        if cv.WaitKey(pause) == 27:
                            exit()
                    else :
                          print "frame was None after drawLagoons!?"  
                else :
                    print str(lvl) + " out of range :" + str(bb)
                    return None
            if (goodRead != 4) :
                print str(goodRead) + " good level reads"
        print "Levels " + str(Levels)
        return Levels

    def updateLagoons(self,pause=100) :
        """Blob detection to locate Lagoons. Must be called before updateLevels()."""
        numblobs = 0
        needed = ipcam.params['NumLagoons']
        while (numblobs < needed) :
            frame = self.lagoonImage()   # Grab a cropped image centerend on the lagoons
            cv2.imwrite('mypic.jpg', frame)
            print "Frame shape:" + str(frame.shape)
            bbs = self.evocv.blobs(frame,pause)    # Find the green blobs
            sbbs = self.blobs2lagoons(bbs)     # Sort left to right interpret as lagoon rect
            numblobs = len(sbbs)
            print "blobs2lagoons returned " +str(numblobs)
            if (numblobs >= needed) :   # Check for the minimum number of lagoon outlines
                for i in range(needed) :
                    Lagoon['Lagoon'+str(i+1)] = sbbs[i]
                    print 'Lagoon'+str(i+1) + "   " + str(sbbs[i])
            else :
                print "Need at least " + str(needed) + " bbs, but got " + str(len(sbbs))
                for bb in sbbs:
                    cv2.rectangle(frame,(bb[0],bb[1]),(bb[0]+bb[2],bb[1]+bb[3]),(0,0,255),2)
                if (frame != None) :
                    cv2.imshow("camera",frame)
                    if cv.WaitKey(pause) == 27:
                        exit()
                else :
                    print "frame was None after drawing bbs"
                    
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
                if bb[0] > (pbb[0]+pbb[2]) :
                    lagoons.append(bb)
                    ln = ln + 1
                else:
                    print str(bb) + " not added to lagoon list"
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
        if (image != None) :
            cv2.imshow("camera", image)
            if cv.WaitKey(pause) == 27:
                return
        else :
            print "image was None in drawLagoons"

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


def write_settings(cFile):
    global settings
    f = open(cFile, 'w')
    f.write(str(settings))
    f.close()

def read_settings(cFile):
    global settings
    f = open(cFile, 'r')
    settings = eval(f.read())
    f.close()

def setupCamera(setup) :
    cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
    if cv2.__dict__['moveWindow'] != None :
        cv2.moveWindow("camera", 100, 0)
    else :
        print "cv2 does not contain moveWindow. Update your OpenCV installation."
    cam = ipCamera(setup)
    cam.brightness(cam.params['brightness'])
    cam.contrast(cam.params['contrast'])
    return cam

# Rather than a command line argument, process reads one line from stdin to identify configuration
# 'splatspace'  Wired outdoor camera
# 'splatwifi'   Wireless outdoor camera
# 'usb'         Any USB camera
# 'musuem'      Wired indoor PTZ camera
# 'sandstone'   Wireless outdoor camera (home ssid: milton)

configFile = "evo.settings"
#defaultConfig = 'museum'
#defaultConfig = 'usb'
#defaultConfig = 'sandstone'
defaultConfig = 'museumo'

def load(name, file, default_dict) :
	try:
		gdb[name] = eval(open(file).read())
	except:
		print file + " not found: Using default coordinates"
		gdb[name] = default_dict

# getFluor(ipcam, 'flux.config')
def getFluor(ipcam, file) :
    basefile = './baseline.jpg'
    baseline = None
    fluxfile = './flux'
    if (gdb['layout'] == None) :
	load('layout', file, {
            'LagoonRegion' : (650,180,940,620),
            'Lagoon1' :   (67, 13, 60, 210),
            'Lagoon2' :  (151, 14, 60, 210),
            'Lagoon3' :   (226, 10, 59, 210),
            'Lagoon4' :   (299, 16, 60, 210),
            'NumLagoons'   :    4,
            'Frames'       :   80,
            'lagoonHeight' : 210,    # Can be thought of as the divisor for levelScale
            'levelScale'   : 100,   # 100 will give level as percentage of lagoonHeight
            'camera'       : 'outdoor',
            'rotate'       : 90,
            'MAC'          : "00:62:6e:4f:17:d9",
            'defaultIP'    : "172.16.3.123",
            'userpwd'      :  "&usr=scrapsec&pwd=lakewould",
            'brightness'   :  60,   # 0-100 for outdoor camera
            'brightnessCmd': ":88/cgi-bin/CGIProxy.fcgi?cmd=setBrightness&brightness=",
            'contrast'     :  30,   # 0-100 for outdoor camera
            'contrastCmd'  : ":88/cgi-bin/CGIProxy.fcgi?cmd=setContrast&constrast=", # NB: Foscam typo
            'picCmd'       : ":88/cgi-bin/CGIProxy.fcgi?cmd=snapPicture2&usr=scrapsec&pwd=lakewould"
            })

    result = None
    cntr = 0
    if (os.path.exists(basefile)) :
        baseline = cv2.split(cv2.imread(basefile))[1]
    elif ( not 'baseline' in sys.argv) :
        print "Run [flux baseline] to create dark heat image file"
    frames = gdb['layout']['Frames']
    orig = ipcam.grab()
    fluor = orig[:,:,1]               # FIRST GREEN IMAGE
    while(cntr < frames) :
        orig = ipcam.grab()
        (bl, gr, rd) = cv2.split(orig)
        fluor = cv2.add(fluor, cv2.subtract(gr,cv2.add(bl/2,rd/2)))
        cv2.imshow("camera",fluor)
        if cv.WaitKey(10) == 27:
                exit()
        cntr = cntr + 1

    if (baseline == None and len(sys.argv)>1 and sys.argv[1] == 'baseline') :
        print "Creating baseline file"
        cv2.imwrite(basefile, fluor)
    else :
        fluor = cv2.subtract(fluor,baseline)
    ffile = open(fluxfile,'w')
    for l in [ 'Lagoon1', 'Lagoon2', 'Lagoon3', 'Lagoon4' ]:
        (x, y, w, h) = gdb['layout'][l]
        ffile.write(l + "(" + str(cv2.mean(fluor[y:y+h,x:x+w])[0]) + ").\n")
    ffile.close()

if __name__ == "__main__" :
    print "openCV('" + str(cv2.__version__) + "')."
    levelfile = './levels'
    read_settings(configFile)
#    config = sys.stdin.readline()[:-1]
#    if not(config in settings.keys()) :
    config = defaultConfig
    print "Using " + config + " configuration"

    ipcam = setupCamera(config)  # Initialize Camera 'usb' 'museum' 'splatwifi', etc.
    if ( 'fluor' in sys.argv) :
        getFluor(ipcam, 'flux.config')
        print 'bailing out early'
        exit(0)

    retry = True
    (x1,y1,x2,y2) = ipcam.params['LagoonRegion']
    for f in range(10) :
        img = ipcam.grab()
        if (img != None) :
            cv2.rectangle(img,(y1,x1),(y2,x2),(0,0,255),2)
            cv2.imshow("camera",img)
            if cv.WaitKey(100) == 27:
                exit()
        else:
            print "Image grab returned None in __main__"
            exit()
    print "done waiting for brightness to settle"
    ffile = open(levelfile,'a')
    while(retry) :
        retry = False
        ipcam.updateLagoons(pause=10) # blob contours shown for 4 seconds
        for i in range(5) :
            if ( ipcam.updateLevels(pause=100) == None) :
                print "Go back to blob detection and try again"
                retry = True
                break
            for k in Levels.keys() :
                ffile.write(k + "(" + str(Levels[k]) + ").\n")
    ffile.close()
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

