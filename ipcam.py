#!/usr/bin/python -u
#!C:/Python27/python -u
import os, time, subprocess, re
import base64, urllib2
 
import numpy as np
import cv2
import cv2.cv as cv

LagoonRegion = (300,0,480,260)
Lagoon = {}
Level = {}

#
# Lagoons is a dictionary containing the coordinates by name "LagoonN"
#
# IPCamera module kknows about different cameras can find camera on 
# network from MAC address, but requires superuser and takes a long time
# This process should write the IP address into config file along
# with lagoon locations and other things which won't be changing
#
# Fluorescence module does color integration (returns saturation cycle)
# Blob detection module ( find lagoon/cellstat coordinates)
# import ipcam, fluor, blob
import blob
import level

#outdoor = "00:62:6e:4f:17:d9"
#indoor =  "c4:d6:55:34:8d:07"
# regular expressions
#MAC        ([a-fA-F0-9]{2}[:|\-]?){6} )
#IP ^((^(\d{1,3}\.){3}(\d{1,3})$)|([\da-fA-F]{1,4}:){7}([\da-fA-F]{1,4}))$
# "http://"+ip+":88/cgi-bin/CGIProxy.fcgi?cmd=snapPicture2&usr=admin&pwd=lakewould"

#defaultIP = "192.168.254.24"  # Change this so we don't need superuser to find it
#port = ":88"
#cmd = "/cgi-bin/CGIProxy.fcgi?cmd=snapPicture2"
#usrpw = "&usr=admin&pwd=lakewould"

def config_out():
    global Lagoon
    f = open('pace.config', 'w')
    print f
    print "THIS" + str(Lagoon)
    f.write(str(Lagoon))
    f.close()

def config_in():
    global Lagoon
    f = open('pace.config', 'r')
    Lagoon = eval(f.read())
    f.close()

class ipCamera(object):
    """Color can be Blue=0, Green=1, or Red=2 (openCV => BGR, not RGB)
       Image represents integration of [ selected color - 1/2(unselected colors) ]"""

    def __init__(self, ip_or_mac):
#        self.defaultIP = "192.168.254.26" # IP for Windows or no superuser
#Sun    self.defaultIP = "172.16.3.101"
        self.defaultIP = "172.16.3.164"
        self.ip = self.ValidIP(ip_or_mac)
        if (self.ip == None) :
            print ip_or_mac, " is not a valid IP/MAC for a Camera"
            exit(1)
        self.cameras = { "00:62:6e:4f:17:d9" : 'outdoor',
                         "c4:d6:55:34:8d:07" : 'indoor' }
        self.camType = self.cameras.get(ip_or_mac,'indoor')
        self.picCmd = {'outdoor':":88/cgi-bin/CGIProxy.fcgi?cmd=snapPicture2&usr=admin&pwd=lakewould",
                       'indoor': "/snapshot.cgi?resolution=32&user=admin&pwd=lakewould" }
        self.url = "http://" + self.ip + self.picCmd[self.camType]
        self.normalBrightness = 100
        self.normalContrast = 4
        self.brightnessCmd = {'outdoor':"http://"+self.ip+":88/cgi-bin/CGIProxy.fcgi?cmd=setBrightness&brightness=",
                              'indoor' : "http://"+self.ip+"/camera_control.cgi?param=1&value=" }
        # NB: Foscam typo
        self.contrastCmd = {'outdoor': "http://"+self.ip+":88/cgi-bin/CGIProxy.fcgi?cmd=setContrast&constrast=",
                            'indoor':  "http://"+self.ip+"/camera_control.cgi?param=2&value=" }
        self.userpwd = {'outdoor': "&usr=admin&pwd=lakewould",
                        'indoor':  "&user=admin&pwd=lakewould" }

        print "Using URL: " + self.url
        self.req = urllib2.Request(self.url)

    def contrast(self, level):
        self.cmdToCamera(self.contrastCmd[self.camType]+str(level)+self.userpwd[self.camType])

    def brightness(self, level):
        self.cmdToCamera(self.brightnessCmd[self.camType]+str(level)+self.userpwd[self.camType])

    def ValidIP(self,s):
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
        try :
            return cv2.imdecode(np.asarray(bytearray(urllib2.urlopen(self.req).read()), dtype=np.uint8), 1)
        except urllib2.URLError, msg :
            print msg, " Failed to get image from camera at ", self.ip
        return None

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

    def bioBlobs(self, color, (x1,y1,x2,y2)) :
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

gstate = -1
def findBlob(image) :
        global gstate
        gstate = gstate + 1
        return (100,33,30,80)

def drawBlobs(image,bbs) :
    cler = [cv.Scalar(0,0,255,255),cv.Scalar(0,255,255,255),cv.Scalar(255,0,0,255),cv.Scalar(255,0,255,255)]
    i = 0
    for bb in bbs :
        cv2.rectangle(image,(bb[0],bb[1]),(bb[0]+bb[2],bb[1]+bb[3]),cler[i%4],1)
        cv2.circle(image,(bb[0],bb[1]),5,cler[(i+1)%4],2)
        i = i + 1

def outlineLagoons(image) :
    global Lagoon
    cler = [cv.Scalar(0,0,255,255),cv.Scalar(0,255,255,255),cv.Scalar(255,0,0,255),cv.Scalar(255,0,255,255)]
    for i in range(4) :
        bbx = Lagoon['Lagoon'+str(i)]
        cv2.rectangle(image,(bbx[0],bbx[1]),(bbx[0]+bbx[2],bbx[1]+bbx[3]),cler[i],1)

def findLagoons(image) :
    """Find blobs in sub-images and then add the offset to the results"""
    global Lagoon
    (rows, cols, depth) = image.shape;
# We start recognizing blobs from the left margin = 0 and
# then move the margin past that blob to look for the next
    x1 = 0
    for i in range(4) :
        name = 'Lagoon'+str(i)
        subimage = image[rows/2:rows, x1:cols, 1]
        Lagoon[name] = [sum(x) for x in zip(findBlob(subimage),(x1,rows/2,0,0))]
        x1 = Lagoon[name][0] + Lagoon[name][2]  # Move left edge of subimage past blob
    outlineLagoons(image)
    cv2.imshow("camera", image)
    if cv.WaitKey(4000) == 27:
            exit()
    
def blobs2lagoons(bbs) :
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
    lagoonHeight = 80
    outlines = []
    for l in lagoons:
        outlines.append((l[0],l[1]-(lagoonHeight-l[3]), l[2],lagoonHeight))
    return outlines
                
def setupCamera() :
    outdoor = "00:62:6e:4f:17:d9"
    indoor = "c4:d6:55:34:8d:07"
    cam = ipCamera(indoor)
    br = 200  # 0-240 for indoor (ptz 905) 0-100 for outdoor (910)
    co = 4    # 0-6 for indoor  0-100 for outdoor
    #    br = 80  # 0-240 for indoor (ptz 905) 0-100 for outdoor (910)
    #    co = 50    # 0-6 for indoor  0-100 for outdoor
    cam.brightness(br)
    cam.contrast(co)
    cv2.namedWindow("camera", cv2.CV_WINDOW_AUTOSIZE)
    cv2.moveWindow("camera", 100, 200)
    return cam


if __name__ == "__main__" :
    print Lagoon
    config_in()
    for k in Lagoon.keys():
        print k + ": Bounding Box = " + str(Lagoon[k])
    ipcam = setupCamera()
    b = blob.Blob(1) # A blob detector for green(1) blobs
    ldet = level.Level()  # A level detector for monochrome images
    frame = ipcam.grab()
#    findLagoons(frame)
    (x1,y1,x2,y2) = LagoonRegion
    frame = frame[x1:x2,y1:y2,:] # Monochrome, cropped for lagoons
    print frame.shape
    bbs = b.blobs(frame)
    sbbs = blobs2lagoons(bbs)  # e.g. sorted
    if (len(sbbs) >= 4) :
        for i in range(4) :
            Lagoon['Lagoon'+str(i+1)] = sbbs[i]
            print 'Lagoon'+str(i+1) + "   " + str(sbbs[i])
    else :
        print "Need at least four bbs, but got " + str(len(sbbs))
    drawBlobs(frame,sbbs)  # Show level detection regions
    cv2.imshow("camera", frame)
    if cv.WaitKey(6000) == 27:
        exit()
    for k in Lagoon.keys():
        bb = Lagoon[k]
        subi = frame[bb[1]:bb[1]+bb[3], bb[0]:bb[0]+bb[2],1]
        lvl = ldet.level(subi)
        Level[k] = lvl
        cv2.line(frame,(bb[0],bb[1]+lvl),(bb[0]+bb[2],bb[1]+lvl), (0,0,255),2)
        cv2.imshow("camera", frame)
        if cv.WaitKey(4000) == 27:
            exit()

#    ipcam.bioBlobs(2,lagoon_position['Lagoon1'])
#    ipcam.bioBlobs(1,lagoon_position['Lagoon2'])
#    ipcam.bioBlobs(1,lagoon_position['Lagoon3'])
#    ipcam.bioBlobs(0,lagoon_position['Lagoon4'])
#    bioBlobs(1)
#    showThisColor(0)
#    showThisColor(1)
#    showThisColor(2)

