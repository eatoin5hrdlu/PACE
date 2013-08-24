#!/usr/local/bin/python
import sys, os, serial
import time

import observe
# observe.video(False)  # Disable display mode before initializing
observe.initialize( 'saved', 0 )
print "initialized"
ser = None

jar = 'Turbidostat'

vessel = {'0' : 'Turbidostat',
	  '1' : 'Lagoon    1',
	  '2' :	'Lagoon    2',
	  '3' :	'Lagoon    3',
	  '4' : 'Lagoon    4' }

if (len(sys.argv)>1 and sys.argv[1] == 'arduino') :
	if (os.name == 'nt') :
		port = int(sys.stdin.readline())
	else :
		port = ("/dev/ttyUSB" + sys.stdin.readline())[:-1]

	try :
		ser = serial.Serial(port, 9600, timeout=1)
		print "c"
	except serial.serialutil.SerialException:
		print "No arduino microcontroller present"

print "starting the loop"
time.sleep(0.5)

import cv2

observe.getLevelString()
u = ['']

keyMap = {
	63232: 'up',
	63233: 'down',
	63234: 'left',
	63235: 'right'
	}

def isInt(s):
	try:
		int(s)
		return True
	except ValueError:
		return False
	except TypeError:
		return False

def getKeyMapping(key):

	if key == -1:
		return None

	if key in keyMap:
		return keyMap[key]
	else:
		try:
			return chr(key)
		except ValueError:
			return None

while(1) :
	k = cv2.waitKey(10)
	u[0] = getKeyMapping(k)
	if not u[0] is None:
		print u[0]
	#u = sys.stdin.readline()
        if (u[0] == 'x') :
		if (ser) :
			ser.close()
		exit()
        if (u[0] == 'z') :
		print "calling getLevelString()"
		res = observe.getLevelString()
		if (res) :
			sys.stdout.write(res + "\r")
	elif (u[0] == 'r') :
		start = time.time()
		while (time.time() < start + 20) :
			res = observe.getLevelString()
			if (res) :
				sys.stdout.write(res + "\r")
	elif (u[0] == 'f') :
		observe.dimension(jar,0,-10)
		observe.getLevelString()
	elif (u[0] == 'd') :
		observe.dimension(jar,0,10)
		observe.getLevelString()
	elif u[0] == 'h' or u[0] == 'left' :
		observe.slide(jar,0,10)
		observe.getLevelString()
	elif (u[0] == 'j') or u[0] == 'up':
		observe.slide(jar,-10,0)
		observe.getLevelString()
	elif (u[0] == 'k') or u[0] == 'down':
		observe.slide(jar,10,0)
		observe.getLevelString()
	elif (u[0] == 'l') or u[0] == 'right':
		observe.slide(jar,0,-10)
		observe.getLevelString()
	elif (u[0] == 's') :
		observe.dimension(jar,10,0)
		observe.getLevelString()
	elif (u[0] == 't') :
		observe.dimension(jar,-10,0)
		observe.getLevelString()
	elif (u[0] == 'w') :
		observe.savebottles('saved')
	elif u[0] == 'p':
		observe.getLevelString()
	elif isInt(u[0]):
		jar = vessel.get(u[0], 'Turbidostat')
	else:		
		if (jar == None) :
			if (ser) :
				ser.write(u)
				ser.write("\n")
				time.sleep(0.1)
				line = ser.readline()
				while(len(line) > 0) :
					print(line[:-1])
					time.sleep(0.02)
					line = ser.readline()
	        		print('end_of_data')


    
