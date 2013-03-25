import sys, os, serial, time
fl = open('tmp','a')
fl.write(" debug file opened ")

if (os.name == 'nt') :
	port = int(sys.stdin.readline())
else :
	port = ("/dev/ttyUSB" + sys.stdin.readline())[:-1]

fl.write(str(port))
fl.flush()
try :
	ser = serial.Serial(port, 9600, timeout=1)
#	ser.close()
#	ser.open()
except serial.serialutil.SerialException:
	print('-')
	exit(0)

print "c"
time.sleep(0.5)
while(1) :
	u = sys.stdin.readline()
	fl.write(u),
        if (u[0] == 'x') :
		fl.write(" got termination x ")
		fl.flush()
		ser.close()
		fl.close()
		exit()
	ser.write(u)
	ser.write("\n")
	time.sleep(0.1)
	line = ser.readline()
	while(len(line) > 0) :
		print(line[:-1])
		fl.write(line)
		time.sleep(0.02)
		line = ser.readline()
        print('end_of_data')
        fl.write('end_of_data')
        
    
