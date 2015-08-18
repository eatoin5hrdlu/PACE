#!/usr/bin/python -u 
import socket
import time

class MySocket:
    """demonstration class only
      - coded for clarity, not efficiency
    """

    def __init__(self, sock=None):
        if sock is None:
            self.sock = socket.socket(
                            socket.AF_INET, socket.SOCK_STREAM)
        else:
            self.sock = sock

    def connect(self, host, port):
        self.sock.connect((host, port))

    def mysend(self, msg):
        totalsent = 0
        while totalsent < len(msg):
            sent = self.sock.send(msg[totalsent:])
            if sent == 0:
                raise RuntimeError("socket connection broken")
            totalsent = totalsent + sent

    def myreceive(self):
        chunks = []
        bytes_recd = 0
        chunk = ''
        while (not chunk.endswith('\r\n')) :
            chunk = self.sock.recv(20)
            if chunk == b'':
                raise RuntimeError("socket connection broken")
            chunks.append(chunk)
            print "[" + str(chunk) + "]"
            bytes_recd = bytes_recd + len(chunk)
        return b''.join(chunks)

#
# AT+CWJAP="splatspace","hacktheplanet"
# AT+CWMODE=3  (mode set to both AP and STA)
#
#
# AT+CIPMUX=1    (allow multiple connections)
# AT+CIPSERVER=1,23 (telnet port)
#
#  When incoming connection is established we will get
# <connection>,CONNECT
#  When connection closes we will get
# <connection>,CLOSED
#
#  Data comes in as  +IPD,<connection>,<length>:<data>
#  Data goes out as:
# AT+CIPSEND=<connection>,<length>\r\n
# <data>\r\n
#
# AT+CIPCLOSE=<connection>


if (__name__ == "__main__") :
   print "okay, I'm an app"
   s = MySocket()
   s.connect('192.168.2.149',23)
   while(True) :
       time.sleep(4)
       s.mysend("Awassup\r\n")
       print "sent Awassup"
       time.sleep(4)
       reply = s.myreceive()
       print "Got " + reply

