bool disconnected;

#define HOME 1      // WiFi Location for SSID and PASSWORD
#include "wifi.h"

/* Wifi submodule test (plug in replacement for Bluetooth) */

WIFI w = WIFI();

void setup() 
{
	Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
	w.start_server();
	disconnected = false;
}

void respondToRequest()
{
	char *in = w.myrecv();
	switch(in[0]) {
		case 'a' : w.mysend("got an a"); break;
		case 'b' : w.mysend("got a  b"); break;
		case 'x' : disconnected = true;  break;
	}
}

void loop()
{
	respondToRequest();
	delay(500);
	if (disconnected) {
		w.start_server();
		disconnected = false;
	}
}

