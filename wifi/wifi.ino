bool disconnected;

#define EXHIBITS 1          // WiFi Location for SSID and PASSWORD

//#define SPLATSPACE 1
//#define INNATRIX   1
//#define INNATRIX   1
//#define EXHIBITS   1
//#define HOME       1

#include "secrets.h"
#include "wifi.h"

/* Wifi submodule test (plug in replacement for Bluetooth) */

WIFI w = WIFI();  // Calls Serial.begin(<baudrate>)

void setup() 
{
	Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
	w.start_server();
	disconnected = false;
}

void respondToRequest(void)
{
	char c1 = NULL, c2 = NULL;
	int value = 0;

	if ( ! w.connected() )
	{
		if (w.accept() < 0) {
			delay(2000);
//			Serial.println("still waiting...");
		}
	}
	else
	{
		if ( w.myrecv(&c1,&c2,&value) ) 
			process_command(c1,c2,value);
	}
}

bool process_command(char c1, char c2, int value)
{
	char *msg = "thanks[X][Y]";
	msg[7] = c1;
	msg[10] = c2;
	w.mysend(msg);
	return true;
}

void loop()
{
	respondToRequest();
	delay(500);
}


