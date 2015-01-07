#include "bipolar.h"

//#define NUMSAMPLES 12
//#define SAMPLETIME 45

#define NUMSAMPLES 4
#define SAMPLETIME 1

BIPOLAR bp = BIPOLAR(NUMSAMPLES, SAMPLETIME);
boolean once = true;

#define LED       13

void setup()
{
	Serial.begin(9600);
	pinMode(LED, OUTPUT);
	once = true;
}

void flashDelay(int msecs)
{
int delta = msecs/6;
	digitalWrite(LED,1);	delay(msecs/6);
	digitalWrite(LED,0);	delay(msecs/6);
	digitalWrite(LED,1);	delay(msecs/6);
	digitalWrite(LED,0);	delay(msecs/6);
	digitalWrite(LED,1);	delay(msecs/6);
	digitalWrite(LED,0);	delay(msecs/6);
}

void loop()
{
	Serial.println("loop");
	if (once) {
		bp.run();
		once = false;
	}
	else
		flashDelay(1000);
}

