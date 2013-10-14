// Sample Collector
// 1) Set Start Time
// 2) Set number of samples and times
//
#include <EEPROM.h>
const int debug = 1;
#include "EEPROM.h"
void printHelp(void)
{
	Serial.print("\n\n");
	Serial.println("d:  print settings");
	Serial.println("h:  print this option menu");
	Serial.println("t <value> : time between samples");
	Serial.println("e <value> : time between sample groups");
	Serial.println("n <value> : total number of samples");
	Serial.println("g <value> : number of samples in group");
	Serial.println("s: save current settings\n");
}

// BEGIN DEFAULT CALIBRATION PARAMETERS
// Values will be read from EEPROM, after being saved with the 's' command.
// Values below are the defaults before they are saved to EEPROM or 
// after using the 'z' command to zero the EEPROM.

const int SAMPLES    =  4;
const int START_TIME =  6;
int temperature      = 555; // 31 deg C, elevated for eColi
// END PARAMETERS

// Three-color LED control
const int RED   = 6;
const int GREEN = 7;
const int BLUE  = 8;

//#define P1 2
//#define P2 4
//#define P3 3
//#define P4 5

#define P4 2
#define P3 4
#define P2 3
#define P1 5

#define MT 10

void set_indicator(int t)
{
	if ( t < 6 ) return;     // Sensor not being used
        digitalWrite(RED, HIGH);
        digitalWrite(BLUE, HIGH);
        digitalWrite(GREEN, HIGH);
        if      ( t < 10 ) digitalWrite(BLUE, LOW);
        else if ( t < 20 ) digitalWrite(RED, LOW);
        else               digitalWrite(GREEN, LOW);
}

void usage()
{
    Serial.println("Commands:");
    Serial.println(" s N             # Set number of samples to N");
    Serial.println(" d N             # Set inter-sample time to N minutes");
    Serial.println(" s               # Save current values in EEPROM");
    Serial.println(" r               # Restore values from EEPROM");
    Serial.println(" z               # Zero all values in EEPROM");
    Serial.println(" c               # Clear (return to home position)");
    Serial.println(" N               # Print location");
}

void save_values() // "s"
{
	int offset = 0;
	EEPROM.write(offset,   START_TIME);
	EEPROM.write(offset+1, SAMPLES);
	EEPROM.write(offset+2, temperature);
}

void zero_eeprom() // "z"
{
    for (int i=0; i<3; i++)
	EEPROM.write(i,0);
}

int nextSample = 0;
void setup()
{
	saveRestore(RESTORE);
	if (sampleTime == 0) // Defaults (first time)
	{
		sampleTime = 600; // Time between samples
		sampleNum  = 10;  // Total number of samples to take
		groupNum   = 1;   // Number of samples per group
		groupTime  = 0;   // Time between groups
		saveRestore(SAVE);
	}
	nextSample = sampleTime;
	Serial.begin(9600);
	pinMode(2, OUTPUT);
	digitalWrite(2,0);
	pinMode(3, OUTPUT);
	digitalWrite(3,0);
	pinMode(4, OUTPUT);
	digitalWrite(3,0);
	pinMode(5, OUTPUT);
	digitalWrite(5,0);
	pinMode(RED, OUTPUT);
	digitalWrite(RED,1);
	pinMode(GREEN, OUTPUT);
	digitalWrite(GREEN,1);
	pinMode(BLUE, OUTPUT);
	digitalWrite(BLUE,1);
	usage();
	delay(1000);
}

void forward(int steps)
{
	for(int i=0; i<steps; i++) {
		digitalWrite(P1, 1);
		delay(MT);
		digitalWrite(P4, 0);
		delay(MT);
		digitalWrite(P2, 1);
		delay(MT);
		digitalWrite(P1, 0);
		delay(MT);
		digitalWrite(P3, 1);
		delay(MT);
		digitalWrite(P2, 0);
		delay(MT);
		digitalWrite(P4, 1);
		delay(MT);
		digitalWrite(P3, 0);
		delay(MT);
	}
	digitalWrite(P4, 0);
}

void reverse(int steps)
{
	for(int i=0; i<steps; i++) {
		digitalWrite(P4, 1);
		delay(MT);
		digitalWrite(P4, 0);
		digitalWrite(P3, 1);
		delay(MT);
		digitalWrite(P3, 0);
		digitalWrite(P2, 1);
		delay(MT);
		digitalWrite(P2, 0);
		digitalWrite(P1, 1);
		delay(MT);
		digitalWrite(P1, 0);
	}
}

void flash(void)
{
        digitalWrite(13,1);
	delay(300);
	digitalWrite(13,0);
}

void loop()  // Check all Turbostats. Loop time ~ 1 second
{
	flash();
	delay(300);
	flash();
	forward(164);
	flash();
}

// Set global 'RomAddress' and address will be bumped
// by successive calls to moveData( SAVE|RESTORE, size, ptr)
int      SAVE   = 1;
int   RESTORE   = 0;
int RomAddress  = 0;

void moveData(int op, int size, byte *loc)
{
	for(int i=size;i>0;i--)
		if (op == SAVE)
			EEPROM.write(RomAddress++,*loc++);
		else
			*loc++ = EEPROM.read(RomAddress++);
}

void saveRestore(int op)
{
	int i;
	RomAddress = 0;
	moveData(op, sizeof(int), &sampleTime );
	moveData(op, sizeof(int), &sampleNum );
	moveData(op, sizeof(int), &groupNum );
	moveData(op, sizeof(int), &groupTime );
}

int sampleTime = 600; // Time between samples
int sampleNum  = 10;  // Total number of samples to take
int groupNum   = 1;   // Number of samples per group
int groupTime  = 0;   // Time between groups

int currentSample  = 0; 
int sampleTiming   = 0;
int groupTiming    = 0;


void dump(void)    // Print configuration settings
{
  int i;
  Serial.print("t "); Serial.println(sampleTime);
  Serial.print("n "); Serial.println(sampleNum);
  Serial.print("g "); Serial.println(groupSize);
  Serial.print("t "); Serial.println(sampleTime);
}

void respondToRequest(void)
{
	String is = "";
	while (Serial.available() > 0)  // Read a line of input
	{
		int c  = Serial.read();
		if ( c < 32 ) break;
		is += (char)c;
		if (Serial.available() == 0) // It is possible we're too fast
			delay(100);
	}
	if ( is.length() > 0 )  {   // process the command
		int value = 0;
		if (is.length() > 2)
			value = atoi(&is[2]);
		process(is[0], value);
	}
}

void process(char c, int value)
{
	switch(c) {
		case 'd':
			dump();
			break;
		case 'h':
			printHelp();
			break;
		case 'l' :
			Serial.println("l command");
			Serial.print(normalize( TEMPERATURE,
						temperature[sampleIndex]));
			Serial.print(" ");
			Serial.println(normalize(TURBIDITY,
						 turbidity[sampleIndex]));
			break;
		case 'r' :
			saveRestore(RESTORE);
			break;
		case 't' :
			sampleTime = value;
			break;
		case 's' :
			saveRestore(SAVE);
			break;
		default :
			Serial.print("Ignoring [");
			Serial.write(c1);
			Serial.println("]");
	}
}

void loop()
{
	flash();
	delay(1000);  // Minimum Delay increment
	flash();
	sampleCountdown--;
	if (sampleCountdown == 0)
	{
		sampleCountdown = sampleTime;
		if (currentSample % groupNum == 0)
		{

		}
		forward(164);
	}
	respondToRequest();
	flash();
}
