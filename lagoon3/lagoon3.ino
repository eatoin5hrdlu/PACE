/*
 * Lagoon controller
 *
 * 1) Create valve controller
 * 2) Accept commands from main computer to:
 *     a) adjust timings
 *     b) control meniscus light
 *     c) set auto/manual temperature control
 *     d) set auto/manual flow control
 * 3) Check temperature and manage lagoon heater
 *
 * Commands:
 *  l0 :    Meniscus light off
 *  l1 :    Meniscus light on
 *  h0 :    Heater off
 *  h1 :    Heater on
 *  m0 :    Mixing motor off
 *  m1 :    Mixing motor on
 *  a0 :    Auto modes off
 *  a1 :    Auto modes on
 *  vN :    Go to valve position N, Auto mode off
 *
 *  {id}N : Increase/decrease open time of valve by internal increment
 *
 *  cN : Calibrate valve N
 *      Open it on schedule with other valves closed
 *  r  : Run mode (calibration off)
 */
#include "param.h"

#include <Servo.h>
Servo myservo;
void swrite(int val) {
     digitalWrite(VALVEENABLE,0);
     delay(150);
     myservo.write(val);
     delay(150);
     digitalWrite(VALVEENABLE,1);
}

#include "valve.h"        // Includes param.h (change constants there)
VALVE    valve = VALVE(5, VALVEPIN);      // 5-position valve on pin 9


#include "temperature.h" 
TEMPERATURE temp = TEMPERATURE(0);  // Analog Temperature on pin A0

boolean auto_temp;   // Automatically control Heater
boolean auto_valve;  // Automatically control Valves

/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#ifdef STANDALONE
#include "EEPROM.h"
int RomAddress  = 0;

byte id = 'z'; // Zeno = unassigned, by default
float target_temperature;
int interval;   // Variable to keep track of the time

int reading[10];

// Keep temperature within 0.5 degree C

void checkTemperature()
{
float t = temp.celcius();
	if (t < target_temperature)        digitalWrite(HEATER,1);
	if (t > target_temperature + 0.25) digitalWrite(HEATER,0);
}

// 'RomAddress' global will be bumped by successive
// calls to moveData( SAVE|RESTORE, size, ptr)

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
	if (op == SAVE) Serial.println("save");
	else            Serial.println("restore");
	RomAddress = 0;
	moveData(op, 1, &id);
	moveData(op, sizeof(float), (byte *) &target_temperature);
	moveData(op, (NUM_VALVES+1)*sizeof(int), (byte *) valve.getTimes());
	moveData(op, (NUM_VALVES+1)*sizeof(byte), (byte *) valve.getAngles());
}
#endif

void printHelp(void)
{
	Serial.print("\n\n");
	Serial.println("a : set auto {temp, flow, valve} mode");
	Serial.println("at: set auto heater mode");
	Serial.println("af: set auto pump mode");
	Serial.println("av: set auto valve mode");
	Serial.println("iX : Set ID to X");
	Serial.println("m : set manual {temp, flow, valve} mode");
	Serial.println("mt: set manual heater mode");
	Serial.println("mf: set manual pump mode");
	Serial.println("mv: set manual valve mode");

	Serial.println("p+N: turn pump N on");
	Serial.println("p-N: turn pump N off");

	Serial.println("iN: Increase time for valve N");
	Serial.println("dN: Decrease time for valve N");
	Serial.println("cN: Calibrate valve N");
	Serial.println("r:  Normal Run mode");
}

void mixer(byte v)
{
return;
	if (v == 0)
		analogWrite(MIXER,0);
	else 
	    for(int i=3; i<11; i++) {
		analogWrite(MIXER, i*MIXERSPEED/10);
		if (auto_valve) valve.checkValve();
		delay(500);
 	    }
}

boolean lagoon_command(char c1, char c2, int value)
{
char reply[40];
byte d;
	switch(c2)
	{
		case '1': d = 1; break;
		case '0': d = 0; break;
		default : break;
	}
	switch(c1)
	{
		case 'a':
			if (d == 1) {
				auto_temp = true;
				auto_valve = true;
			} else {
				auto_temp = false;
				auto_valve = false;
			}
			break;
		case 'd':
		        valve.setAngle(c2,value);
			break;
		case 'e':
			valve.enable(d);
			break;
		case 'h':
			digitalWrite(HEATER, d);
			break;
		case 'i':
			if (c2 != 0)
				id = c2;
			else {
				Serial.println(id);
			}
			break;
		case 'l':
	                if (d == 1)
				digitalWrite(LED, 0); // Active low
			else
				digitalWrite(LED, 1);
			break;
		case 'm':
			Serial.print("mixer(");
			Serial.print(d);
			Serial.println(").");
			Serial.println(d);
			mixer(d);
			break;

		case 'p':
		        auto_valve = false;
			valve.position(c2-'0');
			break;
		case 'r':  
			switch(c2) {
				case 'v': valve.report(reply);
					  Serial.println(reply);
					   break;
				case 't':
					Serial.print("temperature(");
					Serial.print(temp.celcius());
					Serial.println(").");
					break;
				default: saveRestore(RESTORE);
					 break;
			}
			break;
		case 's':
			saveRestore(SAVE);
			break;
		case 't':
			break;
		case 'v':
			valve.setup_valve(c2-'0', value);
			break;
		default:
			return false;
	}
	if (strlen(reply) > 0) Serial.println(reply);
	return true;
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
		Serial.println("okay");	
	}
	if ( is.length() > 0 )  {   // process the command
		int value = 0;
		if (is.length() > 2)
			value = atoi(&is[2]);
		if (!lagoon_command(is[0], is[1], value))
			Serial.println("bad flow command [" + is + "]");
	}
}

/*
 * average() throw out two extreme values and average the rest
 */

float average(int *arr, int size)
{
	float avg = 0;
	int mx = 0;
        int mn = 2000;
	for (int i=0; i<size; i++) 
	{
		avg += arr[i];
		if (arr[i] < mn) mn = arr[i];
		if (arr[i] > mx) mx = arr[i];
	}
	return ( ( avg - (mn+mx) )/(size-2));
}

float stdev(int *arr, int size, float avg)
{
	float sumsq = 0;
	int mx = 0;
        int mn = 2000;
	for (int i=0; i<size; i++)
	{
		sumsq += (avg - arr[i])*(avg - arr[i]);
		if (arr[i] < mn) mn = arr[i];
		if (arr[i] > mx) mx = arr[i];
	}
	sumsq = sumsq - (avg - mn)*(avg - mn);
	sumsq = sumsq - (avg - mx)*(avg - mx);
	return sqrt(sumsq/(size-2));
}

/*
 * setupp()	1) Initializes serial link
 *		2) Restores settings from EEPROM
 *		2) Calls flow_setup (pumps)
 *		3) Calls turbid_setup (LED/Optics)
 */

boolean once;

void setup()
{
	Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
	auto_temp = true;  // Maintain Temperature Control
	auto_valve = true;  // Maintain Flow

	pinMode(LAGOONOUT, OUTPUT);  digitalWrite(LAGOONOUT, 0);
	pinMode(HEATER,    OUTPUT);  digitalWrite(HEATER, 1);
	pinMode(LED,       OUTPUT);  digitalWrite(LED, 1);
	pinMode(VALVEENABLE,OUTPUT); digitalWrite(VALVEENABLE,1);
	myservo.attach(VALVEPIN);

//        analogWrite(MIXER, 0 );     // Mixer off

	interval = millis();

//	if (true)
	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = '1';	// Default Lagoon ID 
		target_temperature = 36.5;
		valve.setup_valve(1, 8000);
		valve.setup_valve(2, 6000);
		valve.setup_valve(3, 4000);
		valve.setup_valve(4, 2000);
		saveRestore(SAVE);
	}
	else
	{
		saveRestore(RESTORE); // Valve timing copied to valve object
	}
	once = true;
}

int cnt_light = 0;
int cnt_mixer = 0;
void loop()
{
	respondToRequest();     // Check for command
	delay(100);
	if (auto_temp)		// Check and update heater(s)
		checkTemperature();
	if (auto_valve)		// Check and update nutrient valve
		valve.checkValve();
}
