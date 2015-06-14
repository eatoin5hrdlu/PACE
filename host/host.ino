#include <Wire.h>
#include <Adafruit_MLX90614.h>
Adafruit_MLX90614 mlx;
// #define DEBUG 1
#define EOT "end_of_data"
/*
 * Host controller
 *
 * 1) Create nutrient valve controller
 * 2) Accept commands from main computer to:
 *     a) adjust timing
 *     b) control meniscus light
 *     c) set auto/manual temperature control

 * 3) Check temperature and manage host and nutrient heaters
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
 *  oN :    Open valve N
 *  cN :    Close valve N  (also, auto_valve mode turned ON)
 *  pN :    Prime (open valve N, auto_valve mode OFF)
 *  vN :    Add 10ms to open time for valve N
 *  dN :    Subtract 10ms to open time for valve N
 *
 *  {id}N : Increase/decrease open time of valve by internal increment
 *
 *  cN : Calibrate valve N
 *      Open it on schedule with other valves closed
 *  r  : Run mode (calibration off)
 */

#include "valves.h"        // Includes param.h (change constants there)
#include "temperature.h" 

VALVES valves = VALVES(1);
// TEMPERATURE temp = TEMPERATURE(0);  // Analog pin number
TEMPERATURE temp = TEMPERATURE(A5,A4);  // Digital pins SCL, SDA

boolean auto_temp;   // Automatically control Heater
boolean auto_valve;  // Automatically control Valves

/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#ifdef STANDALONE
#include "EEPROM.h"
int RomAddress  = 0;

byte id = 'z'; // Zeno = unassigned, by default
float target_temperature;
float target_turbidity;
int interval;   // Variable to keep track of the time

int reading[10];

// Keep temperature within 0.5 degree C

void checkTemperature()
{
float t = temp.celcius();
	if (t < target_temperature) {
#ifdef DEBUG
		Serial.println(t);
		Serial.println("Temperature is low");
#endif
	        digitalWrite(HEATER,1);
	}
	if (t > target_temperature + 0.25) {
		digitalWrite(HEATER,0);
#ifdef DEBUG
		Serial.println(t);
		Serial.println("Temperature is high");
#endif
	}
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
#ifdef DEBUG
	if (op == SAVE) Serial.println("save");
	else            Serial.println("restore");
#endif
	RomAddress = 0;
	moveData(op, 1, &id);
	moveData(op, sizeof(float), (byte *) &target_temperature);
	moveData(op, MAX_VALVES*sizeof(int), (byte *) valves.getTimes());
	moveData(op, sizeof(float), (byte *) &target_turbidity);
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

int turbread[10];
int turbindex = 0;

double ODhist[10];
int ODhindex = 0;

#define TURB_DELAY 10
int turbdelay = 0;

double turbidity() {
int i;
double total = 0.0;
	for (i=0;i<10;i++) {
		total += ODhist[i];
		Serial.println(total);
	}
	return total/10.0;
}

int checkTurbidity() {
int highlow = 0;
int i, t, avg;
double OD;
	digitalWrite(LASER,1);
	delay(100);
// Read Turbidity and bump the ReadArray index
	turbread[turbindex] = analogRead(ANALOG_TURBIDITY);
	digitalWrite(LASER,0);
	turbindex = (turbindex+1)%10;

// Average the last ten values and bump the delay index
	avg = 0;
	for (i=0;i<10;i++) avg += turbread[i];
	OD = ((double) avg / 9100.0);
	turbdelay = (turbdelay+1)%TURB_DELAY;
// After a certain delay, store the average Optical Density
	if (turbdelay == 0) {
		ODhist[ODhindex] = OD;
		ODhindex = (ODhindex+1)%10;
	}
// Unanimous vote of last ten delayed averages up or down
	t = 0;
	for(i=0;i<10;i++) {
		if (ODhist[i] > target_turbidity) t++;
		if (ODhist[i] < target_turbidity) t--;
	}
	// High or Low Turbidity must be unanimous
	return (t/10);  // -1, 0, +1 
}

void mixer(byte v)
{
	if (v == 0)
		analogWrite(MIXER,0);
	else 
	    for(int i=3; i<11; i++) {
		analogWrite(MIXER, i*MIXERSPEED/10);
		if (auto_valve) valves.checkValves();
		delay(500);
 	    }
}

boolean lagoon_command(char c1, char c2, int value)
{
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
		case 'b':
			Serial.println(turbidity());
			break;
		case 'c':
			valves.closeValve(c2);
			auto_valve = true;
			break;
		case 'd':
			valves.adjust(c2,-10);
			break;
		case 'h':
			digitalWrite(HEATER, d);
			valves.report();
			break;
		case 'i':
			if (c2 != 0)
				id = c2;
			else {
				Serial.println(id);
			}
			break;
		case 'l':
			digitalWrite(LED, d);
			break;
		case 'm':
			Serial.print("mixer ");
			Serial.println(d);
			mixer(d);
			break;
		case 'o':
			valves.openValve(c2);
			break;
		case 'p':
			auto_valve = false;
			valves.openValve(c2);
			break;
		case 'r':
			saveRestore(RESTORE);
			break;
		case 's':
			saveRestore(SAVE);
			break;
		case 't':
			Serial.println(temp.celcius());
			break;
		case 'v':
			valves.adjust(c2,10);
			break;
		default:
			return false;
	}
	Serial.println(EOT);
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
	}
	if ( is.length() > 0 )  {   // process the command
		int value = 0;
		if (is.length() > 2)
			value = atoi(&is[2]);
		if (!lagoon_command(is[0], is[1], value)) {
			Serial.println("bad flow command [" + is + "]");
			Serial.println(EOT);
		}
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
 * setup()	1) Initializes serial link
 *		2) Restores settings from EEPROM
 *		2) Calls flow_setup (pumps)
 *		3) Calls turbid_setup (LED/Optics)
 */

boolean once;

void setup()
{
	auto_temp = true;  // Maintain Temperature Control
	auto_valve = true;  // Maintain Flow (check turbidity)
	valves.setValve(NUTRIENT,3000); // Initially 3 seconds out of 20

	pinMode(NUTRIENT,  OUTPUT);  digitalWrite(NUTRIENT,   0);
	pinMode(HEATER, OUTPUT); digitalWrite(HEATER, 1);
	pinMode(LED, OUTPUT);  digitalWrite(LED, 1);
	pinMode(LASER, OUTPUT);  digitalWrite(LASER, 1);
	// pinMode(MIXER, OUTPUT);
        analogWrite(MIXER, 0);

//	pinMode(OUTFLOW,  OUTPUT);  digitalWrite(OUTFLOW,   0);
//	pinMode(HOSTCELLS, OUTPUT); digitalWrite(HOSTCELLS, 0);
//	pinMode(INDUCER1,  OUTPUT); digitalWrite(INDUCER1,  0);
//	pinMode(INDUCER2,  OUTPUT); digitalWrite(INDUCER2,  0);

	interval = millis();
	Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
	mlx = Adafruit_MLX90614();
	mlx.begin();   // Initialize Mexexis Thermometer
	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = '1';	// Default Lagoon ID 
		target_temperature = 36.5;
		valves.setTime('1',4000);
		valves.setTime('2',3000);
		valves.setTime('3',1000);
		valves.setTime('4',0);
		target_turbidity = 0.4;
		saveRestore(SAVE);
	}
	else
	{
		saveRestore(RESTORE);
#ifdef DEBUG
		Serial.print("target temperature ");
	 	Serial.println(target_temperature);
		Serial.print("target turbidity ");
	 	Serial.print(target_turbidity);
		Serial.println(" restored");
#endif
	}
	once = true;
}

int cnt_light = 0;
int cnt_mixer = 0;
void loop()
{
int tb_thresh;
	respondToRequest();     // Check for command
	delay(10);
	if (auto_temp)		// Check and update heater(s)
		checkTemperature();
	if (auto_valve)		// Check and update nutrient valve
		valves.checkValves();
	delay(1000);
	tb_thresh = checkTurbidity();
	if (tb_thresh > 0) {
#ifdef DEBUG
		Serial.println("Turbidity is defintely too high");
#endif
		valves.adjust('1', +100);
	}
	else if (tb_thresh < 0) {
#ifdef DEBUG
		Serial.println("Turbidity is defintely too low");
#endif
		valves.adjust('1', -100);
	} else {
#ifdef DEBUG
		Serial.println("Turbidity is okay");
#endif
	}

#ifdef DEBUG
	Serial.println(turbidity());
#endif
}
