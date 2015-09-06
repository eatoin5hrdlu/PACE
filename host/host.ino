#include "param.h"        // Includes param.h (change constants there)
#include <Wire.h>
#include <Adafruit_MLX90614.h>
Adafruit_MLX90614 mlx;
// #define DEBUG 1
#define EOT "end_of_file."
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

boolean auto_temp;   // Automatically control Heater
boolean auto_valve;  // Automatically control Valves
boolean bluetooth;
 
char reply[40];

#include "secrets.h"
#include "wifi.h"
WIFI w = WIFI();

void sout(const char *str) {
     if (bluetooth) Serial.print(str);
     else           w.mysend(str);
}

void soutln(const char *str) {
     if (bluetooth) Serial.println(str);
     else           w.mysend(str);
}


// All Pathe Arduino control programs contain at least:
//
//   void respondToRequest(void)
//   bool process_command(char c1, char c2, int value)
//   void setup()
//   void loop()
//

void wfRespondToRequest(void)
{
  char c1 = NULL, c2 = NULL;
  int value = 0;

  if (Serial.available())
  {
    if ( !w.connected() )
    {
        w.accept();
    }
    else
    {
	if (w.myrecv(&c1, &c2, &value))
	{
	    wfProcess_command(c1, c2, value);
        }
    }
  }
}

bool wfProcess_command(char c1, char c2, int value)
{
  if (c1 == 'x' && c2 == 'x' && value == -1)
  {
    w.mysend("closed(x,x,-1).");
    delay(1000);
    w.reboot(3); // Nothing short of a full restart will work
  }
  else
  {
    sprintf(reply, "thanks[%c][%c]%d", c1, c2, value);
    return w.mysend(reply);
  }
}

#include "valves.h"        // Includes param.h (change constants there)
#include "temperature.h" 

VALVES valves = VALVES(1);
// TEMPERATURE temp = TEMPERATURE(0);  // Analog pin number
TEMPERATURE temp = TEMPERATURE(A5,A4);  // Digital pins SCL, SDA

/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#include "EEPROM.h"
int RomAddress  = 0;

byte id = 'z'; // Zeno = unassigned, by default
int gcycletime;
float target_temperature;
float target_turbidity;
int gtscale;
float gtoffset; // Offset and scale for Turbidity calculation

int interval;   // Variable to keep track of the time

int reading[10];

// Keep temperature within 0.5 degree C

void checkTemperature()
{
float t = temp.celcius();
	if (t < target_temperature) {
#ifdef DEBUG
                sprintf(reply, "temperature(low,%f).",t);
		sout(reply);
#endif
	        digitalWrite(HEATER,1);
	        digitalWrite(LED,1);
	}
	if (t > target_temperature + 0.25) {
		digitalWrite(HEATER,0);
		digitalWrite(LED,0);
#ifdef DEBUG
                sprintf(reply, "temperature(high,%f).",t);
		sout(reply);
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
	if (op == SAVE) sout("save.");
	else            sout("restore.");
#endif
	RomAddress = 0;
	moveData(op, 1, &id);
	moveData(op, sizeof(float), (byte *) &target_temperature);
	moveData(op, MAX_VALVES*sizeof(int), (byte *) valves.getTimes());
	moveData(op, sizeof(float), (byte *) &target_turbidity);
	moveData(op, sizeof(int), (byte *) &gtscale);
	moveData(op, sizeof(float), (byte *) &gtoffset);
	moveData(op, sizeof(int), (byte *) &gcycletime);
}

void printHelp(void)
{
String msg = "commands([a(automode),";
       msg = msg + "at(auto_temp),";
       msg = msg + "af(auto_pump),";
       msg = msg +       "av(auto_valve),";
       msg = msg +       "i(id),"
	       +       "m(manual),"
	       +       "mt(manual_temp),"
	       +       "mf(manual_pump),"
	       +       "mv(manual_valve),"
	       +       "pN1(pump_on),"
	       +       "pN0(pump_off),"
	       +       "iN(increase_valve),"
	       +       "dN(decrease_valve),"
	       +       "cN(calibrate_valve),"
	       +       "r(run)]).";
     	       	       
      sout(msg.c_str());
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

/* Assumes constant offset for turbidity calculation */
/* number in thousandths. e.g. 400 = (OD600 0.400) */

void forceTurbidity(int currentTurbidity)
{
   int i;
   unsigned int reading = 0;
   for (i=0; i<5; i++) reading += analogRead(ANALOG_TURBIDITY);
   reading = reading/5;
   int calcturb = reading/gtscale + gtoffset;
   sprintf(reply, "read_turbidity(%d,%d).", reading, calcturb);
   int newscale = reading/(currentTurbidity - gtoffset);
//   sprintf(reply, "scdelta(%d,%d).", gtscale, newscale);
   gtscale = newscale;
//   saveRestore(SAVE);
   soutln(reply);
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

boolean cellstat_command(char c1, char c2, int value)
{
byte d;
float tf;
int hight,lowt;
			int low =  ((int)tf) % 10;
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
		        sprintf(reply,"turbidity(%f).",turbidity());
			soutln(reply);
			break;
		case 'c':
			valves.closeValve(c2);
			auto_valve = true;
			break;
		case 'd':
			valves.adjust(c2,-10);
			break;
		case 'h':
		        switch(c2) {
			 case 'e': printHelp();break;
			 default :
			 	 digitalWrite(HEATER, d);
				 digitalWrite(LED, d);
				 valves.report(reply);
				 soutln(reply);
			}
			break;
		case 'i':
			if (c2 != 0)
				id = c2;
			else {
			     sprintf(reply,"%c.",id);
			     soutln(reply);
			}
			break;
		case 'l':
			digitalWrite(LED, d);
			break;
		case 'm':
		        sprintf(reply,"mixer(%d).",d);
			soutln(reply);
			mixer(d);
			break;
		case 'n':
			forceTurbidity(value);
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
		        switch(c2) {
			  case 'c':
			     gcycletime = value;
			     valves.setCycletime(value);
			     break;
			}
			saveRestore(SAVE);
			break;
		case 't':
		        tf = temp.celcius() * 10.0;
			hight = (int) tf/10.0;
			lowt =  ((int)tf) % 10;
		        sprintf(reply,"%d.%d",hight,lowt);
			soutln(reply);
			break;
		case 'v':
			valves.adjust(c2,10);
			break;
		default:
			return false;
	}
	soutln(EOT);
	return true;
}

void respondToRequest(void)
{
	if (bluetooth) btRespondToRequest();
	else           wfRespondToRequest();
}

void btRespondToRequest(void)
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
		if (!cellstat_command(is[0], is[1], value)) {
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
	bluetooth = true;
	auto_temp = true;  // Maintain Temperature Control
	auto_valve = true;  // Maintain Flow (check turbidity)
	valves.setValve(NUTRIENT,3000); // Initially 3 seconds out of 20

	pinMode(NUTRIENT,  OUTPUT);  digitalWrite(NUTRIENT,   0);
	pinMode(HEATER, OUTPUT); digitalWrite(HEATER, 0);
	pinMode(LED, OUTPUT);  digitalWrite(LED, 0);
	pinMode(JARLIGHT, OUTPUT);  digitalWrite(JARLIGHT, 0);
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
//	if (true)
	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = 'd';	// Default Lagoon ID 
		target_temperature = 36.5;
		gcycletime = DEFAULT_CYCLETIME;
		valves.setTime('1',4000);
		target_turbidity = 0.4;
		gtscale = 9100;
		gtoffset = 0.0;
		saveRestore(SAVE);
	}
	else
	{
		saveRestore(RESTORE);
#ifdef DEBUG
		sprintf(reply,"tmtbscale(%f,%f,%f).",
			target_temperature,target_turbidity,gtscale);
		soutln(reply);
#endif
	}
	valves.setCycletime(gcycletime);
	once = true;
}

int cnt_light = 0;
int cnt_mixer = 0;
void loop()
{
int tb_thresh;
	respondToRequest();     // Check for command
	delay(1000);
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
