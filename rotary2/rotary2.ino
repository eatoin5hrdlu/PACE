/*
 * Rotary Flow controller normal operation:
 *
 * 1) Pause at each position for a few seconds every minute to
 * approximate optimal flow rate (2.5 vol/hour, vol = 200mL).
 * 2) Use levels (from camera) and adjust timing for precise flow rate.
 *
 * Commands:
 *  {id}{io}N : Increase/decrease input/output flow for position N
 *      Add or subtract an increment to on time of the specified pump.
 *
 *  ci : Calibrate In-flow, increase working level:
 *      Disable all out-flow pumps
 *      Run three cycles of all Input pumps (off time equal to on time)
 *      Sleep for 10 seconds
 *      Re-enable normal pump cycle.
 *
 *  co : Calibrate Out-flow, decrease working level:
 *      Disable all in-flow pumps
 *      Run three cycles of all Output pumps (off time equal to on time)
 *      Sleep for 10 seconds
 *      Re-enable normal pump cycle.
 *
 */

#include <Wire.h>
#include <Adafruit_MLX90614.h>

Adafruit_MLX90614 mlx;

#include "pump.h"

PUMP host    = PUMP("host", 22, 28, 23);
PUMP inducer = PUMP("inducer", 24, 28, 25);
PUMP waste   = PUMP("waste", 26, 28, 27);


#include "rotary.h"   // Includes param.h (change constants there)

ROTARY r     = ROTARY(15);

boolean auto_temp;  // Automatically control Heater
boolean auto_flow;  // Maintain Flow Control
boolean auto_valve; // Use Valve to maintain turbidity

int interval;   // Variable to keep track of the time
int reading[10];

// Each position has a pump associated with it.
int   rate[8]	=	{	3000, 3000, 3000, 3000,
				3000, 3000, 3000, 3000 };


// Store the on-time, for each of the four input and four
// output pumps.  These values (in milliSeconds) will be
// updated with optimal values that will be stored in EEPROM.

int cycle = IN;      // State variable for which cycle (IN/OUT) we are in
int cntr = 1;        // Loop iteration counter
long unsigned int start_time = 0;  // When pumps were last turned on


byte cal;	// Calibration constant (LED current)
byte temp;      // Thermostat setting

#define CLEAR	200.0   // Constant for clear water OD

int turbid  = 	500;    // Constant for optimal OD

void   temp_setup() {
  mlx = Adafruit_MLX90614();
  mlx.begin();
  auto_temp = true;
}

int get_temp()  { return (int)(0.5 + mlx.readObjectTempC()); }

void check_temp()
{
	int t = get_temp();
	if ( t < temp )
		digitalWrite(HEATER,1);
	else
		digitalWrite(HEATER,0);
}

/* ROLLING AVERAGE OF OPTICAL DENSITY READINGS */

int  ai;	
int rollval[10]	= {	200, 200, 200, 200, 200,
			200, 200, 200, 200, 200, };

int add2rollingavg(int n)
{
int sum = 0;
	rollval[ai] = n;
	ai = (ai + 1)%10;
	for (int i=0;i<10;i++) sum += rollval[i];
	return sum/10;
}
	
void showpumps(void)
{
	for (int i=0; i<8; i++)
	{
		Serial.print(digitalRead(PUMP_ORIGIN+i));
		Serial.print("   ");
	}
	Serial.println("");
}

/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#ifdef STANDALONE
#include "EEPROM.h"
int RomAddress  = 0;
byte id = 'z'; // Zeno = unassigned, by default

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
	Serial.print(op); Serial.println("Save-Restore");
	RomAddress = 0;
	moveData(op, 1, &id);
	for(int i=0;i<8;i++) moveData(op, sizeof(int), (byte *)(&rate[i])  );
	moveData(op, 1, &cal);
	moveData(op, 1, &temp);
	moveData(op, sizeof(int), (byte *)&turbid);
	moveData(op, 12,             (byte *)r.getPumps());
	moveData(op, 12*sizeof(int), (byte *)r.getDelays());
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

	Serial.println("iiN: Increase input flow for Lagoon N");
	Serial.println("ioN: Increase output flow for Lagoon N");
	Serial.println("diN: Decrease input flow for Lagoon N");
	Serial.println("doN: Decrease output flow for Lagoon N");
	Serial.println("fi : Measure in-flow rate");
	Serial.println("fo : Measure out-flow rate");
	Serial.println("w : calibrate at 200 (e.g. clear water)");
	Serial.print("l : calibrate at "); Serial.println(turbid);
}

boolean flow_command(char c1, char c2, int value)
{
	int direction;
	if (c2 == 0) // SINGLE LETTER COMMANDS
	{
		switch(c1)
		{
			case 'a':
				auto_temp = true;
				auto_flow = true;
				auto_valve = true;
				break;
			case 'c':
				Serial.println(cal);
				break;
			case 'i':
				Serial.write(id);
				Serial.println();
				break;
			case 'l':
				calibrateLED(turbid);
				analogWrite(TURBID_LED, cal);
				break;
			case 'm':
				auto_temp = false;
				auto_flow = false;
				auto_valve = false;
				break;
			case 'o':
				Serial.println(get_average(TURBIDITY));
				break;
			case 't':
				Serial.println(temp);
				break;
			case 'w':
				calibrateLED(CLEAR);
				analogWrite(TURBID_LED, cal);
				break;
			default:
				return false;
		}
		return true;
	}
	switch(c2) 
	{
		case 'i': direction = IN;   break;
		case 'o': direction = OUT;  break;
		default:  id = c2;
	}
	switch(c1)
	{
		case 'a':
			switch(c2)
			{
			case 'v': auto_valve = true; break;
			case 't': auto_temp = true; break;
			case 'f': auto_flow = true; break;
			}
			return true;
		case 'd':	rate[direction+(value-1)] -= 1000;
				saveRestore(SAVE);
				break;

		case 'f':	force_flow(direction);
				break;

		case 'h':
			if (auto_temp) return true;
			switch(c2)
			{
			case '+': digitalWrite(HEATER,1); break;
			case '-': digitalWrite(HEATER,0); break;
			default: return false;
			}
			break;
		case 'i':	id = c2;
				saveRestore(SAVE);
				break;
		case 'm':
			switch(c2)
			{
			case 'v': auto_valve = false; break;
			case 't': auto_temp = false; break;
			case 'f': auto_flow = false; break;
			}
			return true;
		case 'p':
			if (auto_flow) return true;
			switch(c2)
			{
			case '+':
				digitalWrite(PUMP_ORIGIN+(value-1), 1);
				break;
			case '-':
				digitalWrite(PUMP_ORIGIN+(value-1), 0);
				break;
			default :
				return false;
			}

		case 'u':	rate[direction+(value-1)] += 1000;
				saveRestore(SAVE);
				break;
		case 'v':
			if (auto_valve) return true;
			switch(c2)
			{
			case 'o': digitalWrite(VALVE,1); break;
			case 'c': digitalWrite(VALVE,0); break;
			default: return false;
			}
			break;

		default:
			return false;
	}
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
		if (!flow_command(is[0], is[1], value))
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
 * Return average of 10 readings of analog input
 */

float get_average(int Ain)
{
	for (int j=0; j<10; j++) {
		delay(10);
		reading[j] = analogRead(Ain);
	}
	return average(reading, 10);
}


/* PUMP CONTROL
 *
 * check_pumps() takes the time the pumps were last started
 * and returns the same value until the pumps are all shut off,
 * when it will return 0.
 */

int check_pumps(int started)
{
	int elapsed = millis() - started;
	for (int i = 0; i < 8; i++)
	{
		if (elapsed > rate[i] && digitalRead(PUMP_ORIGIN+i) == 1)
		{
			digitalWrite(PUMP_ORIGIN+i,0);
			int tm_err = (elapsed - rate[i])-1;
			if (tm_err > 2) {
#ifdef DEBUG
				Serial.print(i);
				Serial.print(" pump off error: ");
				Serial.println((elapsed - rate[i])-1);
#endif
			}
		}
	}
	if (pumps_on()) return started;
#ifdef DEBUG
	else		Serial.println("no pumps on");
#endif
	return 0;
}

boolean pumps_on(void)     /* Are any pumps on? */
{
	for (int i = 0; i < 8; i++)
		if ( digitalRead( PUMP_ORIGIN + i ) == 1)
			return true;
	return false;
}

int start_pumps(int direction)
{
#ifdef DEBUG
	if (pumps_on()) Serial.println("Pumps were already on!");
#endif
	int start = PUMP_ORIGIN + direction;
	int stop = start + 4;
	for (int i = start; i < stop; i++) digitalWrite(i,1);
	return millis();
}
	

void force_flow(int direction)
{
	int off_pumps;
	if (direction == IN)	off_pumps = OUT;
	else			off_pumps = IN;
	for(int i=0; i<4; i++ )
		digitalWrite(PUMP_ORIGIN+off_pumps+i,0);

	for(int t=0; t<3; t++)          // Three cycles
	{
		for(int i=0; i<4; i++ ) // For each pump
		{
			digitalWrite(PUMP_ORIGIN+direction+i,1);
#ifdef DEBUG
			Serial.print("Pump ");
			Serial.print(direction+i+1);
			Serial.println(" on");
#endif
			delay(rate[direction+i]);
			digitalWrite(PUMP_ORIGIN+direction+i,0);
#ifdef DEBUG
			Serial.print("Pump ");
			Serial.print(direction+i+1);
			Serial.println(" off");
#endif
			delay(1000);
		}
	}
	interval = 1 + (millis()/PUMP_CYCLE); // Restart pumping after delay
}



void flow_setup()
{
	auto_flow = true;  // Maintain Flow Control
	pinMode(HEATER, OUTPUT); // up to 7KW heater
	pinMode(VALVE, OUTPUT);	 // Bacterial Nutrient supply valve
	pinMode(LED, OUTPUT);	 // Arduino Indicator light
	interval = millis()/PUMP_CYCLE; // Intervals since startup (zero?)
	for (int i=0;i<8;i++) {
		pinMode(PUMP_ORIGIN+i, OUTPUT);
		digitalWrite(PUMP_ORIGIN+i, 0);
	}
#ifdef DEBUG
	Serial.print("Pumps initially set to: ");
	showpumps();
	Serial.print("On-times initially set to: ");
	for(int i=0;i<8;i++) { Serial.print(rate[i]);Serial.print("   "); }
	Serial.println("");
#endif
}


void check_flow()
{
#ifdef DEBUG
	if (cntr%(PUMP_CYCLE/30)==0)	showpumps();
#endif
	// Don't bother to check pumps if start_time is zero
	// (e.g. Last we checked, all pumps were off)

	if (start_time > 0)
		start_time = check_pumps(start_time);
	int current = millis() / PUMP_CYCLE;

	if ( current > interval )  // Time for a pumping cycle
	{
#ifdef DEBUG
		Serial.println("pump cycle");
#endif
		interval = current;
		start_time = start_pumps(cycle);
		if (cycle == IN) cycle = OUT;
		else             cycle = IN;
#ifdef DEBUG
		showpumps();
#endif
	}
	cntr++;
}

/* OPTICS FOR TURBIDITY MEASUREMENT */

void turbid_setup()
{
	auto_valve = true;  // Maintain turbidity with medium flow
	ai = 0; // Initialize index for rolling average array
	analogWrite(TURBID_LED, cal);
}

void check_turbid()
{
	if (get_average(TURBIDITY) > turbid)
		digitalWrite(VALVE, 1);
	else
		digitalWrite(VALVE, 0);
}

int LEDcalibration(int nominal)
{
int	ledout = 128;
int	change = ledout/2;
	analogWrite(TURBID_LED, ledout);
	delay(200);
float	avg = get_average(TURBIDITY);
int 	error = (int) (avg - (float)nominal);

	while ( abs(error) > 2)
	{
		if (error < 0) ledout = ledout - change;
		else           ledout = ledout + change;
		change = change/2 + 1;
		analogWrite(TURBID_LED, ledout);
		delay(200);
		avg = get_average(TURBIDITY);
		error = (int) (avg - (float) nominal);
	}
	return ledout;
}

/*
 * Find the LED current (PWM output) corresponding
 * to the specified sensor output.
 */

void calibrateLED(int level)
{
int tmp;
int avg = 0;

#ifdef DEBUG
	Serial.println("wait 5 seconds");
	delay(5000);
#endif
	for(int i=0; i<10; i++)
	{
		delay(300);
		tmp = LEDcalibration(level);
#ifdef DEBUG
		Serial.println(tmp);
#endif
		avg += tmp;
	}
	avg = (int) ((float)avg) / 10.0;
#ifdef DEBUG
	Serial.print(avg);
	Serial.print("  /10 ->  ");
	Serial.println(avg);
#endif
	cal = (byte) avg;
	saveRestore(SAVE);
}

#define P1 6
#define P2 4
#define P3 5
#define P4 3
#define MT 50

void steps(int p1, int s)
{
	if (s<1) return;
	digitalWrite(P4, 1);
	for(int i=0; i<s; i++) {
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
	

/*
 * setup()	1) Initializes serial link
 *		2) Restores settings from EEPROM
 *		2) Calls flow_setup (pumps)
 *		3) Calls turbid_setup (LED/Optics)
 */


void setup()
{
	Serial.begin(9600); // 9600 baud, 8-bits, no parity, one stop bit
	pinMode(ROTATOR, OUTPUT);
//	r = ROTARY(15);
	r.reset();
	temp_setup();

/*
	if (EEPROM.read(0)==0)	// First time
	{
		id = 'a';	// Default ID 'Aristotle'
		cal = 141;	// Default LED current/light level
		temp = 100;	// Temperature set point
		turbid = 500;	// Turbidity set point
		saveRestore(SAVE);
	}
	else
		saveRestore(RESTORE);
	flow_setup();
	turbid_setup();
*/
}


void loop() {
int cp;
	delay(1000);
	Serial.print(".");
	if (host.getMode() == OFF) {
		Serial.println("Setting host to priming mode");
		host.setMode(PRIME);
	}
	if (inducer.getMode() == OFF) {
		Serial.println("inducer to priming mode");
		inducer.setMode(PRIME);
	}
	if (waste.getMode() == OFF) {
		Serial.println("waste to priming mode");
		waste.setMode(PRIME);
	}
	host.check();
	inducer.check();
	waste.check();

	if (host.getMode() == FLOW)
	    Serial.println("host cell supply is now if FLOW mode");
	if (inducer.getMode() == FLOW)
	    Serial.println("Inducer supply is now if FLOW mode");
	if (waste.getMode() == FLOW)
	    Serial.println("Waste line is now if FLOW mode");
}



void oldloop() 
{
int p;
int cnt;
	Serial.println("Loop");
	while(1) {
		Serial.println(get_temp());
		delay(1000);
	}
	for (int i = 0; i< 19; i++)
	{
		r.check_drift(4);
		p = r.position();
		Serial.print("          POSITION ");Serial.println(p);
		if (r.check_dwell(p))    // Hold current position
			r.pass2(p); // Then move to next position
	}
	r.printTorques();

/*
	if (auto_flow)		// Check and update pumps
		check_flow();

	if (auto_temp)		// Check and update heater
		check_temp();

	if (auto_valve)		// Check and update valve
		check_turbid();

*/
}