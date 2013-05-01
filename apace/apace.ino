/*
 * Turbidostat Controller:
 *  setup()                 : Assign pins, (restore EEPROM settings)
 *  loop()                  :
 *   sample()               : Store relevant samples ( Temp, Turbid )
 *    check()               : Check Temp and control HEATER
 *                          : Check Turbidity and control VALVE
 *   respondToRequest(void) : Check for serial input
 *    process(x,y,value)    : Respond to command "xy [value]"
 *
 *  int normalize(type, value) : Return (value-offset)*Scale for 'type' data
 *  dump(void)         : Dump configuration and sample data
 *  saveRestore(op)    : SAVE (RESTORE) configuration to (from) EEPROM
 *
 * Raw data from analogRead() is stored, but normalized data
 * (offset/scaled) is used for all tests and output.
 */
#include "EEPROM.h"
void printHelp(void)
{
	Serial.print("\n\n");
	Serial.println("d:  print all settings and stored data");
	Serial.println("h:  print this option menu");
	Serial.println("i:  print turbostat ID letter");
	Serial.println("ix: set turbostat ID to 'x'");
	Serial.println("x:  close turbidostat\n");
	Serial.println("ch <value> : set high temperature level");
	Serial.println("cl <value> : set low temperature level");
	Serial.println("cm <value> : set temperature delta");
	Serial.println("cs <value> : set temperature scaling");
	Serial.println("co <value> : set temperature offset");

	Serial.println("th <value> : set high turbidity level");
	Serial.println("tl <value> : set low turbidity level");
	Serial.println("tm <value> : set turbidity delta");
	Serial.println("ts <value> : set turbidity scaling");
	Serial.println("to <value> : set turbidity offset\n");
}
/*
 * Sample, store, convert, and return analog inputs
 */
bool alldata = false;  // Dump entire array, or just non-zero data
byte id = 'z'; // Zeno = unassigned, by default

int   scale[6]  = { 100, 100, 100, 100, 100, 100 };
int   offset[6] = {   0,   0,   0,   0,   0,   0 };
int   high[6]   = { 350, 400, 900, 900, 900, 900 };
int   low[6]    = { 390, 390, 300, 300, 300, 300 };
int   margin[6] = {  10,  10,  10,  10,  10,  10 };

// Set global 'RomAddress' and address will be bumped
// by successive calls to moveData( SAVE|RESTORE, size, ptr)

int      SAVE   = 1;
int   RESTORE   = 0;
int RomAddress  = 0;

// So far, variable space is 79 bytes

void moveData(int op, int size, byte *loc)
{
	for(int i=size;i>0;i--)
		if (op == SAVE)
			EEPROM.write(RomAddress++,*loc++);
		else
			*loc++ = EEPROM.read(RomAddress++);
}

byte identity(void)
{
	if (id == 'z' && EEPROM.read(0) != 0)
		id = EEPROM.read(0);
	return id;
}

void saveRestore(int op)
{
	int i;
	RomAddress = 0;
	moveData(op, 1, &id);
	for(i=0;i<6;i++) moveData(op, sizeof(int), (byte *)(&high[i])  );
	for(i=0;i<6;i++) moveData(op, sizeof(int), (byte *)(&low[i])   );
	for(i=0;i<6;i++) moveData(op, sizeof(int), (byte *)(&margin[i]));
	for(i=0;i<6;i++) moveData(op, sizeof(int), (byte *)(&offset[i]));
}

// Set scale[] to 1.0 on first startup
// so we don't do the initial restore more than once.

int normalize(int i, int analog)
{ 
	float s = ( (float) scale[i] ) / 100.0;
	return( (int) ( s * (analog-offset[i]) ) ); // Offset, then scale
//	return( ((int)( s * analog )) - offset[i]); // Scale, then offset
}

int TEMPERATURE = 0;  // Analog pin numbers
int TURBIDITY   = 1;

int pushButton = 2;
int HEATER      = 9;  // Digital pin numbers
int VALVE       = 10;
int LED         = 13;

void debug(void)
{
static int lastValue;
	int buttonState = digitalRead(pushButton);
	if (buttonState != lastValue) {
		lastValue = buttonState;
		Serial.println(buttonState);
		digitalWrite(LED, !buttonState);
	}
}

/* The maximum variable space on Arduino is 2K.
 * Do not allocate space for much more than 200 samples
 * or the Arduino program will fail without warning.
 * 
 * 200 samples each of Temperature and Turbidity => 800 bytes
 *	1 sample pair every five minutes for 16.6 hours
 *	1 sample pair every half hour for 4.1 days
 *
 * We can easily double if the valid temp/turbidity values
 * fit into one byte (25% of the full 10-bit analog range)
 */

int numSamples = 200;
int  temperature[200];
int    turbidity[200];

int sampleIndex = 0;
static bool toggle = false;	  // Arduino operation indicator
static long int clock  = 0,       // Counts major loop iterations (~0.1 sec)
		control = 5000,    // Update time for HEATER or VALVE
		recording = 10000; // 10000:10s, 300000:5 min, 1800000:30 min
/*
 * sample()
 *  'clock' is incremented every time sample() is called ( ~0.1 sec )
 *  Data is recorded after 'recording' clock ticks.
 *  HEATER(Temp) and VALVE(Turbidity) outputs updated after 'control' ticks.
 *
 * [ Value:Time ] -- [ 10000:10 sec ] [ 300000:5 min ] [ 1800000 : 30 min ]
 */
void sample(void)
{
	clock++;
	if (clock % recording == 0)
	{
		sampleIndex = (sampleIndex + 1) % numSamples;  // Move pointer
		toggle = !toggle;
		digitalWrite(LED, toggle);          // Change indicator
	}
	if (clock % control == 0)
	{
		temperature[sampleIndex] = analogRead(TEMPERATURE);
		turbidity[sampleIndex] = analogRead(TURBIDITY);
		check();
	}
}

// Print configuration settings, then all data as a time series
// Seconds   Temperature   Turbidity
//    0          334          680
//
void dump(void)
{
  int i;
  Serial.print("clock "); Serial.println(clock);
  Serial.print("ch "); Serial.println(high[TEMPERATURE]);
  Serial.print("cl "); Serial.println(low[TEMPERATURE]);
  Serial.print("cm "); Serial.println(margin[TEMPERATURE]);
  Serial.print("cs "); Serial.println(scale[TEMPERATURE]);
  Serial.print("co "); Serial.println(offset[TEMPERATURE]);

  Serial.print("th "); Serial.println(high[TURBIDITY]);
  Serial.print("tl "); Serial.println(low[TURBIDITY]);
  Serial.print("tm "); Serial.println(margin[TURBIDITY]);
  Serial.print("ts "); Serial.println(scale[TURBIDITY]);
  Serial.print("to "); Serial.println(offset[TURBIDITY]);

  Serial.println("DATA");
  Serial.println("Time,Temperature,Turbidity");
// 'recording' is modulus for timing
// Roughly: 10000:10s, 60000: 1 min, 300000:5 min, 1800000:30 min
// So, divide by 1000 to get seconds, or 60000 for minutes
// Hardwired now for minutes (as lowest measurement interval)
  int timeDelta = max(1, recording/10000); // 60000 for minutes, 10000 for debugging
  int minutes = 1;
  int index = 1;

  for(i=sampleIndex+1; i<numSamples; i++)    // Print oldest samples first
  {
    if (alldata || temperature[i] != 0) {
        Serial.print(index);
	index++;
	Serial.print(",");
	Serial.print(minutes);
	minutes += timeDelta;
	Serial.print(",");
	Serial.print(normalize(TEMPERATURE, temperature[i]));
	Serial.print(",");
	Serial.println(normalize(TURBIDITY, turbidity[i]));
    }
  }
  for(i=0; i<sampleIndex+1; i++)  // Ending with the most recent data
  {
    if (alldata || temperature[i] != 0) {
        Serial.print(index);
	index++;
	Serial.print(",");
	Serial.print(minutes++);
	minutes += timeDelta;
	Serial.print(",");
	Serial.print(normalize(TEMPERATURE, temperature[i]));
	Serial.print(",");
	Serial.println(normalize(TURBIDITY, turbidity[i]));
    }
  }
}

void check(void)
{
static int temp_msg, turb_msg;

	int temp = normalize(TEMPERATURE, temperature[sampleIndex]);
	if (temp < low[TEMPERATURE]) {
		if (temp_msg != 1) {
//			Serial.println("Heater on");
			temp_msg = 1;
		}
		digitalWrite(HEATER, 1);
	} else if (temp > high[TEMPERATURE]) {
		if (temp_msg != 2) {
//			Serial.println("Heater off");
			temp_msg = 2;
		}
	        digitalWrite(HEATER, 0);
	}

	int turbid = normalize(TURBIDITY, turbidity[sampleIndex]);
	if (turbid > high[TURBIDITY]) {
		if (turb_msg != 1) {
//			Serial.println("Valve open");
			turb_msg = 1;
		}
		digitalWrite(VALVE, 1);
	} else if (turbid < low[TURBIDITY]) {
		if (turb_msg != 2) {
//			Serial.println("Valve closed");
			turb_msg = 2;
		}
		digitalWrite(VALVE, 0);
	}
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
		if (is.length() > 3)
			value = atoi(&is[3]);
		process(is[0], is[1], value);
//		Serial.println("[" + is + "]");
	}
}

void process(char c1, char c2, int value)
{
	switch(c1) {
		case 'c' :
			switch(c2)  {
				case 'h' : high[TEMPERATURE] = value;  break;
				case 'l' : low[TEMPERATURE] = value;   break;
				case 'm' : margin[TEMPERATURE] = value;break;
				case 'o' : offset[TEMPERATURE] = value;break;
				case 's' : scale[TEMPERATURE] = value;break;
				default  : Serial.print("Ignoring [c");
					   Serial.write(c2);
					   Serial.println("]");
			}
			break;
		case 'd':
			dump();
			break;
		case 'h':
			printHelp();
			break;
		case 'i':
			if ( c2 == 0 ) {
				Serial.write(id);
				Serial.println();
			}
			else	       id = c2;
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
		case 's' :
			saveRestore(SAVE);
			
			break;
		case 't' :
			switch(c2)  {
				case 'h' : high[TURBIDITY] = value;  break;
				case 'l' : low[TURBIDITY] = value;   break;
				case 'm' : margin[TURBIDITY] = value;break;
				case 'o' : offset[TURBIDITY] = value;break;
				case 's' : scale[TURBIDITY] = value;break;
				default  : Serial.print("Ignoring [t");
					   Serial.write(c2);
					   Serial.println("]");
			}	
			break;
		default :
			Serial.print("Ignoring [");
			Serial.write(c1);
			Serial.println("]");
	}
}

void setup() {        // setup() runs on power-up or after RESET

  Serial.begin(9600); // 9600 baud, 8-bits, no parity, one stop bit

  pinMode(pushButton, INPUT);  // DEBUG
  pinMode(HEATER, OUTPUT);     // up to 7KW heater
  pinMode(VALVE, OUTPUT);      // Bacterial Nutrient supply valve
  pinMode(LED, OUTPUT);        // Arduino Indicator light

//  Serial.print("Program uses ");
//  Serial.print( sizeof(int) * numSamples * 2 + 79 );
//  Serial.println(" bytes of (SRAM) memory");
    if (EEPROM.read(0) != 0) {
	saveRestore(RESTORE);
    }
}

void loop() 
{
	sample();           // Gather the data
//	debug();            // Give us feedback on processing (pushButton=pin2)
	delay(2);           // Delay between reads for stability
	respondToRequest(); // Handle queries from computer
}


