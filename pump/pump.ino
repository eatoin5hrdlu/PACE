
#define EOT "end_of_data"
/*
 * Pump controller
 *
 * 1) Create N pump/valve controls
 * 2) Accept commands from main computer to:
 *     a) prime channel(s)
 *     b) turn pumps on/off  (valves always close when pumps are on)
 *     c) open/close valves
 * Commands:
 *  l0 :    light off
 *  l1 :    light on
 *  h0 :    Heater off
 *  h1 :    Heater on
 *  i  :    Indentify yourself
 *  m0 :    Mixer off
 *  m1 :    Mixer on
 *  a0 :    Auto modes off  (default for pumps)
 *  a1 :    Auto modes on
 *  oN :    Open valve N
 *  cN :    Close valve N  (also, auto_valve mode turned ON)
 *  pN1 :   Pump N on (valve closed)
 *  pN0 :   Pump N off (valve returns to previous state)
 */

#include "pumps.h"        // Includes param.h (change constants there)

PUMPS pumps = PUMPS(5);

/* EEPROM SAVE AND RESTORE OF ID AND CALIBRATION CONSTANTS */

#ifdef STANDALONE
#include "EEPROM.h"
int RomAddress  = 0;

byte id = 'p'; // p = Generic pump, by default

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
}
#endif

void mixer(byte v)
{
	if (v == 0)
		analogWrite(MIXER,0);
	else 
	    for(int i=3; i<11; i++) {
		analogWrite(MIXER, i*MIXERSPEED/10);
		delay(500);
 	    }
}

boolean pump_command(char c1, char c2, int value)
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
		case 'c':
			pumps.closeValve(c2-'0');
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
	//		digitalWrite(LED, d);
			break;
		case 'm':
			mixer(d);
			break;
		case 'o':
			pumps.openValve(c2-'0');
			break;
		case 'p':
			if (value > 0) {
				pumps.pumpStart(c2-'0');
			} else {
				pumps.pumpStop(c2-'0');
			}
			break;
		case 'r':
			saveRestore(RESTORE);
			break;
		case 's':
			saveRestore(SAVE);
			break;
		case 'v':
			pumps.openValve(c2-'0');
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
		if (!pump_command(is[0], is[1], value)) {
			Serial.println("bad command [" + is + "]");
			Serial.println(EOT);
		}
	}
}

/*
 * setup()	1) Initializes serial link
 *		2) Restores settings from EEPROM
 *		2) Calls pump_setup (pumps)
 */

boolean once;

void setup()
{

	pinMode(NUTRIENT_PUMP,  OUTPUT);
	digitalWrite(NUTRIENT_PUMP,   0);
	pinMode(NUTRIENT_VALVE,  OUTPUT);
	digitalWrite(NUTRIENT_VALVE,   0);

	pinMode(CELLSTAT_PUMP,  OUTPUT);
	digitalWrite(CELLSTAT_PUMP,   0);
	pinMode(CELLSTAT_VALVE,  OUTPUT);
	digitalWrite(CELLSTAT_VALVE,   0);

	pinMode(INDUCER1_PUMP,  OUTPUT);
	digitalWrite(INDUCER1_PUMP,   0);
	pinMode(INDUCER1_VALVE,  OUTPUT);
	digitalWrite(INDUCER1_VALVE,   0);

	pinMode(INDUCER2_PUMP,  OUTPUT);
	digitalWrite(INDUCER2_PUMP,   0);
	pinMode(INDUCER2_VALVE,  OUTPUT);
	digitalWrite(INDUCER2_VALVE,   0);

	pinMode(INHIBITOR_PUMP,  OUTPUT);
	digitalWrite(INHIBITOR_PUMP,   0);
	pinMode(INHIBITOR_VALVE,  OUTPUT);
	digitalWrite(INHIBITOR_VALVE,   0);
        analogWrite(MIXER, 0);

	Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
	if (EEPROM.read(0)==0 || EEPROM.read(0)==255)	// First time
	{
		id = 'p';	// Default Pump ID 
		saveRestore(SAVE);
	}
	else
	{
		saveRestore(RESTORE);
	}
	once = true;
}

void loop()
{
	respondToRequest();     // Respond to commands
	delay(100);
}
