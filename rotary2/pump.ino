/********************************************
 * PUMP control Version 1.0.0
 * by Peter Reintjes peterr@ncmls.org
 ********************************************/

#if ARDUINO >= 100
  #include "Arduino.h"
#else
  #include "WProgram.h"
#endif

#include "pump.h"

int PUMP::g_valve_state;
long int PUMP::g_start_time;
long int PUMP::g_prime_time;

PUMP::PUMP(char *name, int hold_pin, int avalve_pin, int pump_pin)
{
    strncpy(myname,name,20);
    pump = pump_pin;
    activate = avalve_pin;
    hold = hold_pin;
    pinMode(pump, OUTPUT);
    digitalWrite(pump,0);
    pinMode(activate, OUTPUT);
    digitalWrite(activate,0);
    pinMode(hold, OUTPUT);
    digitalWrite(hold,0);
    currentMode = OFF;
    g_start_time = millis();
}
 
void PUMP::setMode(int Mode)
{
    if (Mode == currentMode)
	return;
    if (Mode == PRIME) {
	digitalWrite(hold, 0);      // Close the valve
	digitalWrite(activate, 1);  // Activate High current/voltage
	digitalWrite(pump, 1);      // Turn on the Pump
	valve_state = OFF;          // Identify High current model
	g_prime_time = millis();
	who(); Serial.println(" now in priming mode");
    } else if (Mode == FLOW) {
	digitalWrite(pump, 0);                 // Turn off the pump
	if (valve_state == OFF) {
		digitalWrite(activate, 1);      // Activate high current/voltage source
		digitalWrite(hold, 1);          // Open the valve
		g_valve_state = valve_state = HIGH_POWER_MODE;  // Identify High current mode
		who(); Serial.println(" explicity placed in FLOW mode");
	}
    } else if (Mode == OFF) {
	digitalWrite(activate, 0);   // Everything off
	digitalWrite(hold,     0);
	digitalWrite(pump,     0);
	g_valve_state = valve_state = OFF;
	who(); Serial.println(" valve and pump turned OFF");
    }
    g_start_time = millis();
    currentMode = Mode;
}

void PUMP::who() { Serial.print(myname); } 

boolean PUMP::check() {
	if (g_valve_state == HIGH_POWER_MODE and duration() > VALVE_ACTIVATION_MS) {
		digitalWrite(hold, 1);
		digitalWrite(activate, 0);
		valve_state = LOW_POWER_MODE;
		g_valve_state = LOW_POWER_MODE;
		who();
		Serial.println(" switching to low power (valve open) mode");
	}
	return priming();
}

long int PUMP::duration() { return  millis() - g_start_time; }
int PUMP::getMode()       { return  currentMode; }

// Return the priming state
// but also automatically turn priming off (reverting to FLOW)
// when time exceeds MAX_PRIMING_TIME_MS
boolean PUMP::priming() {
	if (currentMode == PRIME) {
		if ( ( millis() - g_prime_time) > MAX_PRIMING_TIME_MS ) {
			who();
			Serial.print(" exceeded priming time ");
			setMode(FLOW);
			return false;
		}
		return true;
	}
	return false;
}
	

