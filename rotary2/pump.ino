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

int PUMP::g_start_time;
int PUMP::g_valve_state;
int PUMP::g_prime_time;

PUMP::PUMP(char *name, int pump_pin, int avalve_pin, int hold_pin)
{
    strncpy(myname,name,20);
    pump = pump_pin;
    activate = avalve_pin;
    hold = hold_pin;
    pinMode(pump, OUTPUT);
    pinMode(activate, OUTPUT);
    pinMode(hold, OUTPUT);
    currentMode = OFF;
    start_time = millis();
}
 
void PUMP::setMode(int Mode)
{
    if (Mode == currentMode)
	return;
    if (Mode == PRIME) {
	digitalWrite(pump, 1);      // Turn on the Pump
	digitalWrite(hold, 0);      // Close the valve
	valve_state = OFF;          // Identify High current model
	prime_time = millis();
	g_prime_time = millis();
    } else if (Mode == FLOW) {
	digitalWrite(pump, 0);                 // Turn off the pump
	if (valve_state == OFF) {
		digitalWrite(activate, 1);      // Activate high current/voltage source
		digitalWrite(hold, 1);          // Open the valve
		valve_state = HIGH_POWER_MODE;  // Identify High current mode
		g_valve_state = HIGH_POWER_MODE; // Identify High current mode
	}
    } else if (Mode == OFF) {
	digitalWrite(activate, 0);   // Everything off
	digitalWrite(hold,     0);
	digitalWrite(pump,     0);
    }
    g_start_time = start_time = millis();
    currentMode = Mode;
}

void PUMP::who() { Serial.print("(");Serial.print(myname);Serial.print(") ");} 

boolean PUMP::check() {
	if (g_valve_state == HIGH_POWER_MODE and duration() > VALVE_ACTIVATION_MS) {
		digitalWrite(hold, 1);
		digitalWrite(activate, 0);
		valve_state = LOW_POWER_MODE;
		g_valve_state = LOW_POWER_MODE;
		who();
		Serial.println("Valve activation power off (low power mode)");
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
			Serial.println("Priming time exceeded");
			setMode(FLOW);
			return false;
		}
		return true;
	}
	return false;
}
	

