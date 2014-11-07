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

PUMP::PUMP(int pump_pin, int avalve_pin, int hold_pin)
{
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
	digitalWrite(pump, 1);         // Turn on the Pump
	digitalWrite(activate, 1);     // Open the valve
	valve_state = HIGH_POWER_MODE; // Identify High current mode
	prime_time = millis();
    } else if (Mode == FLOW) {
	digitalWrite(pump, 0);                 // Turn off the pump
	if (valve_state == OFF) {
		digitalWrite(activate, 1);     // Open the valve
		valve_state = HIGH_POWER_MODE; // Identify High current mode
	}
    } else if (Mode == OFF) {
	digitalWrite(activate, 0);   // Everything off
	digitalWrite(hold,     0);
	digitalWrite(pump,     0);
    }
    start_time = millis();
    currentMode = Mode;
}

boolean PUMP::check() {
	if (valve_state == HIGH_POWER_MODE and duration() > VALVE_ACTIVATION_MS) {
		digitalWrite(hold, 1);
		digitalWrite(activate, 0);
		valve_state = LOW_POWER_MODE;
		Serial.println("Valve put in low power mode");
	}
	return priming();
}

long int PUMP::duration() { return  millis() - start_time; }
int PUMP::getMode()       { return  currentMode; }

// Return the priming state
// but also automatically turn priming off (reverting to FLOW)
// when time exceeds MAX_PRIMING_TIME_MS
boolean PUMP::priming() {
	if (currentMode == PRIME) {
		if ( ( millis() - prime_time) > MAX_PRIMING_TIME_MS ) {
			Serial.println("Priming time exceeded");
			setMode(FLOW);
			return false;
		}
		return true;
	}
	return false;
}
	

