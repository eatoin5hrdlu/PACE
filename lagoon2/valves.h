#ifndef VALVES_v1_h
#define VALVES_v1_h
#define LIBRARY_VERSION	1.0.0
//
// The number of valves and initial timings are specified in
// the constructor, rather than having a parameterized object.
//
// METHODS:
//
// void closeValve( valve ) - Close valve
// int getTime( valve )     - Return the valves "open time" value
// int setTime(int v, int t)- Set valves "open time" value
// void adjust(valve, value)- Add value to the valve's current "open time"
// boolean checkValves()    - Open valves if time for a new cycle and
//                            close valves after their "open time" 
// void openValve(int v)    - Open the valve for its "open time" duration
//

#include "Arduino.h"
#include "param.h"

class VALVES
{
 public:

  VALVES() {
    size = NUM_VALVES;
    valve_time[0] = 4000;  // OUTFLOW DEFAULT
    valve_pin[0] =  5;
    valve_open[0] = 0;

    valve_time[1] = 3000;  // HOSTCELL DEFAULT
    valve_pin[1] =  4;
    valve_open[1] = 0;

    valve_time[2] = 1000;  // INDUCER 1 ( Arabinose )
    valve_pin[2] =  3;
    valve_open[2] = 0;

    valve_time[3] = 0;     // INDUCER 2 ( cAMP )
    valve_pin[3] =  2;
    valve_open[3] = 0;

    cycletime = 20000;    // Ten second cycle time
  }

   void report() {
      for(int i=0; i<NUM_VALVES; i++)
      {
        Serial.println(valve_time[i]);
      }
   }

boolean checkValves(void) {
      unsigned long now = millis();

      if (now > lasttime + cycletime) { // Time to open valves
	Serial.println(".");
	for (int i=0; i<size; i++) {
	  if (valve_open[i] != 0) {
	    Serial.print("Valve was left open!!");
	    closeValve(i);
	  }
	  openValve(i);  // Open valves with positive on-time
	  delayMicroseconds(100);
	}
	lasttime = millis();

      } else // Check whether it is time to close any valves

	for (int i=0; i<size; i++) {
	  if (valve_open[i] != 0 && (valve_open[i]+valve_time[i]) < now)
	    {
	      closeValve(i);
	    }
	}
  }

// Valve will never open if it's "open time" is zero

  void openValve(int v)      {
    if (valve_time[v] != 0) {
      Serial.print("valve ");
      Serial.print(v);
      Serial.println(" open");
      digitalWrite(valve_pin[v],1);
       valve_open[v] = millis();
    }
  }

  void closeValve(int v)     {
    Serial.print("valve ");
    Serial.print(v);
    Serial.println(" closed");
    digitalWrite(valve_pin[v],0);
    valve_open[v] = 0;
  }
  int *getTimes()            { return &valve_time[0];   }
  int getTime(int vchar)       { return valve_time[(int)(vchar-'1')]; }
  int setTime(char vchar, int t)  { valve_time[(int)(vchar-'1')] = t; }
  void adjust(char vchar, int value) {
    int v = (int)(vchar - '1');
    Serial.print("adjusting valve "); Serial.println(v);
    if (value > 0 && valve_time[v] < cycletime + value)
      valve_time[v] += value;
    else if (value < 0 && valve_time[v] >= abs(value))
      valve_time[v] += value;
  }

 private:
  int size;                         // Number of valves
  byte     valve_pin[NUM_VALVES];
  int      valve_time[NUM_VALVES];
  long int valve_open[NUM_VALVES];  // When was the valve opened

  unsigned long lasttime;  // Beginning of current time interval
  unsigned long cycletime; // Cycle duration (always > valve on time)

  int testtime;
  };
#endif



