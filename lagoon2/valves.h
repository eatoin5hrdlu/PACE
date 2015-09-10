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
#define INFLOW  1
#define OUTFLOW 2
#define ALLFLOW (INFLOW|OUTFLOW)
#define NOFLOW  0
#define ENABLED(v) (valve_dir[v]&flow)

class VALVES
{
 public:

  VALVES() {
    size = NUM_VALVES;
    //   setup_valve(Valve#,Pin#,Time,Direction)
    setup_valve(0, 5, 40000, OUTFLOW);  // Valve#, Pin#, TimeMs, Direction
    setup_valve(1, 4, 35000, INFLOW);   // HOST CELLS
    setup_valve(2, 3,  5000, INFLOW);   // INDUCER 1 ( Ara )
    setup_valve(3, 2,     0, INFLOW);   // INDUCER 2 ( cAMP )
    flow = ALLFLOW;
    cycletime = 60000;    // One minute cycle time
  }

  void setup_valve(int v, byte pn, int tm, byte dir) {
    valve_time[v] = tm;
    valve_pin[v] =  pn;
    valve_dir[v] = dir;
    valve_open[v] = 0;
  }

  void enable_outflow(void)  { flow |= OUTFLOW;  }
  void disable_outflow(void) { flow &= ~OUTFLOW; }
  void enable_inflow(void)   { flow |= INFLOW;   }
  void disable_inflow(void)  { flow &= ~INFLOW;  }

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
  byte     flow;
  byte     valve_pin[NUM_VALVES];
  byte     valve_dir[NUM_VALVES];
  int      valve_time[NUM_VALVES];
  long int valve_open[NUM_VALVES];  // When was the valve opened

  unsigned long lasttime;  // Beginning of current time interval
  unsigned long cycletime; // Cycle duration (always > valve on time)

  int testtime;
  };
#endif



