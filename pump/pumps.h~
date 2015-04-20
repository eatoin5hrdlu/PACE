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
// #define DEBUG 1

class VALVES
{
 public:

  VALVES(int num) {
    size = num;
    int i;
    for(i=0;i<num;i++) valve_pin[i] = -1;

    cycletime = CYCLETIME*1000;  // Cycle time given in seconds
  }

  int getSize(void)           { return size;  }
  int getCycletime(void)      { return cycletime/1000;  }
  void setCycletime(int secs) { cycletime = CYCLETIME*1000; }

  int setValve(int pin, int time) {
    int i;
    for(i=0;i<size;i++) {
      if (valve_pin[i] == pin) { // Pin already assigned
	valve_time[i] = time;
	return i;
      }
      if (valve_pin[i] == -1) {  // First time
	valve_pin[i] = pin;
	valve_time[i] = time;
	return i;
      }
      return -1;
    }
  }

   void report() {
      for(int i=0; i<size; i++)
	if (valve_pin[i] != -1)
	  Serial.println(valve_time[i]);
   }

boolean checkValves(void) {
      unsigned long now = millis();

      if (now > lasttime + cycletime) { // Time to open valves
#ifdef DEBUG
	Serial.println(".");
#endif
	for (int i=0; i<size; i++) {
	  if (valve_open[i] != 0) {
#ifdef DEBUG
	    Serial.print("Valve was left open!!");
#endif
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
#ifdef DEBUG
      Serial.print("valve ");
      Serial.print(v);
      Serial.println(" open");
#endif
      digitalWrite(valve_pin[v],1);
       valve_open[v] = millis();
    }
  }

  void closeValve(int v)     {
#ifdef DEBUG
    Serial.print("valve ");
    Serial.print(v);
    Serial.println(" closed");
#endif
    digitalWrite(valve_pin[v],0);
    valve_open[v] = 0;
  }
  int *getTimes()            { return &valve_time[0];   }
  int getTime(int vchar)       { return valve_time[(int)(vchar-'1')]; }
  int setTime(char vchar, int t)  { valve_time[(int)(vchar-'1')] = t; }
  void adjust(char vchar, int value) {
    int v = (int)(vchar - '1');
#ifdef DEBUG
    Serial.print("adjusting valve "); Serial.println(v);
#endif
    if (value > 0 && valve_time[v] < cycletime + value)
      valve_time[v] += value;
    else if (value < 0 && valve_time[v] >= abs(value))
      valve_time[v] += value;
  }

 private:
  int size;                         // Number of valves
  byte     valve_pin[MAX_VALVES];
  int      valve_time[MAX_VALVES];
  long int valve_open[MAX_VALVES];  // When was the valve opened

  unsigned long lasttime;  // Beginning of current time interval
  unsigned long cycletime; // Cycle duration (always > valve on time)

  int testtime;
  int pinPosition(int pin) {
    int i;
    for(i=0;i<size;i++)
      if (valve_pin[i] == pin)
	return i;
    return -1;
  }
  int positinPin(int pos) {
    if (pos < size && valve_pin[pos] != -1)
      return(valve_pin[pos]);
    return -1;
  }

  };
#endif



