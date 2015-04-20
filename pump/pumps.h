#ifndef PUMPS_v1_h
#define PUMPS_v1_h
#define LIBRARY_VERSION	1.0.0
//
// The number of valves and initial timings are specified in
// the constructor, rather than having a parameterized object.
//
// METHODS:
//
// void startPump( pump ) - Start pump
// void stopPump( pump ) - Stop pump
// void openValve( valve ) - Open valve
// void closeValve( valve ) - Close valve
//

#include "Arduino.h"
#include "param.h"
#define DEBUG 1

class PUMPS
{
 public:

  PUMPS(int num) {
    size = num;
    int i;
    valve_pin[0] = NUTRIENT_VALVE;
    pump_pin[0] = NUTRIENT_PUMP;
    valve_pin[1] = CELLSTAT_VALVE;
    pump_pin[1] = CELLSTAT_PUMP;
    valve_pin[2] = INDUCER1_VALVE;
    pump_pin[2] =  INDUCER1_PUMP;
    valve_pin[3] = INDUCER2_VALVE;
    pump_pin[3] =  INDUCER2_PUMP;
    valve_pin[4] = INHIBITOR_VALVE;
    pump_pin[4] =  INHIBITOR_PUMP;
  }

  int getSize(void)           { return size;  }

  void openValve(int v)      {
#ifdef DEBUG
      Serial.print("valve ");
      Serial.print(v);
      Serial.println(" open");
#endif
      digitalWrite(valve_pin[v],1);
      valve_state[v] = 1;
  }

  void closeValve(int v)     {
#ifdef DEBUG
    Serial.print("valve ");
    Serial.print(v);
    Serial.println(" closed");
#endif
    digitalWrite(valve_pin[v],0);
    valve_state[v] = 0;
  }

  void pumpStart(int p)      {
#ifdef DEBUG
      Serial.print("pump ");
      Serial.print(p);
      Serial.println(" started on pin ");
      Serial.print(pump_pin[p]);
#endif
      digitalWrite(valve_pin[p],0);
      digitalWrite(pump_pin[p],1);
  }

  void pumpStop(int p)     {
#ifdef DEBUG
    Serial.print("pump ");
    Serial.print(p);
    Serial.println(" off");
#endif
    digitalWrite(pump_pin[p],0);
    digitalWrite(valve_pin[p],valve_state[p]); // Return valve state
  }

 private:
  int size;                         // Number of pump/valve units
  byte     valve_pin[MAX_VALVES];
  byte     pump_pin[MAX_PUMPS];
  int      valve_state[MAX_VALVES];  // Previous state of valve

  int valvePinPosition(int pin) {
    int i;
    for(i=0;i<size;i++)
      if (valve_pin[i] == pin)
	return i;
    return -1;
  }

  int valvePositinPin(int pos) {
    if (pos < size && valve_pin[pos] != -1)
      return(valve_pin[pos]);
    return -1;
  }

  int pumpPinPosition(int pin) {
    int i;
    for(i=0;i<size;i++)
      if (pump_pin[i] == pin)
	return i;
    return -1;
  }

  int pumpPositionPin(int pos) {
    if (pos < size && pump_pin[pos] != -1)
      return(pump_pin[pos]);
    return -1;
  }

  };
#endif



