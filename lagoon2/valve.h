#ifndef VALVE5_v1_h
#define VALVE5_v1_h
#define LIBRARY_VERSION	1.0.0
// FROM LAGOON2
// The number of valves and initial timings are specified in
// the constructor, rather than having a parameterized object.
//
// METHODS:
//
// void closeValve( valve ) - Close valve
// int getTime( valve )     - Return the valves "open time" value
// int setTime(int v, int t)- Set valves "open time" value
// void adjust(valve, value)- Add value to the valve's current "open time"
// boolean checkValve()    - Open valves if time for a new cycle and
//                            close valves after their "open time" 
// void openValve(int v)    - Open the valve for its "open time" duration
//
#include "Arduino.h"
#include "param.h"
#include <Servo.h>


void setup()
{
  Serial.begin(9600);
}

class VALVE5
{
 private :
  Servo myservo;

 public:
  VALVE5(int pin) {
    int i;
    size = 4;
    disabled = false;
    myservo.attach(pin);
    cycletime = DEFAULT_CYCLETIME*1000; // Cycle in seconds, store as ms
  }

  void disable(void)
  {
    disabled = true;
    myservo.write(0);
    current = 0;
  }

  void setup_valve(int v, int tm) {
    valve_angle[v] =  v*(180/(size-1));  // Angles are 0, 45, 90, 135, 180
    valve_open[v] = 0;
    if (v == size-1)
      valve_time[v] = tm;
    else
      valve_time[v] = tm/2;
  }

  int getSize(void)           { return size;  }
  int getCycletime(void)      { return cycletime/1000;  }
  void setCycletime(int secs) { cycletime = secs*1000; }

  // report() takes a pointer to a buffer 
   void report(char *reply) {
     char *cp = reply;
     sprintf(cp,"valvetimes([");
     cp += 12;
     for(int i=0; i<size; i++)
       if (valve_pin[i] != -1) {
	 sprintf(cp, "%4d,",valve_time[i]);
	 cp += 5;
       }
     sprintf(cp-1,"]).");
   }

   void next_valve(void)
   {
    if (current == 0)            up = true;  // First half of sequence
    if (up && current == size-1) up = false; // Second half
    valve_open[current] = 0;
    if (up) current = current + 1;
    else    current = current - 1;
    myservo.write(valve_angle[current]);
    if (current != 0)
      valve_open[current] = millis();
   }

boolean checkValve(void) {
  int i;
  unsigned long now = millis();
  if (disabled) return;

      // Beginning of cycle
      if (now > lasttime + cycletime) // Time to start valve sequence
	{
	  current = 0;
	  lasttime = now;
	  next_valve();
	}
      if (valve_open[current] != 0 && now > lasttime + valve_open[current])
	next_valve();
      return true;
}

  int *getTimes()               { return &valve_time[0];   }
  int getTime(int vchar)        { return valve_time[(int)(vchar-'0')]; }
  int setTime(char vchar, int t){ valve_time[(int)(vchar-'0')] = t; }

 private:
  int size;                         // Number of positions
  int current;                      // Current position
  boolean up;
  boolean enabled;
  byte     flow;
  byte     valve_angle[NUM_VALVES];
  int      valve_time[NUM_VALVES];
  long int valve_open[NUM_VALVES];  // When was the valve opened

  unsigned long lasttime;  // Beginning of current time interval
  unsigned long cycletime; // Cycle duration (always > valve on time)

#endif


