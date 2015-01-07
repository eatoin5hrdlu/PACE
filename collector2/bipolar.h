#ifndef BIPOLAR_v1_h
#define BIPOLAR_v1_h
#define LIBRARY_VERSION	1.0.0

#include "Arduino.h"

//
// Create BIPOLAR(number-of-positions, time-between-samples)
//

class BIPOLAR
{
 public:

#define ALIQUOT     4000    // Sample for 10 seconds
#define DRIP        2000    // wait 10 seconds for dripping
#define MINUTE      10000   // 10000 ms for testing, 60000 for actual minutes
#define SPACE       100     // Number of steps per position

#define STOPPED  0
#define FORWARD  1
#define REVERSE  2

#define LIMITSWITCH 8

  // PLATFORM MOTOR PINS
#define DIS1      9
#define DIS2      10
#define PHASE1    11
#define PHASE2    12

  // SAMPLE VALVE MOTOR PINS
#define M1     6
#define E1     7

#define mydelay   delay(2)

  BIPOLAR(int samples, int stime) {
    num_samples = samples;
    sample_time = stime;

    pinMode(LIMITSWITCH, INPUT_PULLUP);
    pinMode(M1, OUTPUT);    digitalWrite(M1,0);
    pinMode(E1, OUTPUT);    digitalWrite(E1,0);
    pinMode(DIS1,OUTPUT);   digitalWrite(DIS1,1);
    pinMode(DIS2,OUTPUT);   digitalWrite(DIS2,1);
    pinMode(PHASE1,OUTPUT); digitalWrite(PHASE1,0);
    pinMode(PHASE2,OUTPUT); digitalWrite(PHASE2,0);
    counter = 0;
    state = STOPPED;
  }
  
  void stop(void) {
    digitalWrite(DIS1,1);
    digitalWrite(DIS2,1);
    digitalWrite(PHASE1,0);
    digitalWrite(PHASE2,0);
    state = STOPPED;
      }

  void forward(void) {
    if (state == STOPPED) {
      digitalWrite(PHASE1,0);
      digitalWrite(PHASE2,0);
      digitalWrite(DIS1, 0);
      digitalWrite(DIS2, 0);
      state = FORWARD;
    }
    // Phase1 = 0 Phase2 = 0 both are enabled
      mydelay;
      digitalWrite(DIS1, 1);     // Phase1(0) off
      mydelay;
      digitalWrite(PHASE1,1);    // Phase1(1) on
      digitalWrite(DIS1, 0);
      mydelay;
      digitalWrite(DIS2, 1);     // Phase2(0) off
      mydelay;
      digitalWrite(PHASE2,1);   // Phase2(1) on
      digitalWrite(DIS2, 0);     
      mydelay;
      digitalWrite(DIS1, 1);    // Phase1(1) off
      mydelay;
      digitalWrite(PHASE1,0);   // Phase1(0) on
      digitalWrite(DIS1, 0);
      mydelay;
      digitalWrite(DIS2, 1);    // Phase2(1) off
      mydelay;
      digitalWrite(PHASE2,0);   // Phase2(0) on
      digitalWrite(DIS2, 0);
      // Phase1 and 2 are both zero and active no delay
  }

  void reverse(void) {
    if (state == STOPPED) {
      state = REVERSE;
      digitalWrite(PHASE1,0);
      digitalWrite(PHASE2,0);
      digitalWrite(DIS1, 0);
      digitalWrite(DIS2, 0);
    }
    // Phase1 = 0 Phase2 = 0 both are enabled
      mydelay;
      digitalWrite(DIS2, 1);     // disable Phase2(0)
      mydelay;
      digitalWrite(PHASE2,1);    // enable Phase2(1)
      digitalWrite(DIS2, 0);     // ON
      mydelay;
      digitalWrite(DIS1, 1);    // disable Phase1(0)
      mydelay;
      digitalWrite(PHASE1,1);   // enable Phase1(1)
      digitalWrite(DIS1, 0);    //  ON
      mydelay;
      digitalWrite(DIS2, 1);    // Phase2 1 off
      mydelay;
      digitalWrite(PHASE2,0);   // Phase2 0
      digitalWrite(DIS2, 0);    // ON
      mydelay;
      digitalWrite(DIS1, 1);    // Phase1 1 off
      mydelay;
      digitalWrite(PHASE1,0);   // Phase1 0 on
      digitalWrite(DIS1, 0);
      // Phase1 and 2 are both zero and active no delay
  }


  void run() {
    Serial.println("Run");
    Serial.println(digitalRead(LIMITSWITCH));
    while(digitalRead(LIMITSWITCH)) 
      reverse();
    Serial.println("Reset finished");

    for (int s=0; s < num_samples; s++)
      {
	Serial.println("Start sample time");
	for(int i=0; i<sample_time; i++) // Minutes til next sample
	  delay(MINUTE);
	Serial.println("move forward");
	for(int i=0; i<SPACE; i++) forward();
	stop();
	Serial.println("stopped");
	digitalWrite(M1,0); digitalWrite(E1,1); // Open Sample Valve
	Serial.println("start sampling");
	delay(ALIQUOT);
	digitalWrite(M1,1); digitalWrite(E1,1);  // Close Sample Valve
	delay(DRIP);
	Serial.println("done sampling");
      }
  }

 private:
  int state;    // Which state of the cycle STOP,FORWARD,REVERSE
  int counter;
  int num_samples;
  int sample_time;
  };
#endif
