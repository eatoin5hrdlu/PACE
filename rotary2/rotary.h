#ifndef ROTARY_v1_h
#define ROTARY_v1_h
#define LIBRARY_VERSION	1.0.0

#include "Arduino.h"
#include "param.h"

// States of the Rotary Machine

#define IDLE       0
#define DRIFTING   2
#define HOLDING    4


//
// Create ROTARY(number-of-positions)
// Set (three) values for each position
// Return pointers to stored data (byte[size],int[size],int[size])
// Reset         ( moves to origin )
// Next Position ( returns necessary delay )
//

class ROTARY
{
 public:

  ROTARY(int s) {
    size = s; 
    incr = 2;
    for(int i=0; i<size; i++) {  // DEFAULT VALUES
      pause[i] = NOMINAL_TIME;   // msec at each position
      holding[i] = HOLDING_TORQUE;
      passing[i] = NOMINAL_TORQUE;  // Lowest passing torque is 44?
      pump[i] = 13;        // No pumping
    }
    Serial.print(" Created Rotary with "); Serial.print(size);
    Serial.println(" positions");
  }

  void set(byte pos, byte pin, int ms)  {
    pump[pos] = pin; pause[pos] = ms;
  }
    
  void reset(void)    {
    Serial.print("reset\n");
    //    pos = position();
    //    while(pos != 0)  move_nextpos();
  }

  void run(void) {
    reset();
    while(1) {
      int dt = move_nextpos();
      pumpon();
      delay(dt);
      pumpoff();
    }
  }

  int position(void) {
    int pos = -1;
    int ar;
    while (pos == -1) {
      ar = goodRead(POINTER) + OFFSET;
      Serial.print("pos: "); Serial.println(ar);
      if (ar < 0) ar = 0;
      ar = ar>>6;
      if (ar < size) /* Valid Position */
	pos = ar;
      else check_drift(0);
    }
    return (pos);
  }

  int move_nextpos(void)      {
    digitalWrite(9,0);
    delay(100);
    set_torque( passing[pos]);
    while ( (7*analogRead(1)/1023) == pos ) ;
    digitalWrite(ROTATOR, 0);
    pos++;
    return(pause[pos]);
  }

  void pumpon(void)  {
    if( pump[pos] )
      digitalWrite(pump[pos],1);
  }


  // Stay at current position for the appropriate time
  boolean dwell(void) { dwell(position()); }

  // If we lose contact with the current position during
  // the dwell phase, it means that we should reduce the
  // pulse-width value for the valve-open(dwell) state.

  boolean dwell(int cp) {
    Serial.print("HOLDING");
    int done = 0;
    set_torque( holding[cp]+2);
    delay(10);
    set_torque(holding[cp]);

    while (done < pause[cp] && AT(cp,2) ) {
      delay(100-testtime);
      done += 100;
    }
    if ( ptrmoved ) {
      analogWrite(ROTATOR,0);
      holding[cp] -= 1;
      Serial.print("--punchthrough-- Holding torque ");
      Serial.print(holding[cp]);
      Serial.print(" adjusted down for position ");
      Serial.println(cp);
      return false;
    }
    Serial.print("(");Serial.print(pause[cp]);Serial.println(")");
    state = HOLDING;
    return true;
  }

  /*
   * Checking on the Dwell() is where we need to make sure
   * the pointer did not push through the current position
   * (we might adjust holding[position] down).
   *
   *    if (!dwell(cp))
   *             pass(cp);
   */

  boolean check_dwell(int cp) {
    if (state == HOLDING) {                          // CHECK IT
      unsigned long now = millis();
      if ( (lasttime + pause[cp] + add[cp]) < now) { // STILL TIMING
	if ( AT(cp,2) )                              // STILL IN POSITION
	  return true;
	else {                                       // OUT OF POSITION
	  pushed_through(cp);
	  state = IDLE;
	  return false;
	}
      } else {
	add[cp] = now-(lasttime + pause[cp] + add[cp]); // STORE OVERRUN
	return false;                                   // FINISHED
      }
    } else {
      set_torque(holding[cp]);                // SET HOLDING TORQUE
      lasttime = millis();                    // START TIMING
      state = HOLDING;
      return true;
    }
    Serial.println("NOTREACHED");
    return true;
  }

  // When dwell ends early, we need to keep track of
  // the un-passed time, so we can make it up next time.

  void pushed_through(int cp)  {
	  analogWrite(ROTATOR,0);
	  int delta = millis() - lasttime;       // Actual time passed
	  if ( delta < ( pause[cp] + add[cp] ) ) // Not enough
	    add[cp] =  ( pause[cp] + add[cp] ) - delta;

	  holding[cp] -= incr;    // Adjust holding value down
	  Serial.print("--punchthrough-- torque=");
	  Serial.print(holding[cp]);
	  Serial.print(" decremented for position ");
	  Serial.println(cp);
  }

  // Start drift cycle if state == IDLE
  // Check time when state == DRIFTING
  // Returns false when at end of drift time
  //
  //  if (!check_drift(3))
  //     if (!check_dwell(cp))
  //        pass(cp);
  //
   boolean check_drift(int delay_seconds) {
     if (state == DRIFTING) {
       if (lasttime + 1000 * delay_seconds > millis() )
	 {
	   state = IDLE;
	   return false;
	 }
       else
	 return true;

     } else if (state == IDLE ) {

       lasttime = millis();
       analogWrite(ROTATOR, DRIFT_TORQUE*3 );
       delay(30);
       analogWrite(ROTATOR, (3*DRIFT_TORQUE)/2 );
       delay(24);
       analogWrite(ROTATOR, 1 + DRIFT_TORQUE/2 );
       state = DRIFTING;
       return true;
     } else 
       return false;
  }

   int nextpos(int pin) {
     if (pin < (size-1)) return pin + 1;
     else return 0;
   }

   //#define DEBUG 1

   int goodRead(int Ain)
   {
     int n = 0;
     int a[3];
     int v;
     int pass = 0;
     a[n] = analogRead(Ain);
     while(n<3 && pass<3) {
       v = analogRead(Ain);
       if ( abs(a[n]-v) < 4) { n++; a[n] = v; pass = 0;}
       else { n = 0; a[0] = v; pass++; /* big change */ }
     }
     if (pass == 3) return v;
     v = 0;
     for (n=0;n<3;n++) v += a[n];
     return v/3;
   }

   /*
    * The heart of the pointer's sensitivity (how it knows
    * when it has gone too far) is to take enough readings
    * to quickly tell the difference between noise and
    * moving past the current position.
    *
    *  testtime -- time spent verifying position
    *  ptrmoved -- boolean value last returned
    *
    */
     boolean AT(int p, int tries) {
       int v = 1025;    // Valid readings are smaller than this
       int prev;        // Previous reading
       int progress = 0;
       ptrmoved = true;
       testtime = 0;
       for(int i=0; i<tries; i++ ) {
	 delay(1);
	 testtime += 2;
	 prev = v;
	 v = goodRead(POINTER)+OFFSET;
	 //	 Serial.println(v);
	 if ( v > prev + 2 ) progress++;
	 else                progress = 0;
	 if (progress > 3)  return false; // Clearly drifting
	 if (v < 0) v = 0; // look out for negative OFFSET
	 if ( v>>6 == p ) { ptrmoved = false; return true; }
       }
#ifdef DEBUG
     Serial.print(p);
     Serial.print(" assume moved ");
     Serial.println(v>>6);
#endif
     return false;
   }

     /*
      * We keep track of the time spent making the pass
      * in case we want to subtract it from the dwell 
      * time on the next pass.
      */

   void pass2(int c) {
     Serial.println("pass2");
     int torque = max(holding[c], passing[c]-24);
     int cnt = 0;
     int timeused = 0;
     while ( AT(c,4) ) {  // Was AT(c,6), before goodRead() filter
       if (++cnt % 22 == 1) { // Bump up the torque
	 set_torque(torque);
	 torque += 1;
	 timeused += testtime;
       }
       delay(2);
       timeused += 2;
     }
     analogWrite(ROTATOR, 0);
     passing[c] = torque - 2; // was -4
     holding[c] = passing[c] - (passing[c]/3);  // 66% of total
     Serial.print(passing[c]); Serial.println(" exit pass2");
   }

   void pass(void) { pass(position()); }

   void pass(int cp) {
    int current;
    int pulsetime = 34;
    int tries = 0;
    if (passing[cp] > NOMINAL_TORQUE) incr = 1;

    current = position();
    while( cp == current ) {
      delay(14); // shorter
      //      Serial.print(cp);
      check_drift(0);
      current = position();
      if ( current == cp) passing[cp] += incr; // WE HAVEN'T MOVED
      else {
	if ( current == nextpos(cp) ) {     // NORMAL MOVE AHEAD
	  passing[cp] = passing[cp] - 4;    // Store adjusted torque
	  Serial.print("Passed with (4+) ");
	  Serial.println(passing[cp]);
	} else {
	  if (current == position()) {
	    mark_skipped(cp, position());
	    passing[cp]--;
	    Serial.println("skipped");
	  }  else {
	    Serial.println("Must have been a glitch");
	    cp = current == position();
	  }
	}
      } // We've moved (correctly or not)
    } // End while at same position
  } // End pass()

    void set_torque(int t) {
      //      spc(t);
      analogWrite(ROTATOR, t);
    }

    void spc(int n) {
      int t = n;
      while(n>0) {
	Serial.print("."); n -= 3;
      }
      Serial.println(t);
    }

    void printTorques(void) {
	Serial.print("Torque Table: ");
	for (int i = 0; i < size ; i++) {
		Serial.print(" ");
		Serial.print(passing[i]);
	}
	Serial.println("");
    }

    void mark_skipped(int pos1, int pos2) 
    {
      pos1 = nextpos(pos1);
      while(pos1 != pos2) {
	skipped[pos1]++;
	Serial.print(pos1); Serial.println(" marked as skipped");
	pos1 = nextpos(pos1);
	Serial.print("pos2 = "); Serial.println(pos2);
      }
    }

  void pumpoff(void) {
    if( pump[pos] )
      digitalWrite(pump[pos],0);
  }
  // Change time delay for a given position
  void adjust(int pos, int val)    { pause[pos] += val; }

  void openvalve(void) { set_torque(holding[pos]); }

  byte *getPumps(void)    { return pump;     }
  int *getDelays(void)    { return pause;    }

 private:
  int size;            // Number of positions
  int pos;             // Current integral position
  int state;           // Which state of the cycle (drift,hold,pass,etc.)
  int incr;            // (diminishing) increment for adjusting torque
  byte pump[16];       // Array of pump ids for positions
  int pause[16];       // Time in ms to stay at each position
  int add[16];         // Additional time lost to push_through()
  byte holding[16];     // PWM value for holding valve open
  byte passing[16];     // PWM value for pushing to next valve
  byte skipped[16];
  unsigned long lasttime; // Beginning of current time interval
  int testtime;
  boolean ptrmoved;
  };
#endif

