#ifndef PUMPS_v1_h
#define PUMPS_v1_h
#define LIBRARY_VERSION	1.0.0

class PUMP
{
  public:

  #define OFF    0
  #define PRIME	 1
  #define FLOW   2
  #define HIGH_POWER_MODE 3
  #define LOW_POWER_MODE  4

  #define VALVE_ACTIVATION_MS   5000  // One second
  #define MAX_PRIMING_TIME_MS  20000 // Twenty seconds 

  PUMP(int,int,int);      // Configure pins for Pump, Valve Activate, and Hold

  void setMode(int Mode); // Mode is one of {PRIME, FLOW, OFF }
  int  getMode();         //
  boolean check();        // Check and adjust valve power and return PRIMING state
  long int duration();    // Length of time pump has been in current state
  boolean priming();

  private:
    int currentMode;
    int pump, activate, hold;
    int valve_state;
    int start_time;
    int prime_time;
};
#endif

