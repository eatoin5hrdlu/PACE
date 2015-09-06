#define STANDALONE 1

#define SAVE	1          // EEPROM STORAGE COMMANDS
#define RESTORE	0

#define ANALOG_TEMPERATURE   0 // Analog input for temperature
#define ANALOG_TURBIDITY     3 // Analog input for turbidity

#define HEATER          9     // Control solid-state relay (~40VAC)
#define LED             13     // Turbidity light
#define LASER           8     // Meniscus light
#define JARLIGHT        7     // Meniscus light
#define NUTRIENT        4     // Only one valve for Cellstat
#define MIXER           3     // PWM for 12V motor

#define DEFAULT_CYCLETIME       20  // Seconds

//#define INDUCER1      4
//#define HOSTCELLS     3
//#define OUTFLOW       2

#define MAX_VALVES      6

#define MIXERSPEED 180   // PWM value for top mixer speed


