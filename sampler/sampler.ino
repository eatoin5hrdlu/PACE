//
// PATHE Sample Collector
// Peter Reintjes		July 2015

// PIN ASSIGNMENTS
// Encoder produces a pair of quadrature square waves
// From which we can determine direction as well as # of revolutions

#define ENCA (PINB&_BV(PB1))
#define ENCB (PINB&_BV(PB2))

#define EncoderB 2
#define EncoderA 3

#define MOTOR    12
#define DISTANCE 1400

int ph;  
int toggle; 
int count;
int error;
bool rotating;
bool running;
long int totalcount;
int distance;
int phaseerror;

long unsigned int atime;
long unsigned int wavelength;
int onDelay;

// For measuring interrupt pulse width
long unsigned int width[100];
int wcount;
int lasttime;

// End of Transmission string returned at the end of every conversation
// We still need to have a read timeout at the host, just in case.
#define EOT "end_of_data"


void motorOn()  { PORTB |= _BV(PB4); Serial.println("on"); }
void motorOff() { PORTB &= ~_BV(PB4); Serial.println("Off");}

void respondToRequest(void)
{
	String is = "";
	while (Serial.available() > 0)  // Read a line of input
	{
		int c  = Serial.read();
		if ( c < 32 ) break;
		is += (char)c;
		if (Serial.available() == 0) // It is possible we're too fast
			delay(100);
	}
	if ( is.length() > 0 )  {   // process the command
		int value = 0;
		if (is.length() > 2)
			value = atoi(&is[2]);
		if (!process_command(is[0], is[1], value))
			Serial.println(" bad flow command [" + is + "]");
	}
}

boolean process_command(char c1, char c2, int value)
{
boolean rval = true;
byte d;
char buffer[20];
	switch(c2)
	{
		case '1': d = 1; break;
		case '0': d = 0; break;
		default : break;
	}
	switch(c1)
	{
		case '1': motorOn(); break;
		case '0': motorOff(); break;

		case 'c': // Creep Motor
			PORTB |= _BV(PB4);
			delay(15);
			PORTB &= ~_BV(PB4);
			break;
		case 'r': // Roll
			running = true;
			break;
		case 's': // Spin
			rotating = false;
			break;
		case 'n': // Force Next Sample
			sample();
			break;
		case 'h': 
			PORTB &= ~_BV(PB4);
			running = false;
			rotating = false;
			Serial.println("Hello. Resetting...");
			PORTB &= ~_BV(PB4);
			break;
		case 'm': 
			digitalWrite(MOTOR,1);
			delay(500);
			digitalWrite(MOTOR,0);
			break;
	}
	Serial.println(EOT);
}

void movePlatform() {
	Serial.println("moving platform");
}

void sample() {
	rotate();
	delay(5000);
	rotate();
  delay(5000);
	movePlatform();
}

int pwidth;  /* <Stall>-99 */
int faster() {	if (pwidth < 40) pwidth++; return pwidth; } /* Max is 40 */
int slower() {	if (pwidth > 2) pwidth--; return pwidth; } /* Min is stall PWM */
#define DEAD      1
#define DEAD_SLOW 2
#define SLOW      3
#define MEDIUM    6
#define FAST      7

int newPW(int diff) {  /* Approaching Zero Count */
int newSpeed;
	newSpeed = pwidth;
	if (diff < 50) newSpeed = newSpeed/2;
	if (diff < 100) newSpeed = newSpeed/2;
	if (diff < 200) newSpeed = newSpeed/2;
//	if (newSpeed != pwidth) Serial.print("New pwidth "); Serial.println(newSpeed);
	return newSpeed;
}

void rotate() {
  int lastcount, stallcount;
  stallcount = 0;
  count = 0;  /* Global, updated by moved() (Interrupt Handler) */
  lastcount = count;				if (rotating)

  pwidth = MEDIUM;
  Serial.print("initial pwidth "); Serial.println(pwidth);
  if (!rotating) return;

  PORTB |= _BV(PB4); // ON
  while(count == lastcount) Serial.print("."); // delayMicroseconds(100);
  Serial.println("x");

  error = distance - count;
  while (rotating && error > 10) {
//	Serial.print(" phase error ");Serial.println(phaseerror);
	delayMicroseconds(300); /* Must be slower than an interrupt */
 	error = distance - count;
	pwidth = newPW(error);
//	Serial.print("pwidth "); Serial.println(pwidth);
//	Serial.print("count "); Serial.println(count);
//	Serial.print("stallcount "); Serial.println(stallcount);
//	if (wavelength != 0) {
//		Serial.print("period "); Serial.println(wavelength);
//		Serial.print("onDelay "); Serial.println(onDelay);
//	}

	if (count != lastcount) {
		stallcount = 0; /* We're rolling */
		lastcount = count;
	}
	else {
		stallcount++;
		if (stallcount > 24) { /* 1mS check for stalling */
			if (count == lastcount) {
				if (error > 10) {
					if (rotating) {
						faster();
						Serial.print("STALLED PWIDTH ");
						Serial.println(pwidth);
						PORTB |= _BV(PB4); // ON
						delayMicroseconds(40);
					}
				}
			} else {
				lastcount = count;
				delayMicroseconds(200);
			}
		}
	}
	error = distance - count;
  }
  PORTB &= ~_BV(PB4); // OFF
  delay(1000); /* delay(100) it was still coasting */
/**/
  Serial.print("pwidth "); Serial.print(pwidth);
  Serial.print("STOPPED AT ");Serial.print(distance);
  Serial.print(" ACTUAL "); Serial.print(count);
  Serial.print(" TOT "); Serial.print(totalcount);
/**/
  totalcount += count;
/**/
  Serial.print(" NEWTOT "); Serial.print(totalcount);
  if (totalcount > 98000) totalcount = totalcount - 98000;
  Serial.print(" MOD700 "); Serial.print(totalcount%700);
  error = distance - count;
  Serial.print("   ERROR "); Serial.println(error);
  distance = DISTANCE + error; /* Make up for error on next cycle */
//  distance = DISTANCE;
  Serial.print("NEXT DISTANCE = ");Serial.print(distance);
  int i;
//  for (i=0; i<100; i++) {Serial.print(width[i]); Serial.print("  "); }

}
int usecw;
bool forward;

// wavelength = uSecs between start of phase 0 and phase 3
void achg() {
	PORTB &= ~_BV(PB4); // OFF
	if (forward) {
		wavelength = micros() - atime;
		if (rotating) pulse();
	}
	forward = false;
}
 

void bchg() {
	PORTB &= ~_BV(PB4); // OFF
	if (forward == false) {
		count++;
		if (count > distance-120) {
			PORTB &= ~_BV(PB4); // OFF
			rotating = false;
			return;
		}
		atime = micros();
		if (rotating) pulse();
	}
	forward = true;
}

void pulse() {
	if (wavelength > 200) // Don't even turn it on if going fast
		PORTB |= _BV(PB4); // ON
	if (wavelength > 0 && wavelength < 2000) {
		onDelay = (wavelength/40)+(4*pwidth);
		delayMicroseconds(onDelay);
		PORTB &= ~_BV(PB4); // OFF
	}
}


void moved() {  // INTERRUPT HANDLER  (every  150 uSeconds )
	wavelength = micros() - atime;
//	width[wcount++] = time - lasttime;
	if (forward) { count++; forward = false; }
	else count--;
	if (rotating) {
		if (wavelength > 200) // Don't even turn it on if going fast
			PORTB |= _BV(PB4); // ON
		if (wavelength > 0 && wavelength < 2000) {
			onDelay = (wavelength/20)+(8*pwidth);
			delayMicroseconds(onDelay);
			PORTB &= ~_BV(PB4); // OFF
		}
	}
//	lasttime = time;
}

void phase() {  // INTERRUPT 1 HANDLER
	PORTB &= ~_BV(PB4); // OFF
	forward = true;
	atime = micros();
}

void setup() {
  Serial.begin(9600);
  pinMode(13, OUTPUT);
  pinMode(MOTOR, OUTPUT);
  digitalWrite(MOTOR,0);
  toggle = 0;

  pinMode(9, INPUT_PULLUP);  /* ENCODER A READ */
  pinMode(10, INPUT_PULLUP); /* ENCODER B READ */

  pinMode(EncoderB,INPUT_PULLUP); /* A Interrupt */
  pinMode(EncoderA,INPUT_PULLUP); /* B */
  distance = DISTANCE;
  rotating = false;
  running = false;
  count = distance; /* Unnecessary? */
  totalcount = 0;
//  attachInterrupt(0,moved,CHANGE);
//  attachInterrupt(1,phase,CHANGE);
  attachInterrupt(0,achg,CHANGE);
  attachInterrupt(1,bchg,CHANGE);
  Serial.println("SETUP");
  wavelength = 10000000;
  wcount = 0;
  phaseerror = 0;
}

// LOOP
void loop() 
{
	delay(10);
	respondToRequest();
	delay(3000);
	if (toggle) {
		digitalWrite(13,1);
		toggle = 0;
	} else {
		digitalWrite(13,0);
		toggle = 1;
	}
	Serial.print(" phase error ");Serial.println(phaseerror);
	Serial.print(" count ");Serial.println(count);
	if (running) {
		rotating = true;
		rotate();
	} else
		rotating = false;
	
}

