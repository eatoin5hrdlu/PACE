//
// PATHE Sample Collector
// Peter Reintjes		July 2015

// PIN ASSIGNMENTS
// Encoder produces a pair of quadrature square waves
// From which we can determine direction as well as # of revolutions
#define EncoderB 2
#define EncoderA 3
#define MOTOR    4

#define EOT "end_of_data" 

// Interrupt Zero (0) is pin 2 (EncoderB), so then we look at EncoderA
#define EncoderB 2
#define EncoderA 3
#define MOTOR    4

// End of Transmission string returned at the end of every conversation
// We still need to have a read timeout at the host, just in case.
#define EOT "end_of_data" 
#define DISTANCE  240
int where = 0;

#define WAIT_FOR_VALVE while(where>0) delay(20)

void rotate(int r) {
	where = r;
	digitalWrite(MOTOR,1);
}

void moved() {  // INTERRUPT HANDLER
	if (where > 0)  where--;
	else digitalWrite(MOTOR,0);
}

void setup() {
	pinMode(MOTOR, OUTPUT);
	digitalWrite(MOTOR,0);
	where = 0;
	Serial.begin(9600);
	pinMode(EncoderA,INPUT_PULLUP); /* A */
	pinMode(EncoderB,INPUT_PULLUP);  /* B INT0 on falling edge */ 
	attachInterrupt(0,moved,FALLING);
	pinMode(13, OUTPUT);
}

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
		case 'c': // Creep Motor
			rotate(1);
			break;
		case 'r': // Roll
			int x;
			for (x=0;x<DISTANCE;x++) {
				rotate(1);
				delay(1);
			}
			break;
		case 's': // Spin
			rotate(DISTANCE-(DISTANCE/3));
			break;
		case 'n': // Force Next Sample
			sample();
			break;
		case 'h': 
			Serial.println("hello");
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
	rotate(DISTANCE);
	WAIT_FOR_VALVE;
	delay(2000);
	rotate(DISTANCE);
	WAIT_FOR_VALVE;
	movePlatform();
}
int toggle = 0;

void loop() 
{
	delay(10);
	respondToRequest();
	delay(100);
	if (toggle) {
		digitalWrite(13,1);
		toggle = 0;
	} else {
		digitalWrite(13,0);
		toggle = 1;
	}
}
