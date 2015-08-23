int LED = 13;  // Needed for 'Serial not defined' bug
#include "secrets.h"
#include "wifi.h"

// All Pathe Arduino control programs contain at least:
//
//   void respondToRequest(void)
//   bool process_command(char c1, char c2, int value)
//   void setup()
//   void loop()
//

int seq = 0;

WIFI w = WIFI();

void respondToRequest(void)
{
  char c1 = NULL, c2 = NULL;
  int value = 0;

  if (Serial.available())
  {
    if ( !w.connected() )
    {
        w.accept();
    }
    else
    {
	if (w.myrecv(&c1, &c2, &value))
	{
	    process_command(c1, c2, value);
        }
    }
  }
}

bool process_command(char c1, char c2, int value)
{
  char reply[40];

  if (c1 == 'x' && c2 == 'x' && value == -1)
  {
    w.mysend("closed(x,x,-1).");
    delay(1000);
    w.reboot(3); // Nothing short of a full restart will work
  }
  else
  {
    sprintf(reply, "thanks[%c][%c]%d", c1, c2, value+seq++);
    return w.mysend(reply);
  }
}

void setup()
{
  Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
  pinMode(13, LED);
  w.start_wifi();
}

void loop()
{
  respondToRequest();
  delay(1000);
}
