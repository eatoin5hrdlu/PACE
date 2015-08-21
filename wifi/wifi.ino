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

WIFI w = WIFI();

void respondToRequest(void)
{
  char c1 = NULL, c2 = NULL;
  int value = 0;

  if ( ! w.connected() )
    w.accept();
  else if ( w.myrecv(&c1, &c2, &value) )
    process_command(c1, c2, value);
}

bool process_command(char c1, char c2, int value)
{
  char *msg = "thanks[X][Y]";
  switch (c1)
  {
    case 'a':
      switch (c2)
      {
        case 'a':
          msg[7] = 'x';
          break;
        case 'b':
          msg[7] = 'y';
          break;
      }
      break;

    case 'b':
      msg[7] = 'z';
      break;
    default :
      msg[7] = c1;
      msg[10] = c2;
  }
  w.mysend(msg);
  return true;
}


void setup()
{
  Serial.begin(9600); // 9600, 8-bits, no parity, one stop bit
  pinMode(13, LED);
  w.start_server();
}


void loop()
{
  respondToRequest();
  delay(500);
}


