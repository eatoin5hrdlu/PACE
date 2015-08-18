#ifndef WIFI_v1_h
#define WIFI_v1_h
#define LIBRARY_VERSION	1.0.0

#ifdef SPLATSPACE
#define SSID "splatspace"
#define PASSWORD "hacktheplanet"
#endif

#ifdef INNATRIX
#define SSID "UNC-PSK"
#define PASSWORD "IDontWantChangeIWantSwissCheese"
#endif

#ifdef HOME
#define SSID "milton"
#define PASSWORD ""
#endif

/*
 * Wifi submodule (meant to be plug-in alternative for Bluetooth)
 */

class WIFI
{

 private:
  int lastid;
  bool once;
  char buf[100];

 public:

  WIFI() {
    lastid = 0;
    once = true;
    Serial.println("AT");
    delay(500);
    if (!readstring() || strncmp("OK",buf,2))
      mysend("error0 not AT-OK");
  }

  char *
    mysend(char *data) 
    {
      Serial.print("AT+CIPSEND=");
      Serial.print(lastid);
      Serial.print(",");
      Serial.println(strlen(data)+2);
      Serial.println(data);
      return data;
    }

  bool
    readstring(void)
    {
      int i = 0;
      while (Serial.available() > 0)  // Read a line of input
	{
	  int c  = Serial.read();
	  if ( c == 13 ) continue;
	  if ( c == 10 ) break;
	  buf[i++] = (char)c;
	  if (Serial.available() == 0) // It is possible we're too fast
	    delay(200);
	}
      if (i == 0) return false;
      return true;
    }

  char *
    myrecv(void) 
    {
      int token,length,idx;
      if (readstring()) 
	{
	  if (!strncmp(buf,"+IDP=",5)) {
	    int token,length;
	    int idx = 5;
	    lastid = buf[idx]-'0';
	    idx++;
	    if (buf[idx] != ',') {
	      return mysend("error1");
	    }
	    idx++;
	    token = idx;
	    while(buf[idx] != ',') idx++;
	    buf[idx] = 0;
	    length = atoi(&buf[token]);
	    idx++;
	    if (buf[idx+length-1] != 10 || buf[idx+length-2] != 13) {
	      return mysend("error2");
	    } else {
	      mysend("Data was not properly terminated");
	    }
	    buf[idx+length-2] = 0;
	    return(&buf[idx]);
	  }
	}
    }

  void respondToRequest(void)
  {
    char *cp = myrecv();
    if ( strlen(cp) > 0 )  {   // process the command
      int value = 0;
      if (strlen(cp) > 2)
	value = atoi(&cp[2]);
      if (!process_command(cp[0], cp[1], value)) {
	mysend("bad command");
      }
    }
  }

  bool process_command(char c, char c2, int value)
  {
    char *msg = "thanks[X][Y]";
    msg[7] = c;
    msg[10] = c2;
    mysend(msg);
    return true;
  }

  void start_server() 
  {
    if (once) {
      Serial.println("AT+CWJAP=\"milton\",\"\"");
      Serial.println("AT+CWMODE=3");
      Serial.println("AT+CIPMUX=1");
      once = false;
    }
    Serial.println("AT+CIPSERVER=1,23");
  }

};  // End of Wifi Class
#endif

