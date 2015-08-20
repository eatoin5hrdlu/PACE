#ifndef WIFI_v1_h
#define WIFI_v1_h
#define LIBRARY_VERSION	1.0.0

/*
 * Wifi submodule (plug-in alternative for Bluetooth)
 *
 * 1) Configure ESP8266 WiFi connection server and wait to be contacted
 *    WIFI w = WIFI();
 *         w.start_server();    
 *
 * 2) Accept connection and respond to requests
 *         char *cp = w.recv()    returns pointer to string buffer
 *         w.send(char buf[])     sends text addressed in buf
 */

class WIFI
{
 private:
  int id;
  bool once;
  char buf[100];

 public:
  WIFI() {
    id = -1;
    once = true;
  }

  void initialize() {
      Serial.println("ATE0");  delay(1000); okResponse(5);
  }

  char * mysend(char *data)
    {
      if (id > -1) // Connected
	{
	  Serial.print("AT+CIPSEND=");
	  Serial.print(id);
	  Serial.print(",");
	  Serial.println(strlen(data)+2);
	  Serial.println(data);
	  Serial.println("");
	}
      return data;
    }

  bool readline(void)  // Every line must end with \n ( \r ignored )
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
    buf[i] = NULL;
    if (i == 0) return false;
    return true;
  }

  bool myrecv(char *pc1, char *pc2, int *pvalue)
  {
      char c1,c2;
      int value,len,conid;
      if (readline()) // Fills buf or returns false
	{
	  if ( sscanf(buf,"+IPD,%d,%d:%c%c%d",&id,&len,pc1,pc2,pvalue) > 2)
	    return true;
	  if ( sscanf(buf,"%d,CLOSED", &conid) == 1 )
	    {
	      if (conid == id) { id = -1; return false; }
	    }
	}
      return false;
  }

  void respondToRequest(void)
  {
    char c1 = NULL, c2 = NULL;
    int value = 0;
    if ( myrecv(&c1,&c2,&value) )
      process_command(c1,c2,value);
  }

  bool connected() { return (id > -1); }
  
  int accept()
  {
    if (readline())
      sscanf(buf, "%d,CONNECT", &id);
    return id;
  }

  bool process_command(char c1, char c2, int value)
  {
    char *msg = "thanks[X][Y]";
    msg[7] = c1;
    msg[10] = c2;
    mysend(msg);
    return true;
  }

  bool okResponse(int n)    // Read all available input into buf
  {
    delay(1000);      // Give the data a chance to arrive
    int len = 0;
    while (Serial.available() > 0)
	{
	  buf[len++] = Serial.read();
	  if (Serial.available() == 0)
	    delay(500);
	}
    buf[len] = NULL;
    if (len>0)
      {
	int i;
	for (i=0;i<len-3;i++)
	  {
	    if ( strncmp(&buf[i],"OK",2) == 0 )
	      {
		flash(n);
		return true;
	      }
	  }
      }
    flash(1);
    return false;
  }

  void flash(int n)
  {
    int i;
    int dly = ( n==1 ? 1000 : 100 ); // One long or N short flashes
    for (i=0;i<n;i++) {
      digitalWrite(LED,1);
      delay(dly);
      digitalWrite(LED,0);
      delay(dly);
    }
  }

  void start_server() 
  {
    if (once) {
      delay(5000);
      initialize();
      Serial.println("AT+CWQAP");  delay(2000); okResponse(2);
      Serial.println("AT+CWMODE=3");  delay(1000); okResponse(3);

      Serial.println(SECRETJOIN);
      delay(10000);
      while(!okResponse(2)) {
	Serial.println(SECRETJOIN);
	delay(10000);
      }
      Serial.println("AT+CIPMUX=1");  delay(1000); okResponse(3);
      once = false;
    }
    Serial.println("AT+CIPSERVER=0"); delay(2000); okResponse(6);
    Serial.println("AT+CIPSERVER=1,23"); delay(5000); okResponse(6);
 }

};  // End of WIFI Class
#endif

