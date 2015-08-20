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
  unsigned long lastcomm;

 public:
  WIFI() {
    id = -1;
    once = true;
    lastcomm = millis();
  }

  void initialize() {
    //    Serial.println("ATE0");  okResponse(1000, 5);
  }

  bool mysend(char *data)
    {
      if (id > -1) // Connected
	{
	  Serial.print("AT+CIPSEND=");
	  Serial.print(id);
	  Serial.print(",");
	  Serial.println(strlen(data)+2);
	  while(!Serial.available());
	  while(Serial.read() != '>') delay(100);
	  Serial.println(data);
	  return okResponse(1000,1);
	}
      return false; // no connection?
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
      {
	process_command(c1,c2,value);
	lastcomm = millis();
      }
    if ( (millis() - lastcomm) > 60000 )
      {
	flash(10);
	start_server();
	lastcomm = millis();
      }
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

  bool okResponse(int dly, int n) // Read all available input into buf 
  {
    int waiting = 0;
    while (waiting < dly)  // Give the data a chance to arrive
      {
	if ( dly > 5000 ) { delay(dly); waiting = dly; }
	else              { delay(500);	waiting += 500;}
	if (Serial.available() > 0) waiting = dly;
      }
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

  void old_start_server() 
  {
    if (once) {
      delay(5000);
      initialize();
      // AT+CWAUTOCONN=0  // No auto connect set in ESP so don't need next line
      //      Serial.println("AT+CWQAP");  okResponse(2000,2);
      //Serial.println("AT+CWMODE=1");  okResponse(1000,3); // Station only
      Serial.println("AT+CWMODE=3");  okResponse(1000,3);  // Access point and Station

      Serial.println(SECRETJOIN);
      while(!okResponse(12000,2)) {
	Serial.println(SECRETJOIN);
      }
      Serial.println("AT+CIPMUX=0");  okResponse(1000,3);
      once = false;
    }
    Serial.println("AT+CIPSERVER=0"); okResponse(2000, 6);
    Serial.println("AT+CIPSERVER=1,23"); okResponse(5000, 6);
 }

  bool atcmd(char *cmd, int dly, int numflash) 
  {
    if (strncmp(cmd,"AT+",3)) 
      Serial.print("AT+");
    Serial.println(cmd);
    return okResponse(dly, numflash);
  }

  bool start_server() 
  {
    atcmd("RST",1000,6);
    delay(1000);
    atcmd("ATE0", 1000, 5);
    atcmd("CIPMODE=0", 1000, 4);
    atcmd("CWAUTOCONN=0",1000, 3);
    atcmd("CWQAP", 1000, 2);
    atcmd("CWMODE=3",1000, 1);

    // Allowing 12 second to connect to an access point, one
    // long flash every 12 seconds means we are stuck here!
    while(!atcmd(JOINAP, 12000, 6)) ;

    atcmd("CIPMUX=1", 1000, 1);
    atcmd("CIPSERVER=0", 1000, 2);
    return atcmd("CIPSERVER=1,23",2000, 3); // All is well if CIPSERVER returns true
 }

};  // End of WIFI Class
#endif

