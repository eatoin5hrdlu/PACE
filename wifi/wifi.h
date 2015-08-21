#ifndef WIFI_v1_h
#define WIFI_v1_h
#define LIBRARY_VERSION	1.0.0

/*
 * Wifi submodule (alternative for Bluetooth)
 *
 * 1) Configure ESP8266 WiFi connection server and wait to be contacted
 *    WIFI w = WIFI();
 *         w.start_server();    
 *
 * 2) Accept connection and respond to requests
 *         w.myrecv(&c1,&c2,&val) fills in 2-chars and an int
 *         w.mysend(char *cp)     send text over link
 */

class WIFI
{
 private:
  int id;           // -1 not connected, 0-10 WiFi, 100 Bluetooth
  bool once;
  int  ap;          // Index of successfully connected access point
  char buf[100];    // Arduino Serial only has 64-byte buffer
  unsigned long lastcomm; // mS since last interaction for (3m) timeout

 public:
  WIFI() { ap = -1; id = -1; once = true; lastcomm = millis(); }

  bool mysend(char *data)
    {
      if (id > -1) // Connected
	{
	  Serial.print("AT+CIPSEND=");
	  Serial.print(id);
	  Serial.print(",");
	  Serial.println(strlen(data)+2);
	  while(!Serial.available()) delay(10);
	  while(Serial.read() != '>') delay(100);
	  Serial.println(data);
	  return okResponse(500,1);
	}
      return false; // no connection?
    }

  // Every line must end with \n ( \r ignored )
  // Waits up to delay mS for some data to appear
  bool readline(int dly)
    {
    int i = 0;
    int incr = dly/100;
    while (!Serial.available() && i < dly) { delay(incr); i += incr; }
    i = 0;
    while ( Serial.available() && i < sizeof(buf) )
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
      if (readline(0)) // Fills buf or returns false
	{
	  if ( sscanf(buf,"+IPD,%d,%d:%c%c%d",&id,&len,pc1,pc2,pvalue) > 2)
	    {
	      lastcomm = millis();  // Reset timeout
	      return true;
	    }
	  if ( sscanf(buf,"%d,CLOSED", &conid) == 1 )
	      if (conid == id) {
		id = -1;
		return false;
	      }
	}
      if ( (millis() - lastcomm) > 480000 ) // 8 minute timeout!
	{
	  closeConnection(id);
	  id = -1;
	  start_server();
	  lastcomm = millis();
	}
      return false;
  }

  bool connected() { return (id > -1); }
  
  int accept()
  {
    if (readline(0))
      sscanf(buf, "%d,CONNECT", &id);
    return id;
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
    while (Serial.available() > 0 && len < sizeof(buf))
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
    int dly = ( n==1 ? 1000 : 200 ); // One long or N short flashes
    for (i=0;i<n;i++) {
      digitalWrite(LED,1);
      delay(dly-100);
      digitalWrite(LED,0);
      delay(dly);
    }
  }

  bool closeConnection(int id)
  {
    Serial.print("AT+CIPCLOSE=");
    Serial.println(id);
    return okResponse(1000, 9);
  }

  bool atcmd(char *cmd, int dly, int numflash) 
  {
    if (strncmp(cmd,"AT",2)) 
      Serial.print("AT+");
    Serial.println(cmd);
    return okResponse(dly, numflash);
  }

  bool joinAP() // Join first available, or known good Access Point
  {
    if (ap > -1) return (atcmd(secrets[ap].joinap, 12000, 5));
    
    int s;
    for (s=0; s < NUM_SECRETS; s++)
      if ( atcmd(secrets[s].joinap, 12000, 5) )
	{
	  ap = s;
	  return true;
	}
    return false;
  }

  bool start_server()
  {
    atcmd("RST",6000, 2);
    if ( !atcmd("ATE0", 1000, 4) )
      {
	id = 100;
	return true;  // Must be bluetooth
      }
    atcmd("CIPMODE=0", 1000, 2);
    atcmd("CWAUTOCONN=0",1000, 4);
    atcmd("CWQAP", 1000, 2);
    atcmd("CWMODE=3",1000, 4);
    if ( joinAP() ) {
      atcmd("CIPMUX=1", 1000, 2);
      atcmd("CIPSERVER=0", 1000, 4);
      return atcmd("CIPSERVER=1,23",2000, 2);
    }
    return false;
 }

};  // End of WIFI Class
#endif

