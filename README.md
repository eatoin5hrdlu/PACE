PACE :  Phage-Assisted Continuous Evolution
====
Software for a low-cost implementation of  PACE apparatus.

PACE is described by David R. Liu, Kevin M. Esvelt and Jacob C. Carlson
in    http://www.ncbi.nlm.nih.gov/pubmed/21478873

This project contains Arduino code for Turbidostats and the Sample Collector,
as well as the server software to provide an on-screen interface to Turbidostat
and Sample Collector control and Web interface.

SWI-Prolog based web server, GUI, and Turbidostat control software
is written mostly in Prolog with some Python (e.g. pySerial, for a 
portable serial interface to Arduino) and HTML.

Running PACE on a Linux or Windows machine provides an interface
for up to 16 turbidostats: USB connected Arduinos running pace.ino
and one sample collector: USB connected Arduino running collector.ino.
It also contains a web server to provide essentially the same
interface via the URL:  &lt;machine-name&gt;:8080/pace.pl

Prerequisite Software
====
- Python2.7
- OpenCV (python)

Getting Started
====
Clone the repo:
    git clone https://github.com/eatoin5hrdlu/PACE.git

Run the app:
    cd app
    python levelapp.py

Controls
====
*z  - read one frame and find levels and display
*r - read, find levels and display for 20 seconds.
*0<nl>   Select the turbidostat as current vessel
*{1,2,3,4 for lagoons)
*<arrow keys> to move up/down/left/right, or:
*h  - move to the left
*j  - move down
*k  - move up
*l  - move right
*t -make region taller
*s - make region shorter
*f - make region fatter
*d - diminish (make region thinner)
*x - exits the program