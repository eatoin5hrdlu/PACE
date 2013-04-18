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
interface via the URL:  <machine-name>:8080/pace.pl

