insideradius = 42;
outsideradius = 55;
height = 10;
inletradius = 2;
filterradius = 4;
inducerradius = 1.5;
sampleradius = 2;
t0 = 30;  // tab spacing
i0 = 16;  //inlet spacing

module base(size) {

    difference() {
     cylinder(r=outsideradius+10,h=size,center=true);
     translate([0,0,size/2])
        cylinder(r=outsideradius,h=size,center=true);
    } 
}
module tabs(size) {
    c0 = 45;
    difference() {
     cylinder(r=insideradius,h=size,center=true);
     translate([0,0,size/2])
        cylinder(r=insideradius-10,h=size,center=true);
     translate([t0,t0,0])
		    cube([c0,c0,10],center=true);
     translate([-t0,-t0,0])
		    cube([c0,c0,10],center=true);
     translate([-t0,t0,0])
		    cube([c0,c0,10],center=true);
     translate([t0,-t0,0])
		    cube([c0,c0,10],center=true);
    } 
}
module cap() {
	base(height);
  tabs(height);
}
module septa() {
     difference() {
        cap();
        translate([i0,i0,0])
			      cylinder(r=inletradius,h=30,center=true);
        translate([-i0,-i0,0])
			      cylinder(r=filterradius,h=30,center=true);
        translate([-i0,i0,0])
			      cylinder(r=inducerradius,h=30,center=true);
        translate([i0,-i0,0])
			      cylinder(r=sampleradius,h=30,center=true);

     }
}
septa();

