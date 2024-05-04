
x_distance_screw=93;
y_distance_screw=78+0;

x_sh=-46.5;
y_sh=-45-6;

x_lcd_pos=-3.5;
y_lcd_pos=-5;

x_lcd_size=72;
y_lcd_size=25;


x_lcd_peg_pos= 2.5; // Position (with respect to the top left aperture of the screen)
y_lcd_peg_pos= 3; // Position (with respect to the top left aperture of the screen)

width_lcd_pegs = 75; // Horizontal distance between screws for the LCD module
height_lcd_pegs= 31; // Vertical distance between screws

x_knob_pos=-12;
y_knob_pos=-69;

x_slot_pos=-55+2;
y_slot_pos=-85;

x_switch_pos=-100+12+8;
y_switch_pos=-69;


x_switch_pos_l=2;
y_switch_pos_l=15;

hole_diam_screw_PCB = 1;
hole_diam_screw_LCD = 1;



internal_height=26;

//top();
translate ([0,0,20]) bottom();

module bottom() { 
    difference() {
        union() {
            difference() {
                union () {
                    difference() {
                        rounded_cube(100+4, 108+4,4);
                        translate([0,0,-2]) cube([110,120,3], center=true);
                    }
                    translate([0,0,-1]) cube([100,108,2], center=true);  
                }
                translate([0,0,-1.5]) cube([98,106,3], center=true);  
            }
            translate([0,0,-3]) {
                translate([x_sh,y_sh,0]) bottom_peg();
                translate([x_sh,y_sh+y_distance_screw,0]) bottom_peg();
                translate([x_sh+x_distance_screw,y_sh,0]) bottom_peg();
                translate([x_sh+x_distance_screw,y_sh+y_distance_screw,0])
                    bottom_peg();
            }
        }
    
        translate([0,0,-2]) {
            translate([x_sh,y_sh,0]) screw_hole();
            translate([x_sh,y_sh+y_distance_screw,0]) screw_hole();
            translate([x_sh+x_distance_screw,y_sh,0]) screw_hole();
            translate([x_sh+x_distance_screw,y_sh+y_distance_screw,0]) screw_hole();
        }
        translate([50,42,-3]) cube([20,20,5], center=true);
        translate([0,-56.5,-4.5]) cube([110,5,8], center=true);
    }
}

module screw_hole()
{
    cylinder(r=2.5,h=6);
}

module bottom_peg()
{
    difference() {
        cylinder(r=3.5,h=4);
        translate([0,0,-2]) cylinder(r=1.6,h=6);
    }
}


module top() {

    difference() {
        union () {
            rounded_cube(100+4, 108+4,internal_height);
        }
        translate([0,0,2]) cube([100,108,internal_height], center=true);    
        translate([0,0,26/2]) cube([100+5,108+5,5], center=true);
        
        // aperture for the LCD, calculated from the left-top screw.
        translate([x_sh+x_distance_screw+x_lcd_pos-x_lcd_size/2,
            y_sh+y_distance_screw+y_lcd_pos-y_lcd_size/2,
            -26/2])
        {    
            cube([x_lcd_size,y_lcd_size,5], center=true);
        }
        
        // Hole for the encoder knob
        translate([x_sh+x_distance_screw+x_knob_pos,
            y_sh+y_distance_screw+y_knob_pos,
            -26/2-2])
        {    
            cylinder(r=4,h=18);
        }
        // Hole for the wires
        translate([x_sh+x_distance_screw+x_slot_pos,
            y_sh+y_distance_screw+y_slot_pos,
            5-2])
        {    
            cube([20,10,4], center=true);
        }
        // Hole for the power switch (top switch)
        /*translate([x_sh+x_distance_screw+x_switch_pos,
            y_sh+y_distance_screw+y_switch_pos,
            -26/2-2])
        {    
            cylinder(r=4,h=18);
        }*/
        
        // Hole for the power switch (lateral switch)
        translate([x_sh+x_distance_screw+x_switch_pos_l,
            y_sh+y_distance_screw+y_switch_pos_l,
            0])
        {    
            rotate([0,90,0]) cylinder(r=16.5/2,h=18);
        }
    }
    // Main PCB and backside screws
    translate([0,0,internal_height-17-internal_height/2+1]) {
        translate([x_sh,y_sh,-17/2]) peg();
        translate([x_sh,y_sh+y_distance_screw,-17/2]) peg();
        translate([x_sh+x_distance_screw,y_sh,-17/2]) peg();
        // This screw is covered by the LCD
        translate([x_sh+x_distance_screw,y_sh+y_distance_screw,-17/2+10])  {
            difference() {
                union() {
                    translate([2,0,3]) {
                        cube([5,4,7],center=true);
                    }
                    cylinder(r=3,h=7);
                }
                translate([0,0,-2]) cylinder(r=1.2,h=10);
            }
        }
    }

    // LCD support screws
    translate([x_sh+x_distance_screw+x_lcd_pos+x_lcd_peg_pos,
            y_sh+y_distance_screw+y_lcd_pos+y_lcd_peg_pos,
            -26/2+2]) {
        small_peg();
        translate([0,-height_lcd_pegs,0]) small_peg();
        translate([-width_lcd_pegs,0,0]) small_peg();
        translate([-width_lcd_pegs,-height_lcd_pegs,0]) small_peg();

    }

}

module peg()
{
    difference() {
        cylinder(r=3,h=17);
        translate([0,0,5]) cylinder(r=hole_diam_screw_PCB,h=13);
    }
}

module small_peg()
{
    difference() {
        cylinder(r=2.5,h=5);
        translate([0,0,0]) cylinder(r=hole_diam_screw_LCD,h=6);
    }
}

/*translate([7,-10,-13.5]) mirror()
        linear_extrude(1)
            #text("Cap tester", size=10, halign = "center", valign="center");  
*/



$fn=64;

module rounded_cube(esizex, esizey,esizez,radius=1)
{
    hull() {
        sizex=esizex-radius*2;
        sizey=esizey-radius*2;
        sizez=esizez-radius*2;
         
        
        translate([sizex/2,sizey/2,sizez/2]) sphere(r=radius);
        translate([-sizex/2,sizey/2,sizez/2]) sphere(r=radius);
        translate([sizex/2,-sizey/2,sizez/2]) sphere(r=radius);
        translate([-sizex/2,-sizey/2,sizez/2]) sphere(r=radius);
        
        translate([sizex/2,sizey/2,-sizez/2]) sphere(r=radius);
        translate([-sizex/2,sizey/2,-sizez/2]) sphere(r=radius);
        translate([sizex/2,-sizey/2,-sizez/2]) sphere(r=radius);
        translate([-sizex/2,-sizey/2,-sizez/2]) sphere(r=radius);
        
    }
}