# Array of frequencies to calculate the coefficients.

f= [20,75,200,500,1000,2000,5000,10e3,20e3,50e3,100e3,200e3];

R=10

omega = 2*pi*f;

format short g
factor = (omega*R)*(256*16*256*16)/1e10
