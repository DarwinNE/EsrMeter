# Array of frequencies to calculate the coefficients.
A=20;B=10;C=1;f= [50,100,200,500,1000,2000,5000,10e3,20e3,50e3,100e3,200e3];


# Some measured values for capacitors

# 100nF, ceramic
# A=100+98/256;  B=100+110/256; C=8+128/256; f=100   # Hz   Current too small
# A=100+250/256; B=101+6/256;   C=8+128/256; f=200   # Hz   Current too small
# A=101+30/256;  B=101+42/256;  C=8+128/256; f=500   # Hz   Current too small
# A=101+38/256;  B=101+47/256;  C=8+128/256; f=1e3   # Hz   Current too small
# A=100+250/256; B=101+6/256;   C=8+128/256; f=2e3   # Hz   Current too small
# A=101+68/256;  B=101+43/256;  C=8+128/256; f=5e3   # Hz   Current too small
# A=101+113/256; B=100+23/256;  C=8+128/256; f=10e3  # Hz   Current too small
# A=101+124/256; B=99+111/256;  C=8+128/256; f=20e3  # Hz   Current too small
# A=101+46/256;  B=90+40/256;   C=8+93/256;  f=50e3   # Hz   Ok
# A=98+255/256;  B=68+64/256;   C=8+64/256;  f=100e3 # Hz   Influence of ESR
# A=86+193/256;  B=30+133/256;  C=9+126/256; f=200e3 # Hz   Esr effect


# 10 µF, good capacitor
# A=100+105/256; B=100+29/256; C=8+128/256;  f=100   # Hz   Current too small
# A=100+246/256; B=99+107/256; C=8+128/256;  f=200   # Hz   Current small but ok
# A=101+41/256;  B=92+235/256; C=8+128/256;  f=500   # Hz   OK, best meas of C.
# A=101+60/256;  B=78+133/256; C=8+160/256;  f=1e3   # Hz   Good measurement
# A=101+93/256;  B=54+249/256; C=8+192/256;  f=2e3   # Hz   Good measurement
# A=101+74/256;  B=30+105/256; C=8+210/256;  f=5e3   # Hz   Influence of ESR
# A=99+199/256;  B=22+196/256; C=8+241/256;  f=10e3  # Hz   Influence of ESR++
# A=97+25/256;   B=19+198/256; C=8+254/256;  f=20e3  # Hz   No effect of C
# A=95+100/256;  B=18+128/256; C=9+64/256;   f=50e3  # Hz   No effect of C
# A=91+110/256;  B=18+64/256;  C=9+197/256;  f=100e3 # Hz   No effect of C
# A=78+50/256;   B=18+146/256; C=11+127/256; f=200e3 # Hz   No effect of C


# 7.5 µF, very bad! Measured 2.2µF and 56 ohm on the LCR-T4.
# A=100+101/256; B=99+249/256; C=8+128/256;  f=100   # Hz   Current too small
# A=100+245/256; B=100+99/256; C=8+128/256;  f=200   # Hz   Current too small
# A=101+33/256;  B=100+21/256; C=8+128/256;  f=500   # Hz   Current too small
# A=101+43/256;  B=99+179/256; C=8+128/256;  f=1e3   # Hz   Current too small
# A=101+52/256;  B=99+30/256;  C=8+128/256;  f=2e3   # Hz   Current too small
# A=101+92/256;  B=97+184/256; C=8+128/256;  f=5e3   # Hz   Maybe ok
# A=101+141/256; B=95+164/256; C=8+128/256;  f=10e3  # Hz   Maybe ok
# A=101+82/256;  B=90+241/256; C=8+128/256;  f=20e3  # Hz   Maybe ok
# A=100+141/256; B=74+234/256; C=8+126/256;  f=50e3  # Hz   Ok
# A=97+142/256;  B=47+102/256; C=8+128/256;  f=100e3 # Hz   Ok
# A=83+82/256;   B=14+64/256;  C=10+68/256;  f=200e3 # Hz   Influence of ESR

# 56 µF 400V
#A = 201+225/256; B=187+60/256; C=17+0/256;  f=100

#A = 76+110/256; B=12+100/256; C=9+192/256;  f=200e3



A
B
C

R=10


omega = 2*pi*f

Rs = (B-C)/(A-B)*R

Rs=0.1

I = (B-C)/(A-C)

# All calculated capacitances are in microfarad
Cap = sqrt((1-I)/((Rs+R)^2*I-Rs*R))./omega*1e6

I
Ir=1/I

Ir_FP = Ir*65536

Irm1=1/I-1

Irm1_FP = Irm1*65536*256

sq=sqrt(Irm1)

sq_FP = sqrt(Irm1_FP)

sq_FP_scaled = sq_FP*256*16

factor = (omega*R)*(256*16*256*16)/1e10

Cap_s = sqrt(1/I-1)./(omega.*R).*1e6

Cap_s_scaled = sq_FP_scaled./factor

threshold = 2^16
