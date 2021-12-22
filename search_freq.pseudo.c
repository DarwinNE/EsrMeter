/** C-like description of the algorithm to find the best frequency for
    measuring the capacitance.*/

/** Idea: keep in memory the frequency and the results of a previous read.
    Another idea: if the read is invalid, try to increase the frequency. If the
    frequency is the maximum one, abort the measurement (capacitance too low).
    
    If the read is valid, decrease the frequency. A perfect situation is one
    in which the results are toggling between valid (min freq) and invalid (as
    the frequency is below the minimum).
*/

oldw = INVALID;
freq = 0;

// Situations difficult to handle are the read at freq=0 and the read at
// freq == fmax.

while(true) {
    if(freq >=0 && freq<=fmax) {    // Check the frequency is correct.
        SetFreq();          // Try to measure the capacitance. If calculation
        if(curw=ReadADC()==MEAS_OK) {
            curw=CalcCap();  // fails the value of the capacity is not updated.
        }
        printf(".");
    } else {
        // Here the frequency is invalid. We must correct it.
        if(freq<0) {
            curw = FREQHI;     // Frequency is still too high.
            freq = 0;
        } else {
            curw = FREQLO;     // Frequency is still too low.
            freq = fmax;
        }
    }
    if(curw==CAP_OK) {
        // The current read is valid. The value of the capacitance is stored
        // in memory and is not lost.
        // Try to decrease the frequency.
        --freq;
    } else {
        // The current read is invalid. It can be because we are at the
        // minimum frequency. In both cases, we need to increase the frequency.
        // The optimum case can be recognized as the previous read is OK.
        if(oldw==CAP_OK) {
            // We are toggling at the minimum frequency! Perfect situation.
            // The old value of the capacity is the correct one and can be
            // written, the ESR calculated too.
            WriteCap();
            MeasureESR();
            WriteESR();
        } else {
            printf("Measuring");
        }
        // If capacitance can not be read, increase the frequency.
        ++freq;
    }
    oldw = curw;
}


