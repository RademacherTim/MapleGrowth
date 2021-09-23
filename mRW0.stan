//
// This Stan program defines a simple model for ring width, with a vector of 
// values of tree-ring widths 'rwYS' modeled as half-normally distributed with
// scale parameter 'theta', which is dependent on within-tree variation 'epsilon 
// [core]' that is normally distributed.
//

// The input data is a vector 'rwYS' of length 'N'.
data{
    int Y; // number of years
    int S; // number of sites
    int N; // number of ring width samples
    int y[N]; // year of formation
    vector[N] rw; // ring width 
    //vector[N] tasSep0; // mean temperature of current September   
    vector[N] tasAug0; // mean temperature of current August   
    vector[N] tasJul0; // mean temperature of current Jul   
    vector[N] tasJun0; // mean temperature of current Jun   
    vector[N] tasMay0; // mean temperature of current May   
    //vector[N] tasApr0; // mean temperature of current April   
    //vector[N] tasMar0; // mean temperature of current March   
    //vector[N] tasFeb0; // mean temperature of current Februaru   
    //vector[N] tasJan0; // mean temperature of current January   
    //vector[N] preSep0; // total precipitation of current September   
    vector[N] preAug0; // total precipitation of current August   
    vector[N] preJul0; // total precipitation of current Jul   
    vector[N] preJun0; // total precipitation of current Jun   
    vector[N] preMay0; // total precipitation of current May   
    //vector[N] preApr0; // total precipitation of current April   
    //vector[N] preMar0; // total precipitation of current March   
    //vector[N] preFeb0; // total precipitation of current Februaru   
    //vector[N] preJan0; // total precipitation of current January   
    int s[N]; // site index
}
// The parameter accepted by the model. Our model accepts one parameter 'theta'.
parameters{
    real rwYS;    // ring width data for each site and year of formation
    vector[K] aS; // site specific background growth rate
    //real bTJan0;  // slope parameter for current January mean temperature effect  
    //real bTFeb0;  // slope parameter for current February mean temperature effect
    //real bTMar0;  // slope parameter for current March mean temperature effect
    //real bTApr0;  // slope parameter for current April mean temperature effect
    real bTMay0;  // slope parameter for current May mean temperature effect
    real bTJun0;  // slope parameter for current June mean temperature effect
    real bTJul0;  // slope parameter for current July mean temperature effect
    real bTAug0;  // slope parameter for current August mean temperature effect
    //real bTSep0;  // slope parameter for current September mean temperature effect
    //real bPJan0;  // slope parameter for current January total precipitation effect  
    //real bPFeb0;  // slope parameter for current February total precipitation effect
    //real bPMar0;  // slope parameter for current March total precipitation effect
    //real bPApr0;  // slope parameter for current April total precipitation effect
    real bPMay0;  // slope parameter for current May total precipitation effect
    real bPJun0;  // slope parameter for current June total precipitation effect
    real bPJul0;  // slope parameter for current July total precipitation effect
    real bPAug0;  // slope parameter for current August total precipitation effect
    //real bPSep0;  // slope parameter for current September total precipitation effect
    real<lower=0> sigmaS; // within-site variability
}

// The model to be estimated. 
model{
    vector[N] rwY;
    sigmaS ~ exponential( 1 );
    //bTSep0 ~ normal( 0 , 2 );
    bTAug0 ~ normal( 0 , 2 );
    bTJul0 ~ normal( 0 , 2 );
    bTJun0 ~ normal( 0 , 2 );
    bTMay0 ~ normal( 0 , 2 );
    //bTApr0 ~ normal( 0 , 2 );
    //bTMar0 ~ normal( 0 , 2 );
    //bTFeb0 ~ normal( 0 , 2 );
    //bTJan0 ~ normal( 0 , 2 );
    //bPSep0 ~ normal( 0 , 2 );
    bPAug0 ~ normal( 0 , 2 );
    bPJul0 ~ normal( 0 , 2 );
    bPJun0 ~ normal( 0 , 2 );
    bPMay0 ~ normal( 0 , 2 );
    //bPApr0 ~ normal( 0 , 2 );
    //bPMar0 ~ normal( 0 , 2 );
    //bPFeb0 ~ normal( 0 , 2 );
    //bPJan0 ~ normal( 0 , 2 );
    aS ~ normal( 0 , 2 );
    for ( i in 1:N ) {
        rwY[i] = aS[s[i]] + //bTJan0 * tasJan0[i] + bTFeb0 * tasFeb0[i] + 
        //bTMar0 * tasMar0[i] + bTApr0 * tasApr0[i] + 
        bTMay0 * tasMay0[i] + 
        bTJun0 * tasJun0[i] + bTJul0 * tasJul0[i] + bTAug0 * tasAug0[i] + 
        //bPSep0 * preSep0[i] + bPJan0 * preJan0[i] + bPFeb0 * preFeb0[i] + 
        //bPMar0 * preMar0[i] + bPApr0 * preApr0[i] + 
        bPMay0 * preMay0[i] + 
        bPJun0 * preJun0[i] + bPJul0 * preJul0[i] + bPAug0 * preAug0[i]// + 
        //bPSep0 * preSep0[i]
        ;
    }
    rwYS ~ normal( rwY , sigmaS );
}
