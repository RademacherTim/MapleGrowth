//
// This Stan program defines a simple model for ring width, with a vector of 
// values of tree-ring widths 'rwYSTI' modeled as half-normally distributed with
// scale parameter 'theta', which is dependent on within-tree variation 'epsilon 
// [core]' that is normally distributed.
//

// The input data is a vector 'rwYSTI' of length 'N'.
data{
    int N;
    vector[N] rwYSTI;
    //int y[N];
    //int s[N];
    int t[N];
    int inc[N];
}

// The parameter accepted by the model. Our model accepts one parameter 'theta'.
parameters{
    real<lower=0> rwYST;
    //real<lower=0> rwYS;
    real<lower=0> a0;
    real<lower=0> theta;
    vector[69] aT;
    //real<lower=0> sigmaS;
    //real<lower=0> sigmaT;
    real<lower=0> sigmaI;
}

// The model to be estimated. We model the output 'rwYSTI' to be half-normally 
// distributed with dispersion 'theta'.
model{
    vector[N] rwY;
    sigmaI ~ exponential( aI ); // between-core variation
    //sigmaT ~ exponential( 1 ); // between-tree variation
    //sigmaS ~ exponential( 1 ); // between-site variation
    aT ~ normal( 0 , 1 );  // normally distributed between-year variation
    theta ~ normal( 2 , 1 ); // prior for 
    a0 ~ normal( 0 , theta ); // intercept is half-normal, which represents grand mean-growth
    for ( i in 1:N ) {
        rwY[i] = a0 + aY[y[i]];
    }
    rwYS ~ normal( rwY , sigmaS );
    rwYST ~ normal( rwYS , sigmaT );
    rwYSTI ~ normal( rwYST , sigmaI );
}
