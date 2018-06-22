/*
* The four routines for gamma splitting
*/
#include <math.h>
#include "rpart.h"
#include "rpartproto.h"


static double *yvalue, *weight, *rate;
static int *countn, *order, *order2;

int
gammainit(int n, double *y[], int maxcat, char **error,
	  double *parm, int *size, int who, double *wt)
{
  
  /* allocate memory for scratch */
  if (who == 1 && maxcat > 0) {
    yvalue = (double *) ALLOC(3 * maxcat, sizeof(double));
    rate = yvalue + maxcat;
    weight = rate + maxcat;
    order = (int *) ALLOC(3 * maxcat, sizeof(int));
    order2 = order + maxcat;
    countn = order2 + maxcat;
  }
    *size = 1;
    return 0;
}

/*
* The gamma evaluation function.  Return the mean and the dev.
*/
void
gammadev(int n, double *y[], double *value, double *risk, double *wt)
{
    int i;
    double temp = 0., twt = 0.; /* sum of the weights */
    double mu, dev = 0;

    for (i = 0; i < n; i++) {
	    temp += *y[i] * wt[i];
	    twt += wt[i];
    }
    mu = temp / twt;

    for (i = 0; i < n; i++) {
      temp = *y[i];
      if (temp > 0)
        dev += (log(temp / mu) - ((temp - mu) / mu)) * wt[i];
    }

    *value = mu;
    *risk = -2 * dev;
}


/*
 * The gamma splitting function.  Find that split point in x such that
 *  the deviance of y within the two groups is decreased as much
 *  as possible.  It is not necessary to actually calculate the dev, the
 *  improvement involves only means in the two groups.
 */
void
gammasplit(int n, double *y[], double *x, int nclass,
      int edge, double *improve, double *split, int *csplit,
      double myrisk, double *wt)
{
  int i, j;
  int left_n, right_n;
  double left_wt, right_wt;
  double left_sum, right_sum;
  double dev;                 /* dev of the parent node (me) */
double left_mu, right_mu;
double best, temp;
int direction = LEFT;
int where = 0;
int ncat;

/*
* Get the total sum and the total weights
*/
right_sum = 0;
right_wt = 0;
right_n = n;
for (i = 0; i < n; i++) {
  right_sum += *y[i] * wt[i];
  right_wt += wt[i];
}

/*
* Compute the overall mean and dev
*/
right_mu = right_sum / right_wt;
dev = right_wt* log(right_mu);

/*
* at this point we split into 2 disjoint paths
*/
if (nclass > 0)
  goto categorical;

left_wt = 0;
left_sum = 0;
where = -1;
best = dev;
for (i = 0; i < n - edge; i++) {
  left_sum += *y[i] * wt[i];
  right_sum -= *y[i] * wt[i];
  left_wt += wt[i];
  right_wt -= wt[i];
  
  if (x[i + 1] != x[i] && (1 + i) >= edge) {
    left_mu = left_sum / left_wt;
    right_mu = right_sum / right_wt;
    temp = 0;
    if (left_mu > 0)
      temp += left_wt * log(left_mu);
    if (right_mu > 0)
      temp += right_wt * log(right_mu);
    if (temp < best) {
      best = temp;
      where = i;
      direction = (left_mu < right_mu) ? LEFT : RIGHT;
    }
  }
}

*improve = 2 * (dev - best);
if (where >= 0) {           /* found something */
csplit[0] = direction;
  *split = (x[where] + x[where + 1]) / 2;
}
return;

categorical:;
for (i = 0; i < nclass; i++) {
  weight[i] = 0;
  yvalue[i] = 0;
  countn[i] = 0;
}

for (i = 0; i < n; i++) {
  j = (int) (x[i] - 1);
  countn[j]++;            /* number per group */
yvalue[j] += *y[i] * wt[i];
weight[j] += wt[i];    /* sum of time */
}

/*
* Rank the rates  - each is scored as the number of others that it
* is smaller than.  Ignore the categories which had no representatives.
*/
ncat = 0;                   /* may be less than nclass if not all
* categories are present */
for (i = 0; i < nclass; i++) {
  order[i] = 0;
  if (countn[i] > 0) {
    ncat++;
    rate[i] = yvalue[i] / weight[i];
    for (j = i - 1; j >= 0; j--) {
      if (countn[j] > 0) {
        if (rate[i] > rate[j])
          order[j]++;
        else
          order[i]++;
      }
    }
  }
}
/*
* order2 will point to the largest, second largest, etc
*/
for (i = 0; i < nclass; i++)
  if (countn[i] > 0)
    order2[order[i]] = i;
  
  /*
  * Now find the split that we want
  * starting with everyone in the right hand group
  */
  left_n = 0;
  left_sum = 0;
  left_wt = 0;
  best = dev;
  where = 0;
  for (i = 0; i < ncat - 1; i++) {
    j = order2[i];
    left_n += countn[j];
    right_n -= countn[j];
    left_wt += weight[j];
    right_wt -= weight[j];
    left_sum += yvalue[j];
    right_sum -= yvalue[j];
    if (left_n >= edge && right_n >= edge) {
      left_mu = left_sum / left_wt;
      right_mu = right_sum / right_wt;
      temp = 0;
      if (left_mu > 0)
        temp += left_wt * log(left_mu);
      if (right_mu > 0)
        temp += right_wt * log(right_mu);
      if (temp < best) {
        best = temp;
        where = i;
        direction = (left_mu < right_mu) ?  LEFT : RIGHT;
      }
    }
  }
  
  *improve = 2 * (dev - best);
  
  /* if improve = 0, csplit will never be looked at by the calling routine */
  for (i = 0; i < nclass; i++)
    csplit[i] = 0;
  for (i = 0; i <= where; i++)
    csplit[order2[i]] = direction;
  for (; i < ncat; i++)
    csplit[order2[i]] = -direction;
}


/*
 * The error function for gamma splitting
 */
double
gammapred(double *y, double *yhat)
  {
    /* No internal cross-validation implemented */
    return 0.;
  }

