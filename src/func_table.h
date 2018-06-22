/*
 * The table of implimented splitting functions
 *
 *  init_split   - Will be called before a tree is started.  May do very
 *                  little, but empirical Bayes like methods will need to set
 *                  some global variables.
 *  choose_split - function to find the best split
 *  eval         - function to calculate the response estimate and risk
 *  error        - Function that returns the prediction error.
 *  num_y        - Number of columns needed to represent y (usually 1)
 */

extern int anovainit(int n, double *y[], int maxcat, char **error,
		     double *parm, int *size, int who, double *wt);
extern int poissoninit(int n, double *y[], int maxcat, char **error,
		       double *parm, int *size, int who, double *wt);
extern int giniinit(int n, double *y[], int maxcat, char **error,
		    double *parm, int *size, int who, double *wt);
extern int usersplit_init(int n, double *y[], int maxcat, char **error,
			  double *parm, int *size, int who, double *wt);
extern int lognormalinit(int n, double *y[], int maxcat, char **error,
                     double *parm, int *size, int who, double *wt);
extern int gammainit(int n, double *y[], int maxcat, char **error,
                         double *parm, int *size, int who, double *wt);


extern void anovass(int n, double *y[], double *value, double *risk,
		    double *wt);
extern void poissondev(int n, double *y[], double *value, double *risk,
		       double *wt);
extern void ginidev(int n, double *y[], double *value, double *risk,
		    double *wt);
extern void usersplit_eval(int n, double *y[], double *value, double *risk,
			   double *wt);
extern void lognormaldev(int n, double *y[], double *value, double *risk,
                    double *wt);
extern void gammadev(int n, double *y[], double *value, double *risk,
                         double *wt);

extern void anova(int n, double *y[], double *x, int nclass,
		  int edge, double *improve, double *split, int *csplit,
		  double myrisk, double *wt);
extern void poisson(int n, double *y[], double *x, int nclass,
		    int edge, double *improve, double *split, int *csplit,
		    double myrisk, double *wt);
extern void gini(int n, double *y[], double *x, int nclass,
		 int edge, double *improve, double *split, int *csplit,
		 double myrisk, double *wt);
extern void usersplit(int n, double *y[], double *x, int nclass,
		      int edge, double *improve, double *split, int *csplit,
		      double myrisk, double *wt);
extern void lognormal(int n, double *y[], double *x, int nclass,
                  int edge, double *improve, double *split, int *csplit,
                  double myrisk, double *wt);
extern void gammasplit(int n, double *y[], double *x, int nclass,
                      int edge, double *improve, double *split, int *csplit,
                      double myrisk, double *wt);

extern double anovapred(double *y, double *yhat);
extern double ginipred(double *y, double *yhat);
extern double poissonpred(double *y, double *yhat);
extern double usersplit_pred(double *y, double *yhat);
extern double lognormalpred(double *y, double *yhat);
extern double gammapred(double *y, double *yhat);


static struct {
    int (*init_split) ();
    void (*choose_split) ();
    void (*eval) ();
    double (*error) ();
} func_table[] = {
    {anovainit, anova, anovass, anovapred},
    {poissoninit, poisson, poissondev, poissonpred},
    {giniinit, gini, ginidev, ginipred},
    {usersplit_init, usersplit, usersplit_eval, usersplit_pred},
    {lognormalinit, lognormal, lognormaldev, lognormalpred},
    {gammainit, gammasplit, gammadev, gammapred}
};

#define NUM_METHODS 6           /* size of the above structure */
