#include <stdlib.h>
#include <stdio.h>

/*
 * Function to sample the candidate splits from all the variables
 * int *vector: a placeholder vector which will be filled with the sampled candidates for splitting
 * int x: the total number of variables such that the possible variables to choose are 0,1,...,x-1
 * int size: the number of candidates to choose for splitting the current node
 * ((int seed: the seed for the random number generation in the current node)) not used anymore
 */
void sample(int *vector, int x, int size){
  
  //srand(seed);
  
  int i, j = 0;
  
  for (i = 0; i < x && j < size; ++i) {
    int ri = x - i;
    int rj = size - j;
    if (rand() % ri < rj)    
      /* Take it */
      vector[j++] = i; 
  }
  
}

/*
 * Main program to test the sample function with x = 11 and size = 4
 * The function is ran 1.000.000 times and the distribution is checked in R
 * Test result: function delivers a uniform distribution over 0:10
 */
/*
int main(){
 
#define M 4
#define N 11
 
 FILE *f = fopen("file.txt", "w");

for(int i=0; i<1000000; ++i){

int vector[M];
sample(vector, N, M, i);

for(int i = 0; i < M; i++) {
fprintf(f, "%d ", vector[i]);
} 
fprintf(f, "\n");
}

fclose(f);
return(0);
}
 */





