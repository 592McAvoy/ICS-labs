/* 
 * trans.c - Matrix transpose B = A^T
 *
 * Each transpose function must have a prototype of the form:
 * void trans(int M, int N, int A[N][M], int B[M][N]);
 *
 * A transpose function is evaluated by counting the number of misses
 * on a 1KB direct mapped cache with a block size of 32 bytes.
 *
 * Student Name: Luo Yuchen
 * Student ID: 5160309010101
 *
 */ 
#include <stdio.h>
#include "cachelab.h"

int is_transpose(int M, int N, int A[N][M], int B[M][N]);

/* 
 * transpose_submit - This is the solution transpose function that you
 *     will be graded on for Part B of the assignment. Do not change
 *     the description string "Transpose submission", as the driver
 *     searches for that string to identify the transpose function to
 *     be graded. 
 */


char transpose_submit_desc[] = "Transpose submission";
void transpose_submit(int M, int N, int A[N][M], int B[M][N])
{
    int i, j, ii, jj;
    int t0, t1, t2, t3;

    int n = N/8*8;
    int m = M/8*8;

    /* for 64*64 one, devide into several 8*8 matrices, then devide into 4*4 matices */
    if(M==64){
        for(jj = 0;jj < m;jj += 8){
            for(ii = 0;ii < n;ii += 8){
                /* left-up 4*4 matrix */
                for(i = ii; i < ii + 4;i++){
                        j = jj;
                        t0=A[i][j];
                        t1=A[i][j+1];
                        t2=A[i][j+2];
                        t3=A[i][j+3];
                    /* if the 8*8 matrix is on ths diagonal, put value in B at the centrosymmetric position */
                    if(ii == jj){
                        B[63-j][63-i]=t0;
                        B[63-(j+1)][63-i]=t1;
                        B[63-(j+2)][63-i]=t2;
                        B[63-(j+3)][63-i]=t3; 
                    }
                    else{
                        B[j][i]=t0;
                        B[j+1][i]=t1;
                        B[j+2][i]=t2;
                        B[j+3][i]=t3;                         
                    }                   
                }
                
                /* left-down 4*4 matrix */
                for(i = ii+4; i < ii + 8;i++){
                        j = jj;
                        t0=A[i][j];
                        t1=A[i][j+1];
                        t2=A[i][j+2];
                        t3=A[i][j+3];
                    if(ii == jj){
                        B[63-j][63-i]=t0;
                        B[63-(j+1)][63-i]=t1;
                        B[63-(j+2)][63-i]=t2;
                        B[63-(j+3)][63-i]=t3; 
                    }else{
                        B[j][i]=t0;
                        B[j+1][i]=t1;
                        B[j+2][i]=t2;
                        B[j+3][i]=t3;    
                    }            
                }

                /* right-down 4*4 matrix */
                for(i = ii+4; i < ii + 8;i++){
                        j = jj+4;
                        t0=A[i][j];
                        t1=A[i][j+1];
                        t2=A[i][j+2];
                        t3=A[i][j+3];
                    if(ii == jj){
                        B[63-j][63-i]=t0;
                        B[63-(j+1)][63-i]=t1;
                        B[63-(j+2)][63-i]=t2;
                        B[63-(j+3)][63-i]=t3; 
                    }else{
                        B[j][i]=t0;
                        B[j+1][i]=t1;
                        B[j+2][i]=t2;
                        B[j+3][i]=t3; 
                    }        
                }
                /* right-up 4*4 matrix */
                for(i = ii; i < ii + 4;i++){
                        j = jj+4;
                        t0=A[i][j];
                        t1=A[i][j+1];
                        t2=A[i][j+2];
                        t3=A[i][j+3];
                    if(ii == jj){
                        B[63-j][63-i]=t0;
                        B[63-(j+1)][63-i]=t1;
                        B[63-(j+2)][63-i]=t2;
                        B[63-(j+3)][63-i]=t3; 
                    }else{
                        B[j][i]=t0;
                        B[j+1][i]=t1;
                        B[j+2][i]=t2;
                        B[j+3][i]=t3;  
                    }              
                }
            }
        }
        /* recover the misplaced value in B to the right position*/
        ii=0;
        jj=0;
        while(ii<32){
            for(i=ii;i< ii+8;i++){
                for(j=jj;j< jj+8;j++){
                    t0=B[i][j];
                    B[i][j]=B[63-i][63-j];
                    B[63-i][63-j]=t0;
                }
            }
            ii+=8;
            jj+=8;
        }
    }
    /* for 32*32 or 61*47 one, devide into several 8*8 matrices */
    else{
        int skip_idx;
        int skip_val;
        for(jj=0;jj<m;jj += 8){
            for(ii=0;ii<n;ii += 8){
                for(i=ii;i< ii+8;i++){
                    skip_idx = -1;
                    for(j=jj;j < jj+8;j++){
                        /* for value on the diagonal, skip placing */
                        if(i==j){
                            skip_idx = j;
                            skip_val = A[i][j];
                            continue;
                        }
                        B[j][i] = A[i][j];
                    }
                    /* place the skipped value */
                    if(skip_idx != -1){
                        B[i][i] = skip_val;
                    }
                }
            }
        }
    }

    if(m==M && n==N){
        return;
    }
    /* deal with the left part in 61*67 matrix */
    for(ii=0;ii<n;ii+=8){
        /* left-up */
        for(i=ii;i < ii+4;i++){
            j=m;
            t0=A[i][j];
            t1=A[i][j+1];
            t2=A[i][j+2];
            t3=A[i][j+3];
            B[j][i]=t0;
            B[j+1][i]=t1;
            B[j+2][i]=t2;
            B[j+3][i]=t3; 
        }
        /* left-down */
        for(i=ii+4;i < ii+8;i++){
            j=m;
            t0=A[i][j];
            t1=A[i][j+1];
            t2=A[i][j+2];
            t3=A[i][j+3];
            B[j][i]=t0;
            B[j+1][i]=t1;
            B[j+2][i]=t2;
            B[j+3][i]=t3; 
        }
        /* right-down */
        for(i=ii+4;i < ii+8;i++){
            j=m+4;
            t0=A[i][j];            
            B[j][i]=t0;
        }
        /* right-up */
        for(i=ii;i < ii+4;i++){
            j=m+4;
            t0=A[i][j];
            B[j][i]=t0;            
        }        
    } 
    for(j=0;j<m;j+=4){
        for(i=n;i<N;i++){
            t0=A[i][j];
            t1=A[i][j+1];
            t2=A[i][j+2];
            t3=A[i][j+3];
            B[j][i]=t0;
            B[j+1][i]=t1;
            B[j+2][i]=t2;
            B[j+3][i]=t3; 
        }
    }     
    for(i=n;i<N;i++){
        for(j=m;j<M;j++){
            B[j][i] = A[i][j];
        }
    }
}

/* 
 * You can define additional transpose functions below. We've defined
 * a simple one below to help you get started. 
 */ 
char A_submit_desc[] = "Transpose A";
void A_submit(int M, int N, int A[N][M], int B[M][N])
{
    int i, j, ii, jj;
    int t0, t1, t2, t3;

    int n = N/8*8;
    int m = M/8*8;

    if(M==64){
        for(jj = 0;jj < m;jj += 8){
            for(ii = 0;ii < n;ii += 8){
                /* left-up */
                for(i = ii; i < ii + 4;i++){
                        j = jj;
                        t0=A[i][j];
                        t1=A[i][j+1];
                        t2=A[i][j+2];
                        t3=A[i][j+3];

                    if(ii == jj){
                        B[63-j][63-i]=t0;
                        B[63-(j+1)][63-i]=t1;
                        B[63-(j+2)][63-i]=t2;
                        B[63-(j+3)][63-i]=t3; 
                    }
                    else{
                        B[j][i]=t0;
                        B[j+1][i]=t1;
                        B[j+2][i]=t2;
                        B[j+3][i]=t3;                         
                    }                   
                }
                
                /* left-down */
                for(i = ii+4; i < ii + 8;i++){
                        j = jj;
                        t0=A[i][j];
                        t1=A[i][j+1];
                        t2=A[i][j+2];
                        t3=A[i][j+3];
                    if(ii == jj){
                        B[63-j][63-i]=t0;
                        B[63-(j+1)][63-i]=t1;
                        B[63-(j+2)][63-i]=t2;
                        B[63-(j+3)][63-i]=t3; 
                    }else{
                        B[j][i]=t0;
                        B[j+1][i]=t1;
                        B[j+2][i]=t2;
                        B[j+3][i]=t3;    
                    }            
                }
                /* right-down */
                for(i = ii+4; i < ii + 8;i++){
                        j = jj+4;
                        t0=A[i][j];
                        t1=A[i][j+1];
                        t2=A[i][j+2];
                        t3=A[i][j+3];
                    if(ii == jj){
                        B[63-j][63-i]=t0;
                        B[63-(j+1)][63-i]=t1;
                        B[63-(j+2)][63-i]=t2;
                        B[63-(j+3)][63-i]=t3; 
                    }else{
                        B[j][i]=t0;
                        B[j+1][i]=t1;
                        B[j+2][i]=t2;
                        B[j+3][i]=t3; 
                    }        
                }
                /* right-up */
                for(i = ii; i < ii + 4;i++){
                        j = jj+4;
                        t0=A[i][j];
                        t1=A[i][j+1];
                        t2=A[i][j+2];
                        t3=A[i][j+3];
                    if(ii == jj){
                        B[63-j][63-i]=t0;
                        B[63-(j+1)][63-i]=t1;
                        B[63-(j+2)][63-i]=t2;
                        B[63-(j+3)][63-i]=t3; 
                    }else{
                        B[j][i]=t0;
                        B[j+1][i]=t1;
                        B[j+2][i]=t2;
                        B[j+3][i]=t3;  
                    }              
                }
            }
        }
        ii=0;
        jj=0;
        while(ii<32){
            for(i=ii;i< ii+8;i++){
                for(j=jj;j< jj+8;j++){
                    t0=B[i][j];
                    B[i][j]=B[63-i][63-j];
                    B[63-i][63-j]=t0;
                }
            }
            ii+=8;
            jj+=8;
        }
    }
    else{
        int skip_idx;
        int skip_val;
        for(jj=0;jj<m;jj += 8){
            for(ii=0;ii<n;ii += 8){
                for(i=ii;i< ii+8;i++){
                    skip_idx = -1;
                    for(j=jj;j < jj+8;j++){
                        if(i==j){
                            skip_idx = j;
                            skip_val = A[i][j];
                            continue;
                        }
                        B[j][i] = A[i][j];
                    }
                    if(skip_idx != -1){
                        B[i][i] = skip_val;
                    }
                }
            }
        }
    }

    if(m==M && n==N){
        return;
    }
    
    for(ii=0;ii<n;ii+=8){
        /* left-up */
        for(i=ii;i < ii+4;i++){
            j=m;
            t0=A[i][j];
            t1=A[i][j+1];
            t2=A[i][j+2];
            t3=A[i][j+3];
            B[j][i]=t0;
            B[j+1][i]=t1;
            B[j+2][i]=t2;
            B[j+3][i]=t3; 
        }
        /* left-down */
        for(i=ii+4;i < ii+8;i++){
            j=m;
            t0=A[i][j];
            t1=A[i][j+1];
            t2=A[i][j+2];
            t3=A[i][j+3];
            B[j][i]=t0;
            B[j+1][i]=t1;
            B[j+2][i]=t2;
            B[j+3][i]=t3; 
        }
        /* right-down */
        for(i=ii+4;i < ii+8;i++){
            j=m+4;
            t0=A[i][j];            
            B[j][i]=t0;
        }
        /* right-up */
        for(i=ii;i < ii+4;i++){
            j=m+4;
            t0=A[i][j];
            B[j][i]=t0;            
        }        
    } 

    for(j=0;j<m;j+=4){
        for(i=n;i<N;i++){
            t0=A[i][j];
            t1=A[i][j+1];
            t2=A[i][j+2];
            t3=A[i][j+3];
            B[j][i]=t0;
            B[j+1][i]=t1;
            B[j+2][i]=t2;
            B[j+3][i]=t3; 
        }
    }
    
    
    for(i=n;i<N;i++){
        for(j=m;j<M;j++){
            B[j][i] = A[i][j];
        }
    }
}

/* 
 * trans - A simple baseline transpose function, not optimized for the cache.
 */
char trans_desc[] = "Simple row-wise scan transpose";
void trans(int M, int N, int A[N][M], int B[M][N])
{
    int i, j, tmp;
    
    for (i = 0; i < N; i++) {
        for (j = 0; j < M; j++) {
            tmp = A[i][j];
            B[j][i] = tmp;
        }
    }   

    
}

/*
 * registerFunctions - This function registers your transpose
 *     functions with the driver.  At runtime, the driver will
 *     evaluate each of the registered functions and summarize their
 *     performance. This is a handy way to experiment with different
 *     transpose strategies.
 */
void registerFunctions()
{
    /* Register your solution function */
    registerTransFunction(transpose_submit, transpose_submit_desc); 

    /* Register any additional transpose functions */
    //registerTransFunction(trans, trans_desc); 
    registerTransFunction(A_submit, A_submit_desc); 
}

/* 
 * is_transpose - This helper function checks if B is the transpose of
 *     A. You can check the correctness of your transpose by calling
 *     it before returning from the transpose function.
 */
int is_transpose(int M, int N, int A[N][M], int B[M][N])
{
    int i, j;

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; ++j) {
            if (A[i][j] != B[j][i]) {
                return 0;
            }
        }
    }
    return 1;
}

