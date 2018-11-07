/*
 *
 * Student Name:Luo Yuchen
 * Student ID:516030910101
 *
 */
#include "cachelab.h"
#include <getopt.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

/* Macros */
typedef struct{
    int valid;  /* valid bit */
    int tag;    /* tag */
    int count;  /* The access time used as a choice criteria in eviction */
} LINE;    /* Cache line struct */

#define getNum(n){1<<(n)};/* Used to calculate S (2^s) */

/* Global variable */
int SETBIT;         /* s */
int LINES_PER_SET;  /* E */
int BLOCKBIT;       /* b */
char* trace_name;   /* t */

LINE* CACHE;        /* Pointing to the cache array */

static int COUNT;   /* The total amount of command */
static int HIT_COUNT;   /* The amount of cache hit */
static int MISS_COUNT;  /* The amount of cache miss */
static int EVIC_COUNT;  /* The amount of cache line eviction */

/* Function declaration */
int parseCmd(int argc, char* argv[]);
void init();
void run();
void analyse(char* msg);
char parseType(char* msg);
int parseAddr(char* msg);
int parseSetIdx(int addr);
int parseTag(int addr);
int check_hit(int set_idx, int tag);


/* Function implemention */

/*
 * parseCmd - parse parameters in the command line * 
 */
int parseCmd(int argc, char* argv[]){
    int ch;
    while((ch = getopt(argc,argv,"s:E:b:t:")) != -1){
        switch(ch){            
            case 's':{
                SETBIT = atoi(optarg);
                break;
            }
            case 'E':{
                LINES_PER_SET = atoi(optarg);
                break;
            }
            case 'b':{
                BLOCKBIT = atoi(optarg);
                break;
            }
            case 't':{
                trace_name = optarg;
                break;
            }
            default:{
                return 0;
            }
        }
    }
    return 1;
}

/*
 * init - initilize the CACHE * 
 */
void init(){
    int set_num = getNum(SETBIT);
    CACHE = (LINE*)malloc(set_num * LINES_PER_SET * sizeof(LINE));
    int lenth = set_num * LINES_PER_SET;
    for(int i=0;i<lenth;i++){
        CACHE[i].valid = 0;
        CACHE[i].count = 0;
    }
}

/*
 * parseType parseAddr - Given a Valgrind trace, parse the command type and memory address * 
 */
char parseType(char* msg){
    char* ch = msg;
    while(*ch == ' '){
        ch++;
    }
    return *ch;    
}
int parseAddr(char* msg){
    char* ch = msg;
    while((*ch == ' ')||(*ch=='L')||(*ch == 'S')||(*ch=='M')||(*ch == 'I')){
        ch++;
    }
    char* addr = malloc(12*sizeof(char));
    addr[0] = '0';
    addr[1] = 'x';
    for(int i=0;ch[i]!=',';i++){
        addr[i+2]=ch[i];
    }
    int addrValue = 0;
    sscanf(addr,"%x",&addrValue);
    return addrValue;
}

/*
 * parseSetIdx parseTag - Given a memory address, parse the set index and tag * 
 */
int parseSetIdx(int addr){
    int mask = (1 << SETBIT) - 1;
    int result = (addr >> BLOCKBIT) & mask;
    return result;
}
int parseTag(int addr){
    return addr >> (BLOCKBIT + SETBIT);
}

/*
 * check_hit - The main function checking whether a operation hit the cache or not * 
 */
int check_hit(int set_idx, int tag){
    COUNT++;
    LINE temp;

    int evic_idx = set_idx * LINES_PER_SET;
    int add_idx = evic_idx;
    int conflict = 1;
    
    for(int i = evic_idx; i < (set_idx+1)*LINES_PER_SET;i++){
        temp = CACHE[i];
        if(!temp.valid){            /* still have free cache line in this set */
            add_idx = i;
            conflict = 0;
        }
        else {
            if(temp.tag == tag){    /* cache hit */
                HIT_COUNT++;
                CACHE[i].count = COUNT;
                return 1;
            }
            if(conflict && CACHE[evic_idx].count > temp.count){ /* the line that has the minimum count is to be evicted */
                evic_idx = i;
            }
        }
    }

    MISS_COUNT++;
    
    if(!conflict){       /* Not a conflict miss */
        CACHE[add_idx].valid = 1;
        CACHE[add_idx].tag = tag;
        CACHE[add_idx].count = COUNT;
    }
    else{               /* A conflict miss */
        EVIC_COUNT++;
        CACHE[evic_idx].tag = tag;
        CACHE[evic_idx].count = COUNT;
    }
    return 0;
}

/*
 * analyse - The entry of trace resolvation * 
 */
void analyse(char* msg){
    char type = parseType(msg);

    if(type == 'M'){
      HIT_COUNT++;
    }
    else if(type == 'I'){
        return;
    }

    int addr = parseAddr(msg);

    int set_idx = parseSetIdx(addr);
    int tag = parseTag(addr);
    
    check_hit(set_idx,tag);
      
}

/*
 * run - the general function run the program * 
 */
void run(){
    char buffer[100];
    FILE* ff = fopen(trace_name,"r");
    if(!ff){
        printf("file name:%s OPEN ERROR!\n",trace_name);
        return;
    }
    while(fgets(buffer,100,ff)!=NULL){
        analyse(buffer);
    }
}


int main(int argc, char* argv[])
{
    if(!parseCmd(argc,argv)){
        printf("Command Parse Error!");
        return 0;
    }

    COUNT = 0;
    HIT_COUNT = 0;
    MISS_COUNT = 0;
    EVIC_COUNT = 0;

    init();
    run();

    printSummary(HIT_COUNT, MISS_COUNT, EVIC_COUNT);
    return 0;
}
