/*
 * mm-naive.c - The fastest, least memory-efficient malloc package.
 * 
 * In this naive approach, a block is allocated by simply incrementing
 * the brk pointer.  A block is pure payload. There are no headers or
 * footers.  Blocks are never coalesced or reused. Realloc is
 * implemented directly using mm_malloc and mm_free.
 *
 * NOTE TO STUDENTS: Replace this header comment with your own header
 * comment that gives a high level description of your solution.
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>

#include "mm.h"
#include "memlib.h"

/* single word (4) or double word (8) alignment */
#define ALIGNMENT 8

/* rounds up to the nearest multiple of ALIGNMENT */
#define ALIGN(size) (((size) + (ALIGNMENT-1)) & ~0x7)
#define SIZE_T_SIZE (ALIGN(sizeof(size_t)))

/* Basic constants and macros */
#define WSIZE 4
#define DSIZE 8
#define OVERHEAD 8 /* Size of the overhead for each memory block */
#define MIN_BLOCK 24
#define SEC_BLOCK 32
#define CHUNKSIZE (1<<12)

#define MAX(x,y)((x)>(y)?(x):(y))

/* Pack a size and allocated bit into a word */
#define PACK(size,alloc)((size)|(alloc))

/* Read and write a word at address p */
#define GET(p)(*(unsigned int *)(p))
#define PUT(p,val)(*(unsigned int *)(p) = (unsigned int)(val))

/* Read the size and allocated fields from address p */
#define GET_SIZE(p)(GET(p) & ~0x7)
#define GET_ALLOC(p)(GET(p) & 0x1)

/* Given block ptr bp, compute address of its header and footer and potential success free block in the free list*/
#define HDRP(bp)((char*)(bp) - WSIZE)
#define FTRP(bp)((char*)(bp) + GET_SIZE(HDRP(bp)) - OVERHEAD)

#define PREV(bp)((char*)(bp))
#define SUCC(bp)((char*)(bp) + DSIZE)

#define SET_PREV(bp,ptr)(*(unsigned long*)(PREV(bp)) = (unsigned long)(ptr))
#define SET_SUCC(bp,ptr)(*(unsigned long*)(SUCC(bp)) = (unsigned long)(ptr))
/* Given block ptr bp, compute address of next and previous blocks*/
#define NEXT_BLKP(bp)((char*)(bp) + GET_SIZE(HDRP(bp)))
#define PREV_BLKP(bp)((char*)(bp) - GET_SIZE((char*)(bp)-OVERHEAD))
#define NEXT_FREEP(bp)((char*)(*(unsigned int **)(SUCC(bp))))
#define PREV_FREEP(bp)((char*)(*(unsigned int **)(PREV(bp))))

/* Pointers for free list (size = 24 * 2^n)*/
static unsigned int BLANK = 0;
static unsigned int* BLANKADDR;

static char* ptr_0; /* total size -> 24 */
static char* ptr_00; /* total size -> 32 (special)*/
static char* ptr_1; /* total size -> (32,48] */
static char* ptr_2; /* total size -> (48,96] */
static char* ptr_3; /* total size -> (96,192] */
static char* ptr_4; /* total size -> (192,384] */
static char* ptr_5; /* total size -> (384,768] */
static char* ptr_6; /* total size -> (768,1536] */
static char* ptr_7; /* total size -> (1536,) */

static char* heap_listp;/* Point to the head of heap list*/


/* Function declaration*/
static void* extend_heap(size_t words);
static void* find_fit(size_t size);
static void* place(void* bp, size_t size);
static void *coalesce(void *bp);
static int mylog(double n);
static void ADD(void* bp);
static int getMark(size_t size);
static void REMOVE(void* bp);

/* Function calculating log2(n/24) */
static int mylog(double n){
    double temp = n/24;
    int num = 0x1;
    int count = 0;
    while(num < temp && count < 7){
        num = (num << 1);
        count++;
    }
    return count;
}

/* Operations on free list*/

/*
 * ADD - 
 */
static void ADD(void* bp){
    size_t size = GET_SIZE(HDRP(bp));
    int mark = getMark(size);
    SET_PREV(bp,BLANKADDR);
    
    switch(mark){
        case 0:{
            SET_SUCC(bp,ptr_0);
            if(ptr_0 != (char*)BLANKADDR){
                SET_PREV(ptr_0,bp);
            }        
            ptr_0 = bp;
            return;
        }
        case 1:{
            SET_SUCC(bp,ptr_00);
            if(ptr_00 != (char*)BLANKADDR){
                SET_PREV(ptr_00,bp);
            }        
            ptr_00 = bp;
            return;
        }
        case 2:{
            SET_SUCC(bp,ptr_1);
            if(ptr_1 != (char*)BLANKADDR){
                SET_PREV(ptr_1,bp);
            }        
            ptr_1 = bp;
            return;
        }
        case 3:{
            SET_SUCC(bp,ptr_2);
            if(ptr_2 != (char*)BLANKADDR){
                SET_PREV(ptr_2,bp);
            }        
            ptr_2 = bp;
            return;
        }
        case 4:{
            SET_SUCC(bp,ptr_3);
            if(ptr_3 != (char*)BLANKADDR){
                SET_PREV(ptr_3,bp);
            }        
            ptr_3 = bp;
            return;
        }
        case 5:{
            SET_SUCC(bp,ptr_4);
            if(ptr_4 != (char*)BLANKADDR){
                SET_PREV(ptr_4,bp);
            }        
            ptr_4 = bp;
            return;
        }
        case 6:{
            SET_SUCC(bp,ptr_5);
            if(ptr_5 != (char*)BLANKADDR){
                SET_PREV(ptr_5,bp);
            }        
            ptr_5 = bp;
            return;
        }
        case 7:{
            SET_SUCC(bp,ptr_6);
            if(ptr_6 != (char*)BLANKADDR){
                SET_PREV(ptr_6,bp);
            }        
            ptr_6 = bp;
            return;
        }
        default:{
            SET_SUCC(bp,ptr_7);
            if(ptr_7 != (char*)BLANKADDR){
                SET_PREV(ptr_7,bp);
            }        
            ptr_7 = bp;
            return;
        }
    }
}

static int getMark(size_t size){
    int mark = mylog(size);
    if(size > SEC_BLOCK){
        mark += 1;
    }
    return mark;
}

static void REMOVE(void* bp){
    void* prev = PREV_FREEP(bp);
    void* succ = NEXT_FREEP(bp);
    
    if(prev == (char*)BLANKADDR){
        if(succ != (char*)BLANKADDR){
            SET_PREV(succ,prev);      
        }
        size_t size = GET_SIZE(HDRP(bp));
        int mark = getMark(size);
        switch(mark){
            case 0: ptr_0 = succ;break;
            case 1: ptr_00 = succ;break;
            case 2: ptr_1 = succ;break;
            case 3: ptr_2 = succ;break;
            case 4: ptr_3 = succ;break;
            case 5: ptr_4 = succ;break;
            case 6: ptr_5 = succ;break;
            case 7: ptr_6 = succ;break;
            default: ptr_7 = succ;break;
        }
        return;
    }
    else{
        SET_SUCC(prev,succ);
        if(succ != (char*)BLANKADDR){
            SET_PREV(succ,prev);   
        }
        return;
    }  
}

/* 
 * mm_init - initialize the malloc package.
 */
int mm_init(void)
{
    
    if((heap_listp=mem_sbrk(4*WSIZE)) == (void*)-1){
        return -1;
    }

    BLANKADDR = &BLANK;
    ptr_0 = (char*)BLANKADDR; /* total size -> 24 */
    ptr_00= (char*)BLANKADDR; /* total size -> 32 (special)*/
    ptr_1 = (char*)BLANKADDR; /* total size -> (32,48] */
    ptr_2 = (char*)BLANKADDR; /* total size -> (48,96] */
    ptr_3 = (char*)BLANKADDR; /* total size -> (96,192] */
    ptr_4 = (char*)BLANKADDR; /* total size -> (192,384] */
    ptr_5 = (char*)BLANKADDR; /* total size -> (384,768] */
    ptr_6 = (char*)BLANKADDR; /* total size -> (768,1536] */
    ptr_7 = (char*)BLANKADDR; /* total size -> (1536,) */

    PUT(heap_listp,0); /* Alignment padding*/
    PUT(heap_listp + WSIZE,PACK(DSIZE,1)); /* Prologue header */
    PUT(heap_listp + DSIZE,PACK(DSIZE,1)); /* Prologue footer */
    PUT(heap_listp + WSIZE + DSIZE,PACK(0,1)); /* Epilogue header*/
    
    heap_listp += DSIZE;

    void* bp = extend_heap(CHUNKSIZE/WSIZE);

    if( bp == NULL){
        return -1;
    }
    return 0;
}

static void* extend_heap(size_t words){
    char* bp;
    size_t size;

    size = (words%2) ? (words+1)*WSIZE : words*WSIZE;
    if((bp=mem_sbrk(size)) == (void*)-1){
        return NULL;
    }

    PUT(HDRP(bp), PACK(size,0)); /* Free block header */
    PUT(FTRP(bp), PACK(size,0)); /* Free block footer */
    SET_PREV(bp,BLANKADDR);/* Previous free pointer doesn't exist */
    SET_SUCC(bp,BLANKADDR); /* Success free pointer doesn't exist */

    PUT(HDRP(NEXT_BLKP(bp)),PACK(0,1)); /* New epilogue header */

    return coalesce(bp);
}

/* 
 * mm_malloc - Allocate a block by incrementing the brk pointer.
 *     Always allocate a block whose size is a multiple of the alignment.
 */
void *mm_malloc(size_t size)
{
    size_t asize;
    size_t extendsize;
    char* bp;

    if(size <= 0){          /* Ignore spurious requests */
        return NULL;
    }
    if(size <= 2*DSIZE){
        asize = MIN_BLOCK;
    }
    else{                   /* Adjust the block size*/
        asize = ALIGN(size + OVERHEAD);
    }

    if((bp=find_fit(asize)) != NULL){
        return bp;
    }
    extendsize = MAX(asize,CHUNKSIZE);
    if((bp = extend_heap(extendsize/WSIZE)) == NULL){
        return NULL;
    }
    return place(bp,asize);
}

static void* find_fit(size_t size){
    int mark = getMark(size);
    void* bp;
    switch(mark){        
        case 0:{                        /* size -> 24 */                       
            if(ptr_0 != (char*)BLANKADDR){
                if((bp=place(ptr_0,size)) != NULL)
                    return bp;
            }
        }
        case 1:{                       
            if(ptr_00 != (char*)BLANKADDR){ /* size -> 32 */  
                if((bp=place(ptr_00,size)) != NULL)
                    return bp;
            } 
        }                             
        case 2:{
            if(ptr_1 != (char*)BLANKADDR){ /* size -> (32,48] */
                if((bp=place(ptr_1,size)) != NULL)
                    return bp;
            }
        }
        case 3:{                      /* size -> (48,96] */
            if(ptr_2 != (char*)BLANKADDR){
                if((bp=place(ptr_2,size)) != NULL)
                return bp;
            }
        }
        case 4:{                      /* size -> (96,192] */
            if(ptr_3 != (char*)BLANKADDR){
               if((bp=place(ptr_3,size)) != NULL)
                    return bp;
            }
        }
        case 5:{                     /* size -> (192,384] */
            if(ptr_4 != (char*)BLANKADDR){
                if((bp=place(ptr_4,size)) != NULL)
                    return bp;
            }
        }
        case 6:{                     /* size -> (384,768] */
            if(ptr_5 != (char*)BLANKADDR){
                if((bp=place(ptr_5,size)) != NULL)
                    return bp;
            }
        }
        case 7:{                     /* size -> (768,1536] */
            if(ptr_6 != (char*)BLANKADDR){
                if((bp=place(ptr_6,size)) != NULL)
                    return bp;
            }
        }
        default:{                     /* size -> (1536,) */
            if(ptr_7 != (char*)BLANKADDR){
                if((bp=place(ptr_7,size)) != NULL)
                    return bp;
            }
            else{
                return NULL;   /* no fit */
            }
        }
    }
}

static void* place(void* bp, size_t size)
{
    size_t csize = GET_SIZE(HDRP(bp));
    size_t other;

    while(bp != (char*)BLANKADDR){
        csize = GET_SIZE(HDRP(bp));
        if(csize >= size){
            REMOVE(bp);
            other = csize - size;                
            if(other >= MIN_BLOCK){
                PUT(HDRP(bp),PACK(size,1));
                PUT(FTRP(bp),PACK(size,1));
                bp = NEXT_BLKP(bp);
                PUT(HDRP(bp),PACK(other,0));
                PUT(FTRP(bp),PACK(other,0));
                ADD(bp);
                bp = PREV_BLKP(bp);
            }
            else{
                PUT(HDRP(bp),PACK(csize,1));
                PUT(FTRP(bp),PACK(csize,1));
            }
            return bp;
        }
        bp = NEXT_FREEP(bp);        
    }    
    return NULL;        
}

/*
 * mm_free - Freeing a block does nothing.
 */
void mm_free(void *ptr)
{
    size_t size = GET_SIZE(HDRP(ptr));

    PUT(HDRP(ptr),PACK(size,0));
    PUT(FTRP(ptr),PACK(size,0));
    if(getMark(size)<6){
        ADD(ptr);
        return;
    }
    coalesce(ptr);
}

static void *coalesce(void *bp){
    size_t size = GET_SIZE(HDRP(bp));
    size_t prev_alloc = GET_ALLOC(HDRP(PREV_BLKP(bp)));
    size_t succ_alloc = GET_ALLOC(HDRP(NEXT_BLKP(bp)));

    if(prev_alloc && succ_alloc){
        ADD(bp);
        return bp;
    }
    if(!prev_alloc && succ_alloc){
        bp = PREV_BLKP(bp);
        size_t prev_size = GET_SIZE(HDRP(bp));
        REMOVE(bp);
        size += prev_size;

        PUT(HDRP(bp),PACK(size,0));
        PUT(FTRP(bp),PACK(size,0));
        ADD(bp);
        return bp;
    }
    if(prev_alloc && !succ_alloc){
        bp = NEXT_BLKP(bp);
        size_t next_size = GET_SIZE(HDRP(bp));
        REMOVE(bp);
        size += next_size;

        bp = PREV_BLKP(bp);
        PUT(HDRP(bp),PACK(size,0));
        PUT(FTRP(bp),PACK(size,0));
        ADD(bp);
        return bp;
    }
    else{
        bp = NEXT_BLKP(bp);
        size_t next_size = GET_SIZE(HDRP(bp));
        REMOVE(bp);
        size += next_size;

        bp = PREV_BLKP(PREV_BLKP(bp));
        size_t prev_size = GET_SIZE(HDRP(bp));
        REMOVE(bp);
        size += prev_size;

        PUT(HDRP(bp),PACK(size,0));
        PUT(FTRP(bp),PACK(size,0));
        ADD(bp);
        return bp;
    }
}

/*
 * mm_realloc - Implemented simply in terms of mm_malloc and mm_free
 */
void *mm_realloc(void *ptr, size_t size)
{
    if(ptr == NULL){
        return mm_malloc(size);
    }

    if(size <= 0){
        mm_free(ptr);
        return NULL;
    }

    void *oldptr = ptr;
    void *newptr;
    size_t oldSize = GET_SIZE(HDRP(oldptr));
    size_t newSize;
    if(size <= 2*DSIZE){
        newSize = MIN_BLOCK;
    }
    else{                   /* Adjust the block size*/
        newSize = ALIGN(size + OVERHEAD);
    }
    size_t gap;

    if(oldSize >= newSize){      /* The existing block is big enough*/
        gap = oldSize - newSize;
        if(gap >= MIN_BLOCK){
            PUT(HDRP(oldptr),PACK(newSize,1));
            PUT(FTRP(oldptr),PACK(newSize,1));
            oldptr = NEXT_BLKP(oldptr);
            PUT(HDRP(oldptr),PACK(gap,0));
            PUT(FTRP(oldptr),PACK(gap,0));
            ADD(oldptr);
            oldptr = PREV_BLKP(oldptr);
        }
        return oldptr;
    }
    else{                       /* Extra memory space is needed */
        void* next = NEXT_BLKP(oldptr);
        if(!GET_ALLOC(HDRP(next)) && (GET_SIZE(HDRP(next)) + oldSize >= newSize)){  /* The adjacent free block can be used */
            size_t next_size = GET_SIZE(HDRP(next));
            REMOVE(next);

            gap = oldSize + next_size - newSize;
            if(gap >= MIN_BLOCK){
                PUT(HDRP(oldptr),PACK(newSize,1));
                PUT(FTRP(oldptr),PACK(newSize,1));
                oldptr = NEXT_BLKP(oldptr);
                PUT(HDRP(oldptr),PACK(gap,0));
                PUT(FTRP(oldptr),PACK(gap,0));
                ADD(oldptr);
                oldptr = PREV_BLKP(oldptr);
            }
            else{
                PUT(HDRP(oldptr),PACK(oldSize+next_size,1));
                PUT(FTRP(oldptr),PACK(oldSize+next_size,1));
            }
            return oldptr;
        }
        else{                       /* A brand new memory block need to be allocated */
            size_t copySize;
            newptr = mm_malloc(newSize);
            if (newptr == NULL){
                return NULL;
            }
            copySize = oldSize - OVERHEAD;
            memcpy(newptr, oldptr, copySize);
            mm_free(oldptr);
            return newptr;
        }
    }   
}














