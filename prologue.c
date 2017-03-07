/*glue file for our code*/
/*TODO list:
 * change write_sob_void to return without printing (and no newline)
 * add #define SUBMISION to avoid printing debug messages
 * check implementation of string->symbol (might create duplicate symbols)
 * debug
 */

#include "./arch/cisc.h"


#define CHECK_TYPE(arg,type) 	PUSH(R1); \
								MOV(R1,IMM(arg)); \
								ADD(R1,IMM(2)); \
								PUSH(FPARG(R1)); \
								CALL(IS_SOB_##type); \
								DROP(1); \
								CMP(R0,IMM(0)); \
								JUMP_EQ(Lerror_incorr_type); \
								POP(R1); 

//give this the actual expected, without SOB_NIL
#define CHECK_ARGC(expected)	PUSH(R1); \
								MOV(R1,IMM(expected)); \
								INCR(R1); \
								CMP(FPARG(1),R1); \
								PUSH(IMM(__FILE__)); \
								PUSH(IMM(__LINE__)); \
							  	JUMP_NE(Lerror_incorr_num_of_args); \
							  	DROP(2); \
							  	POP(R1);

#define SOB_NIL 	IMM(1)
#define SOB_VOID 	IMM(2)
#define SOB_FALSE 	IMM(3)
#define SOB_TRUE 	IMM(5)

#define EMPTY_ENV 	(-1) //TODO what to put here? does 0 mess up the stack?

#define SUBMISSION //TODO uncomment before submitting!!!!!!!!11
#ifndef SUBMSISSION
	#define PRINT_STACK printf("*********** <-SP\n"); \
				        for(ptr=SP ; &STACK(ptr) != M(stack) ;){ \
				            data = STACK(--ptr); \
				            printf("*%5ld%s  *",data,data>10000? "":"  "); \
				            if(FP-ptr == 0) printf(" <-FP"); \
				            if(FP-ptr == 3) printf(" <-FPARG(0)"); \
				            printf("\n"); } \
				        printf("***********\n");

	#define PRINT_STACK_MSG(place) printf("printing from: %s\n",place);  \
	                                PRINT_STACK

	#define DEBUG(msg...)	if(dbg) fprintf(stderr,msg);

	#define DEBUG_CMP 	DEBUG("test_result = %s\n",M(test_result)? "true":"false");
#else
	#define PRINT_STACK ;
    #define PRINT_STACK_MSG(place) ;
    #define DEBUG(msg...) ;
    #define DEBUG_CMP ;
#endif

int main()
{
	int dbg;
	long ptr;
	long data;
#ifdef SUBMISSION
	dbg=0;
#else
	dbg=1;
#endif

DEBUG("starting machine\n");
  START_MACHINE;

  JUMP(CONTINUE);

#include "./arch/char.lib"
#include "./arch/io.lib"
#include "./arch/math.lib"
#include "./arch/scheme.lib"
#include "./arch/string.lib"
#include "./arch/system.lib"

#include "./arch/our_lib/lib.h"

   CONTINUE:
PUSH(IMM(0));//old FP
PUSH(IMM(SOB_NIL));//start of global frame
PUSH(IMM(1));//argc
PUSH(IMM(EMPTY_ENV));//env   
CALL(START_CODE_LABEL);

JUMP(STOP_MACHINE_LABEL);

START_CODE_LABEL:
PUSH(FP);
MOV(FP,SP);

DEBUG("starting code\n");

