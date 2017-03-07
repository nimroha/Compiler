 /* other functions */
/*applic appends SOB_NIL to every argument list, so argc is always 1 larger than needed*/


/*eq?*/
FUNC_EQ_Q: //compares addresses, then types, then fields
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1), IMM(3));
	JUMP_NE(Lerror_incorr_num_of_args);
	MOV(R1, FPARG(2));
	MOV(R2, FPARG(3));
	//DEBUG("compare addr: R1: %ld,R2:%ld\n",R1,R2)
	CMP(R1, R2);//compare addresses (shallow eq?)
	JUMP_EQ(FUNC_EQ_Q_TRUE);
	CMP(INDD(R1,0),INDD(R2,0)); //compare types
	JUMP_NE(FUNC_EQ_Q_FALSE);
	//ignore complex types:
	CMP(INDD(R1,0),IMM(T_PAIR));//is pair?
	JUMP_EQ(FUNC_EQ_Q_FALSE);
	CMP(INDD(R1,0),IMM(T_STRING));//is string?
	JUMP_EQ(FUNC_EQ_Q_FALSE);
	CMP(INDD(R1,0),IMM(T_VECTOR));//is vector?
	JUMP_EQ(FUNC_EQ_Q_FALSE);
	
	//DEBUG("compare type: INDD(R1,0):%ld,INDD(R2,0):%ld\n",INDD(R1,0),INDD(R2,0))
	//DEBUG("compare first field: INDD(R1,1):%ld,INDD(R2,1):%ld\n",INDD(R1,0),INDD(R2,0))
	
	CMP(INDD(R1,1),INDD(R2,1));//compare the first field
	JUMP_NE(FUNC_EQ_Q_FALSE);
	
	//DEBUG("check if fraction type: R1:%ld,IMM(T_FRACTION):%ld\n",R1,IMM(T_FRACTION))
	
	CMP(INDD(R1,0),IMM(T_FRACTION));//fractions have another field to check
	JUMP_NE(FUNC_EQ_Q_TRUE);//if not fractions, then first field is enough
	
	//DEBUG("compare second field: INDD(R1,2):%ld,INDD(R2,2):%ld\n",INDD(R1,2),INDD(R2,2))
	CMP(INDD(R1,2),INDD(R2,2));//compare the second field
	JUMP_EQ(FUNC_EQ_Q_TRUE);
	FUNC_EQ_Q_FALSE:
	MOV(R0, IMM(SOB_FALSE));
	JUMP(FUNC_EQ_Q_EXIT);
	FUNC_EQ_Q_TRUE:
	MOV(R0, IMM(SOB_TRUE));
	FUNC_EQ_Q_EXIT:
	POP(FP);
	RETURN;

/*char->integer*/
FUNC_CHAR_2_INT:
  PUSH(FP);
  MOV(FP,SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(Lerror_incorr_num_of_args);
  CHECK_TYPE(0,CHAR);
  MOV(R1,FPARG(2));
  PUSH(IMM(1));
  PUSH(INDD(R1,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  POP(FP);
  RETURN;


/*integer->char*/
FUNC_INT_2_CHAR:
  PUSH(FP);
  MOV(FP,SP);
  CHECK_ARGC(1);
  CHECK_TYPE(0,FRACTION);
  MOV(R1,FPARG(2));
  CMP(INDD(R1,2),IMM(1));//check if integer
  JUMP_NE(Lerror_incorr_type);
  PUSH(INDD(R1,1));
  CALL(MAKE_SOB_CHAR);
  DROP(1);
  POP(FP);
  RETURN;

 FUNC_APPLY:
 	//PRINT_STACK_MSG("before of apply");
 	PUSH(FP);
 	MOV(FP,SP);
	//PRINT_STACK_MSG("start of apply");
 	CHECK_ARGC(2);
 	MOV(R1,FPARG(2));
	//DEBUG("FPARG(2)=%ld\n",FPARG(2));
 	CMP(INDD(R1,0),IMM(T_CLOSURE));
 	JUMP_NE(L_error_cannot_apply_non_clos);
 	PUSH(FPARG(3));
 	CALL(PRV_FUNC_LIST_LENGTH);
	//DEBUG("list length=%ld\n",R0);
 	DROP(1);
 	//PRINT_STACK_MSG("after list length");
	//DEBUG("FPARG(2)=%ld\n",FPARG(2));
 	MOV(R1,FPARG(2));
 	MOV(R2,R0);//argc-1 (+1 for initial SOB_NIL)

 	FUNC_APPLY_PUSH_ARGS:
 	PUSH(IMM(SOB_NIL));//start of new frame
 	MOV(R3,R2);
	 	FUNC_APPLY_PUSH_ARGS_LOOP://while(r3>0)
	 	CMP(R3,IMM(0));
	 	JUMP_EQ(FUNC_APPLY_FIX_STACK);
	 	DECR(R3);
	 	MOV(R5,R3);
	 	MOV(R4,FPARG(3));//start of arg list
		 	FUNC_APPLY_FIND_ARG_LOOP://while(r5>0)
		 	CMP(R5,IMM(0));
		 	JUMP_EQ(FUNC_APPLY_ARG_FOUND);
		 	MOV(R4,INDD(R4,2));//go to next list element
		 	DECR(R5);
		 	JUMP(FUNC_APPLY_FIND_ARG_LOOP);
	 	FUNC_APPLY_ARG_FOUND:
	 	PUSH(INDD(R4,1));//push car
	 	JUMP(FUNC_APPLY_PUSH_ARGS_LOOP);

 	FUNC_APPLY_FIX_STACK: //here: R1=proc, R2=argc-1
 	INCR(R2);
	//DEBUG("proc=%ld, argc=%ld\n",R1,R2);
 	PUSH(R2);//argc
 	PUSH(INDD(R1,1));//env
 	PUSH(FPARG(-1));//ret
 	MOV(R3,FPARG(-2));//save old frame pointer
 	//PRINT_STACK_MSG("start of fix stack");
 	//PRINT_STACK_MSG("before messing with the pointers");
 	ADD(R2,IMM(3));//R2 = args+|argc|+|env|+|ret| = size of frame
 	MOV(R4,SP);
 	MOV(SP,FP);
 	MOV(FP,R4);
 	SUB(SP,IMM(7));//size of old frame, SP now points to bottom of old frame
 	ADD(FP,IMM(3));//FPARG(0) now points to top of new frame

 	//PUSH(FPARG(R2));
 	FUNC_APPLY_FIX_STACK_LOOP:
 	PUSH(FPARG(R2));
 	DECR(R2);
 	CMP(R2,IMM(0));
 	JUMP_NE(FUNC_APPLY_FIX_STACK_LOOP);
	//DEBUG("old FP=%ld\n",R3);
 	MOV(FP,R3);//old FP
 	//MOV(FP,SP);
 	//SUB(FP,IMM(5));//
 	//PRINT_STACK_MSG("after fix stack");

 	CMP(R1,IMM(T_CLOSURE));
	//DEBUG("closure=%ld, label=%ld\n",R1,INDD(R1,2));
 	MOV(R1,INDD(R1,2));//reach label
 	JUMPA(R1);

PRV_FUNC_LOOKUP_CLOSURE: //puts the closure pointer in R0 
	PUSH(FP);
	MOV(FP,SP);
	MOV(R1,FPARG(0));
	MOV(R2,M(fvars));
	MOV(R3,IMM(0));
	L_prv_func_lookup_closure_LOOP: //assuming the id exists in table (no stop condition!!)
	CMP(R1,INDD(R2,R3));
	//DEBUG("comparing closures: %ld,%ld\n",R1,INDD(R2,R3));
	JUMP_EQ(L_prv_func_lookup_closure_EXIT);
	ADD(R3,IMM(2)); // fvar table is (id,closure pointer) pairs, so we jump 2 at a time
	JUMP(L_prv_func_lookup_closure_LOOP);
	L_prv_func_lookup_closure_EXIT:
	INCR(R3); //reach the pointer
	MOV(R0,INDD(R2,R3)); //put the pointer in R0
	POP(FP);
	RETURN;

PRV_FUNC_LOOKUP_FVAR: //puts the closure pointer in R0 
	PUSH(FP);
	MOV(FP,SP);
	MOV(R1,FPARG(0));
	MOV(R2,M(fvars));
	MOV(R3,IMM(0));
	L_prv_func_lookup_FVAR_LOOP: //assuming the id exists in table (no stop condition!!)
	CMP(R1,INDD(R2,R3));
	//DEBUG("comparing closures: %ld,%ld\n",R1,INDD(R2,R3));
	JUMP_EQ(L_prv_func_lookup_FVAR_EXIT);
	ADD(R3,IMM(2)); // fvar table is (id,closure pointer) pairs, so we jump 2 at a time
	JUMP(L_prv_func_lookup_FVAR_LOOP);
	L_prv_func_lookup_FVAR_EXIT:
	INCR(R3); //reach the pointer
	ADD(R3,M(fvars));//reach the absolute address
	MOV(R0,R3); //put the address in R0
	POP(FP);
	RETURN;
	
	
PRV_FUNC_PRINT_RESULT:
	PUSH(FP);
	MOV(FP,SP);
	CHECK_ARGC(1);
	MOV(R0,FPARG(2));//save this result for future computations!
	CMP(R0,IMM(SOB_VOID));
	JUMP_EQ(PRV_FUNC_PRINT_RESULT_EXIT);
	PUSH(R0);
	CALL(WRITE_SOB);
	DROP(1);
	CALL(NEWLINE);
	PRV_FUNC_PRINT_RESULT_EXIT:
	MOV(R0,IMM(SOB_VOID));
	POP(FP);
	RETURN;

/*	switch(R0){
		case SOB_NIL:
		printf("NULL\n");
		JUMP(PRV_FUNC_PRINT_RESULT_EXIT);
		break;
		case SOB_VOID:
		printf("void\n");
		JUMP(PRV_FUNC_PRINT_RESULT_EXIT);
		break;
		case SOB_FALSE:
		printf("false\n");
		JUMP(PRV_FUNC_PRINT_RESULT_EXIT);
		break;
		case SOB_TRUE:
		printf("true\n");
		JUMP(PRV_FUNC_PRINT_RESULT_EXIT);
		break;
		default:
		break;
	}
	MOV(R1,IND(R0));
	switch(R1){
		case T_CLOSURE:
		printf("closure: label=%ld\n",INDD(R0,2));
		break;
		case T_FRACTION:
		printf("number: %ld/%ld\n",INDD(R0,1),INDD(R0,2));
		break;
		case T_STRING:
		MOV(R2,INDD(R0,1));
		ADD(R2,IMM(2));
		MOV(R3,IMM(2));
		printf("string: \"");
		while(R3<R2){
			printf("%c",(int)INDD(R0,R3));
			INCR(R3);
		}
		printf("\"\n");
		break;
		case T_CHAR:
		printf("char: %c\n",(int)INDD(R0,1));
		break;
		case T_BOOL:
		printf("boolean: %s\n",INDD(R0,1)? "true":"false");
		break;
		case T_SYMBOL:
		printf("symbol: name=");
		MOV(R1,INDD(R0,1));//move to representing string
		MOV(R2,INDD(R1,1));
		ADD(R2,IMM(2));
		MOV(R3,IMM(2));
		while(R3<R2){
			printf("%c",(int)INDD(R0,R3));
			INCR(R3);
		}
		printf("\n");
		break;
		case T_VECTOR:
		MOV(R2,INDD(R0,1));
		printf("vector: length=%ld, contents: #(",R2);
		ADD(R2,IMM(2));
		MOV(R3,IMM(2));
		while(R3<R2){
			printf(" %ld ",INDD(R0,R3));
			INCR(R3);
		}
		printf(")\n");
		break;
		case T_PAIR:
		printf("list: car=%ld, cdr=%ld\n",INDD(R0,1),INDD(R0,2));
		break;
		default:
		printf("uknown type: R0=%ld, R0[0]=%ld, R0[1]=%ld, R0[2]=%ld\n",R0,INDD(R0,0),INDD(R0,1),INDD(R0,2));
	}
	PRV_FUNC_PRINT_RESULT_EXIT:*/



