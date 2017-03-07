/* pair functions */
/*applic appends SOB_NIL to every argument list, so argc is always 1 larger than needed*/

/*car*/
FUNC_CAR:
	PUSH(FP);
	MOV(FP,SP);
	CHECK_ARGC(1);
	CHECK_TYPE(0,PAIR);
	MOV(R1, FPARG(2));
	CMP(INDD(R1, 0), IMM(T_PAIR));
	JUMP_NE(Lerror_incorr_type);
	MOV(R0,INDD(R1, 1));
	POP(FP);
	RETURN;


/*cdr*/
FUNC_CDR:
	PUSH(FP);
	MOV(FP, SP);
	//PRINT_STACK_MSG("CDR");
	CHECK_ARGC(1);
	CHECK_TYPE(0,PAIR);
	MOV(R1,FPARG(2));
	CMP(INDD(R1, 0), IMM(T_PAIR));
	JUMP_NE(Lerror_incorr_type);
	MOV(R0,INDD(R1, 2));
	POP(FP);
	RETURN;


/*cons*/
FUNC_CONS:
	PUSH(FP);
	MOV(FP, SP);
	CHECK_ARGC(2);
	MOV(R1, FPARG(2));
	MOV(R2, FPARG(3));
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R0, 0), IMM(T_PAIR));
	MOV(INDD(R0, 1), R1);
	MOV(INDD(R0, 2), R2);
	POP(FP);
	RETURN;


/*set-car!*/
FUNC_SET_CAR:
	PUSH(FP);
	MOV(FP, SP);
	CHECK_ARGC(2);
	PUSH(FPARG(2));
	CALL(IS_SOB_PAIR);
	DROP(1);
	CMP(R0,IMM(0));
	JUMP_EQ(Lerror_incorr_type);
	MOV(R1, FPARG(2));
	MOV(R2, FPARG(3));
	MOV(INDD(R1,1),R2);
	MOV(R0,SOB_VOID);
	POP(FP);
	RETURN;

/*set-cdr!*/
FUNC_SET_CDR:
	PUSH(FP);
	MOV(FP, SP);
	CHECK_ARGC(2);
	PUSH(FPARG(2));
	CALL(IS_SOB_PAIR);
	DROP(1);
	CMP(R0,IMM(0));
	JUMP_EQ(Lerror_incorr_type);
	MOV(R1, FPARG(2));
	MOV(R2, FPARG(3));
	MOV(INDD(R1,2),R2);
	MOV(R0,SOB_VOID);
	POP(FP);
	RETURN;

PRV_FUNC_LIST_LENGTH:
	PUSH(FP);
	MOV(FP,SP);
	MOV(R0,FPARG(0));
	MOV(R12,IMM(0));
 	PRV_FUNC_LIST_LENGTH_LOOP:
 	CMP(R0,IMM(SOB_NIL));//reached the end of list
 	JUMP_EQ(PRV_FUNC_LIST_LENGTH_EXIT);
 	INCR(R12);
 	CMP(INDD(R0,0),IMM(T_PAIR));
 	JUMP_NE(Lerror_incorr_type);
 	PUSH(R0);
 	PUSH(IMM(2));//argc
 	PUSH(IMM(EMPTY_ENV));//env
 	CALL(FUNC_CDR);//doesn't use R12
 	DROP(3);
 	JUMP(PRV_FUNC_LIST_LENGTH_LOOP);
 	PRV_FUNC_LIST_LENGTH_EXIT:
 	MOV(R0,R12);
 	POP(FP);
 	RETURN;