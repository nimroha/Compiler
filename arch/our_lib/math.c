 /* math functions */
/*applic appends SOB_NIL to every argument list, so argc is always 1 larger than needed*/

/*+bin*/
FUNC_BIN_ADD:
	PUSH(FP);
	MOV(FP,SP);
	//PRINT_STACK_MSG("BIN_ADD")
	CHECK_ARGC(2);
	CHECK_TYPE(0,FRACTION);
	CHECK_TYPE(1,FRACTION);
	MOV(R1,FPARG(2));//a
	MOV(R2,FPARG(3));//b
	MOV(R3,INDD(R1,2));//den a
	MOV(R4,INDD(R2,2));//den b
	MOV(R5,INDD(R1,1));//num a
	MOV(R6,INDD(R2,1));//num b
	MUL(R5,R4);//num a*den b
	MUL(R6,R3);//num b*den a
	ADD(R5,R6);// new num
	MUL(R3,R4);//new den
	PUSH(R3);
	PUSH(R5);
	CALL(MAKE_SOB_FRACTION);
	DROP(2);
	PUSH(R0);
	//DEBUG("calling PRV_FUNC_SIMPLIFY_FRAC with number=%ld/%ld\n",INDD(R0,1),INDD(R0,2));
	CALL(PRV_FUNC_SIMPLIFY_FRAC);
	//DEBUG(" result=%ld/%ld\n",INDD(R0,1),INDD(R0,2));
	DROP(1);
	POP(FP);
	RETURN;

/**bin*/
FUNC_BIN_MULL:
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1), IMM(3));
	JUMP_NE(Lerror_incorr_num_of_args);
	CHECK_TYPE(0,FRACTION);
	CHECK_TYPE(1,FRACTION);
	MOV(R1,FPARG(2));//a
	MOV(R2,FPARG(3));//b
	MOV(R3,INDD(R1,2));//den a
	MOV(R4,INDD(R2,2));//den b
	MOV(R5,INDD(R1,1));//num a
	MOV(R6,INDD(R2,1));//num b
	MUL(R5,R6);//new num=num a*num b
	MUL(R3,R4);//new den=den a*den b
	PUSH(R3);
	PUSH(R5);
	CALL(MAKE_SOB_FRACTION);
	DROP(2);
	PUSH(R0);
	CALL(PRV_FUNC_SIMPLIFY_FRAC);
	DROP(1);
	POP(FP);
	RETURN;

/*-bin*/
FUNC_BIN_SUB:
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1), IMM(3));
	JUMP_NE(Lerror_incorr_num_of_args);
	CHECK_TYPE(0,FRACTION);
	CHECK_TYPE(1,FRACTION);
	MOV(R1,FPARG(2));//a
	MOV(R2,FPARG(3));//b
	MOV(R3,INDD(R2,1));//num b
	MOV(R4,INDD(R2,2));//den b
	MUL(R3,IMM(-1));//we will be adding the negated b
	PUSH(R4);
	PUSH(R3);
	CALL(MAKE_SOB_FRACTION);
	DROP(2);
	PUSH(R0);// -b
	PUSH(FPARG(2));// a
	PUSH(FPARG(1));// argc
	PUSH(FPARG(0));// env
	CALL(FUNC_BIN_ADD);
	DROP(4);
	POP(FP);
	RETURN;

/*/bin*/
FUNC_BIN_DIV:
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1), IMM(3));
	JUMP_NE(Lerror_incorr_num_of_args);
	CHECK_TYPE(0,FRACTION);
	CHECK_TYPE(1,FRACTION);
	MOV(R1,FPARG(2));//a
	MOV(R2,FPARG(3));//b
	PUSH(INDD(R2,1));//num b
	PUSH(INDD(R2,2));//den b
	CALL(MAKE_SOB_FRACTION);//we will be multiplying the inverted b
	DROP(2);
	PUSH(R0);// 1/b
	PUSH(FPARG(2));// a
	PUSH(FPARG(1));// argc
	PUSH(FPARG(0));// env
	CALL(FUNC_BIN_MULL);
	DROP(4);
	POP(FP);
	RETURN;

/*=bin*/
FUNC_BIN_EQUAL:
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1), IMM(3));
	JUMP_NE(Lerror_incorr_num_of_args);
	CHECK_TYPE(0,FRACTION);
	CHECK_TYPE(1,FRACTION);
	MOV(R1,FPARG(2));//a
	MOV(R2,FPARG(3));//b
	MOV(R3,INDD(R1,2));//den a
	MOV(R4,INDD(R2,2));//den b
	MOV(R5,INDD(R1,1));//num a
	MOV(R6,INDD(R2,1));//num b
	CMP(R5,R6);
	JUMP_NE(FUNC_BIN_EQUAL_FALSE);
	CMP(R3,R4);
	JUMP_NE(FUNC_BIN_EQUAL_FALSE);
	MOV(R0,IMM(SOB_TRUE));
	JUMP(FUNC_BIN_EQUAL_EXIT);
	FUNC_BIN_EQUAL_FALSE:
	MOV(R0,IMM(SOB_FALSE));
	FUNC_BIN_EQUAL_EXIT:
	POP(FP);
	RETURN;

/*<bin*/
FUNC_BIN_LESS_THAN:
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1), IMM(3));
	JUMP_NE(Lerror_incorr_num_of_args);
	CHECK_TYPE(0,FRACTION);
	CHECK_TYPE(1,FRACTION);
	PUSH(FPARG(3));//b
	PUSH(FPARG(2));//a
	PUSH(FPARG(1));//argc
	PUSH(FPARG(0));//env
	CALL(FUNC_BIN_SUB);
	DROP(4);
	MOV(R1,INDD(R0,1));
	CMP(R1,IMM(0));
	JUMP_GE(FUNC_BIN_LESS_THAN_FALSE);
	MOV(R0,IMM(SOB_TRUE));
	JUMP(FUNC_BIN_LESS_THAN_EXIT);
	FUNC_BIN_LESS_THAN_FALSE:
	MOV(R0,IMM(SOB_FALSE));
	FUNC_BIN_LESS_THAN_EXIT:
	POP(FP);
	RETURN;

/*denominator*/
FUNC_DENOMINATOR:
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1), IMM(2));
	JUMP_NE(Lerror_incorr_num_of_args);
	CHECK_TYPE(0,FRACTION);
	CMP(R0,IMM(0));
	JUMP_EQ(Lerror_incorr_type);
	MOV(R1,FPARG(2));
	PUSH(IMM(1));
	PUSH(INDD(R1,2));
	CALL(MAKE_SOB_FRACTION);
	DROP(2);
	POP(FP);
	RETURN;


/*numerator*/
FUNC_NUMERATOR:
	PUSH(FP);
	MOV(FP,SP);
	CMP(FPARG(1), IMM(2));
	JUMP_NE(Lerror_incorr_num_of_args);
	CHECK_TYPE(0,FRACTION);
	CMP(R0,IMM(0));
	JUMP_EQ(Lerror_incorr_type);
	MOV(R1,FPARG(2));
	PUSH(IMM(1));
	PUSH(INDD(R1,1));
	CALL(MAKE_SOB_FRACTION);
	DROP(2);
	POP(FP);
	RETURN;


/*gcd*/
PRV_FUNC_GCD:
	PUSH(FP);
	MOV(FP,SP);
	MOV(R0,FPARG(0));//a
	MOV(R1,FPARG(1));//b
		CMP(R0,IMM(0));//make both arguments positive
		JUMP_GE(PRV_FUNC_GCD_POSITIVE_A);
		MUL(R0,IMM(-1));
		PRV_FUNC_GCD_POSITIVE_A:
		CMP(R1,IMM(0));
		JUMP_GE(PRV_FUNC_GCD_POSITIVE_B);
		MUL(R1,IMM(-1));
		PRV_FUNC_GCD_POSITIVE_B:
	PRV_FUNC_GCD_LOOP:
	CMP(R1,IMM(0));//b=0?
	//DEBUG("a=%ld, b=%ld\n",R0,R1);
	JUMP_EQ(PRV_FUNC_GCD_EXIT);
	MOV(R2,R0);//temp=a
	REM(R2,R1);//r2=a mod(b)
	MOV(R0,R1);//a'=b
	MOV(R1,R2);//b'=amodb
	JUMP(PRV_FUNC_GCD_LOOP);
	PRV_FUNC_GCD_EXIT:
	POP(FP);
	RETURN;



/*simplify-frac*/
PRV_FUNC_SIMPLIFY_FRAC:
	PUSH(FP);
	MOV(FP,SP);
	MOV(R1,FPARG(0));//T_frac
	MOV(R2,INDD(R1,1));//num
	MOV(R3,INDD(R1,2));//den
	//DEBUG("simplify frac:  %ld/%ld\n",R2,R3);
	CMP(R2,IMM(0));
	JUMP_NE(PRV_FUNC_SIMPLIFY_FRAC_NOT_ZERO);
	MOV(INDD(R1,2),IMM(1));//if representing zero => just change denominator to 1
	JUMP(PRV_FUNC_SIMPLIFY_FRAC_EXIT);
	PRV_FUNC_SIMPLIFY_FRAC_NOT_ZERO:
	PUSH(R3);
	PUSH(R2);
	//DEBUG("calling PRV_FUNC_GCD with a=%ld, b=%ld\n",R2,R3);
	CALL(PRV_FUNC_GCD);
	//DEBUG("result=%ld\n",R0);
	DROP(2);
	MOV(R1,FPARG(0));//T_frac
	DIV(INDD(R1,1),R0);
	DIV(INDD(R1,2),R0);
	PRV_FUNC_SIMPLIFY_FRAC_EXIT:
	MOV(R0,R1);
	POP(FP);
	RETURN;


/*fraction constructor
 *expects numerator and denominator on the stack
 */
MAKE_SOB_FRACTION:
	PUSH(FP);
	MOV(FP,SP);
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(R1,FPARG(0));//num
	MOV(R2,FPARG(1));//den
	CMP(R2,IMM(0));
	JUMP_GE(MAKE_SOB_FRACTION_POSITIVE_DEN);
	MUL(R1,IMM(-1));//preserve sign of fraction while denominator is positive
	MUL(R2,IMM(-1));
	MAKE_SOB_FRACTION_POSITIVE_DEN:
	MOV(INDD(R0,0),IMM(T_FRACTION));
	MOV(INDD(R0,1),R1);
	MOV(INDD(R0,2),R2);
	POP(FP);
	RETURN;



	//DEBUG("called FUNC_BIN_ADD: stack: FPARG(-2)=%ld, FPARG(-1)=%ld, FPARG(0)=%ld, FPARG(1)=%ld, FPARG(2)=%ld, FPARG(3)=%ld, FPARG(4)=%ld\n",
		//							FPARG(-2),FPARG(-1),FPARG(0),FPARG(1),FPARG(2),FPARG(3),FPARG(4));