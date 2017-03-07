 /* string functions */
/*applic appends SOB_NIL to every argument list, so argc is always 1 larger than needed*/



/*length
 * args: string
 */
FUNC_STRLEN:
  PUSH(FP);
  MOV(FP,SP);
  //PRINT_STACK_MSG("strlen")
  CHECK_ARGC(1);
  CHECK_TYPE(0,STRING);
  MOV(R1,FPARG(2));
  PUSH(IMM(1));//denominator
  PUSH(INDD(R1,1));//numerator
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  POP(FP);
  RETURN;


/*string-set!
 *args: string int char
 */
FUNC_STRING_SET:
	PUSH(FP);
	MOV(FP, SP);
    CHECK_ARGC(3);
	CHECK_TYPE(0,STRING);
	CHECK_TYPE(1,FRACTION);
	MOV(R2,FPARG(3));
	CMP(INDD(R2,2),IMM(1)); //check if integer
	JUMP_NE(Lerror_incorr_type);
	CHECK_TYPE(2,CHAR);
	MOV(R3,FPARG(4));
	MOV(R3,INDD(R3,1));
	MOV(R1,FPARG(2));//string
	MOV(R2,FPARG(3));
	MOV(R2,INDD(R2,1));//index
	ADD(R2,IMM(2));
	MOV(INDD(R1,R2),R3);//str[r2]=r3
	MOV(R0,SOB_VOID);
	POP(FP);
	RETURN;


/*string-ref
 *args: string int
 */
FUNC_STRING_REF:
	PUSH(FP);
	MOV(FP, SP);
	CMP(FPARG(1), IMM(3));
	JUMP_NE(Lerror_incorr_num_of_args);
	CHECK_TYPE(0,STRING);
	CHECK_TYPE(1,FRACTION);
	MOV(R2,FPARG(3));
	CMP(INDD(R2,2),IMM(1)); //check if integer
	JUMP_NE(Lerror_incorr_type);
	MOV(R1,FPARG(2));
	MOV(R2,INDD(R2,1));
	ADD(R2,IMM(2));
	MOV(R0,INDD(R1,R2));
	PUSH(R0);
	CALL(MAKE_SOB_CHAR);
	DROP(1);
	POP(FP);
	RETURN;



/*make-string-c
 *args: int char
 */
FUNC_MAKE_STRING_C:
	PUSH(FP);
	MOV(FP, SP);
	CHECK_ARGC(2);
	CHECK_TYPE(0,FRACTION);
	CHECK_TYPE(1,CHAR);
	MOV(R1,FPARG(2));
	CMP(INDD(R1,2),IMM(1));//check if integer
	JUMP_NE(Lerror_incorr_type);
	MOV(R5,FPARG(3));//char
	MOV(R5,INDD(R5,1));//ascii value
	MOV(R1,INDD(R1,1));//n
	MOV(R2,R1);
	ADD(R2,IMM(3));//add space for type,length,null
	PUSH(R2);
	CALL(MALLOC);//conserves all registers
	DROP(1);
	DECR(R2);
	MOV(INDD(R0,0),IMM(T_STRING));
	MOV(INDD(R0,1),R1);
	MOV(INDD(R0,R2),IMM(0));//null terminator
		FUNC_MAKE_STRING_C_LOOP:
		DECR(R2);
		CMP(R2,IMM(1));
		JUMP_EQ(FUNC_MAKE_STRING_C_EXIT);
		MOV(INDD(R0,R2),R5);
		JUMP(FUNC_MAKE_STRING_C_LOOP);
	FUNC_MAKE_STRING_C_EXIT:
	POP(FP);
	RETURN;

/*symbol->string*/
FUNC_SYMBOL_2_STRING:
	PUSH(FP);
	MOV(FP, SP);
	CMP(FPARG(1), IMM(2));
	JUMP_NE(Lerror_incorr_num_of_args);
	CHECK_TYPE(0,SYMBOL);
	MOV(R1,FPARG(2));
	MOV(R0,INDD(R1,1)); //a symbol holds a pointer to its representative string
	POP(FP);
	RETURN;


/*input: string
 *output: symbol (creates new one if no entry found)
 */
FUNC_STRING_TO_SYMBOL: //symbol entry is array of pointers |symbol|string|next|
	PUSH(FP);
	MOV(FP,SP);
	CHECK_ARGC(1);
	CHECK_TYPE(0,STRING);
	MOV(R1,FPARG(2));
	MOV(R2,M(symbols));
	FUNC_STRING_TO_SYMBOL_LOOP:
	CMP(INDD(R2,1),R1);//strings match
	JUMP_EQ(FUNC_STRING_TO_SYMBOL_FOUND);
	PUSHAD;
	PUSH(R1);
	PUSH(INDD(R2,1));
	CALL(PRV_FUNC_STR_CMP);
	DROP(2);
	POPAD;
	JUMP_EQ(FUNC_STRING_TO_SYMBOL_FOUND);
	CMP(INDD(R2,1),IMM(0));//end of list
	JUMP_EQ(FUNC_STRING_TO_SYMBOL_NOT_FOUND);
	MOV(R2,INDD(R2,2));//next element
	JUMP(FUNC_STRING_TO_SYMBOL_LOOP);
	FUNC_STRING_TO_SYMBOL_FOUND:
	MOV(R0,INDD(R2,0));
	JUMP(FUNC_STRING_TO_SYMBOL_EXIT);
	FUNC_STRING_TO_SYMBOL_NOT_FOUND:
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R0,2),M(symbols));//add new element to head of list
	MOV(M(symbols),R0);//save head
	MOV(INDD(R0,1),R1);//put string
	MOV(R4,R0);
	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);
	MOV(INDD(R0,0),IMM(T_SYMBOL));//create new symbol
	MOV(INDD(R0,1),R1);
	MOV(INDD(R4,0),R0);//put the new symbol in the entry
	FUNC_STRING_TO_SYMBOL_EXIT:
	POP(FP);
	RETURN;
	
	
PRV_FUNC_STR_CMP:
	PUSH(FP);
	MOV(FP,SP);
	MOV(R1,FPARG(0));
	MOV(R2,FPARG(1));
	MOV(R3,IMM(1));
	CMP(INDD(R1,R3),INDD(R2,R3));//compare lengths
	JUMP_NE(PRV_FUNC_STR_CMP_FALSE);
	PRV_FUNC_STR_CMP_LOOP:
	INCR(R3);
	CMP(INDD(R1,R3),INDD(R2,R3));//compare chars
	JUMP_NE(PRV_FUNC_STR_CMP_FALSE);	
	CMP(INDD(R1,R3),IMM(0));
	JUMP_NE(PRV_FUNC_STR_CMP_LOOP);
	MOV(R0,IMM(SOB_TRUE));
	JUMP(PRV_FUNC_STR_CMP_EXIT);
	PRV_FUNC_STR_CMP_FALSE:
	MOV(R0,IMM(SOB_VOID));
	PRV_FUNC_STR_CMP_EXIT:
	POP(FP);
	RETURN;
	
