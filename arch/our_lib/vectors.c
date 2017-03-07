 /* vector functions */
/*applic appends SOB_NIL to every argument list, so argc is always 1 larger than needed*/


/*vector
 *args: a list of elements
 */
FUNC_VECTOR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R1,FPARG(1));
  DECR(R1);//ignore the SOB_NIL as last argument
  MOV(R10,R1);//R10=n
  ADD(R1,IMM(2));
  PUSH(R1);
  CALL(MALLOC);
  DROP(1);
  MOV(INDD(R0,0),IMM(T_VECTOR));
  MOV(INDD(R0,1),R10);
  MOV(R2,IMM(2));
  FUNC_VECTOR_LOOP:
  CMP(R2,R1);
  JUMP_EQ(FUNC_VECTOR_EXIT);
  MOV(INDD(R0,R2),FPARG(R2));
  INCR(R2);
  JUMP(FUNC_VECTOR_LOOP);
  FUNC_VECTOR_EXIT:
  POP(FP);
  RETURN;


/*vector-length*/
FUNC_VECTOR_LENGTH:
  PUSH(FP);
  MOV(FP, SP);
  CMP(FPARG(1), IMM(2));
  JUMP_NE(Lerror_incorr_num_of_args);
  CHECK_TYPE(0,VECTOR);
  MOV(R1,FPARG(2));
  PUSH(IMM(1));
  PUSH(INDD(R1,1));
  CALL(MAKE_SOB_FRACTION);
  DROP(2);
  POP(FP);
  RETURN;


/*vector-set!
 *args: vector int object
 */
FUNC_VECTOR_SET:
	PUSH(FP);
	MOV(FP, SP);
	CHECK_ARGC(3);
	CHECK_TYPE(0,VECTOR);
	CHECK_TYPE(1,FRACTION);
	MOV(R2,FPARG(3));
	CMP(INDD(R2,2),IMM(1));//check if integer
	JUMP_NE(Lerror_incorr_type);
	MOV(R1,FPARG(2));
	MOV(R2,INDD(R2,1));//n
	ADD(R2,2);
	MOV(INDD(R1,R2),FPARG(4));
	MOV(R0,R1);
	//CALL(PRV_FUNC_PRINT_RESULT);
	MOV(R0,IMM(SOB_VOID));
	POP(FP);
	RETURN;


/*vector-ref
 *args: vector int
 */
FUNC_VECTOR_REF:
	PUSH(FP);
	MOV(FP, SP);
	CMP(FPARG(1), IMM(3));
	JUMP_NE(Lerror_incorr_num_of_args);
	CHECK_TYPE(0,VECTOR);
	CHECK_TYPE(1,FRACTION);
	MOV(R2,FPARG(3));
	CMP(INDD(R2,2),IMM(1));
	JUMP_NE(Lerror_incorr_type);
	MOV(R1, FPARG(2));
	MOV(R2, INDD(R2,1));
	ADD(R2, IMM(2));
	MOV(R0,INDD(R1,R2));
	POP(FP);
	RETURN;



