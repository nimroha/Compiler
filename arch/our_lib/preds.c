/* predicate functions */
/*applic appends SOB_NIL to every argument list, so argc is always 1 larger than needed*/

#define IS_TYPE_PRED(type)	FUNC_##type##_Q: \
								PUSH(FP); \
								MOV(FP,SP); \
								CMP(FPARG(1),IMM(2)); \
								JUMP_NE(Lerror_incorr_num_of_args); \
								PUSH(FPARG(2)); \
								CALL(IS_SOB_##type); \
								DROP(1); \
								CMP(R0,IMM(1)); \
								JUMP_EQ(FUNC_##type##_Q_TRUE); \
								MOV(R0,SOB_FALSE); \
								JUMP(FUNC_##type##_Q_EXIT); \
								FUNC_##type##_Q_TRUE: \
								MOV(R0,SOB_TRUE); \
								FUNC_##type##_Q_EXIT: \
								POP(FP); \
								RETURN;

/*rational?*/
IS_TYPE_PRED(FRACTION)

/*symbol?*/
IS_TYPE_PRED(SYMBOL)

/*vector?*/
IS_TYPE_PRED(VECTOR)

/*char?*/
IS_TYPE_PRED(CHAR)

/*null?*/
IS_TYPE_PRED(NIL)

/*pair?*/
IS_TYPE_PRED(PAIR)

/*string?*/
IS_TYPE_PRED(STRING)

/*procedure?*/
IS_TYPE_PRED(CLOSURE)

/*boolean?*/
IS_TYPE_PRED(BOOL)

/* private predicates */
IS_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_FRACTION);
  JUMP_EQ(L_IS_SOB_FRACTION_TRUE);
  MOV(R0, IMM(0));
  JUMP(L_IS_SOB_FRACTION_EXIT);
 L_IS_SOB_FRACTION_TRUE:
  MOV(R0, IMM(1));
 L_IS_SOB_FRACTION_EXIT:
  POP(FP);
  RETURN;

IS_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_SYMBOL);
  JUMP_EQ(L_IS_SOB_SYMBOL_TRUE);
  MOV(R0, IMM(0));
  JUMP(L_IS_SOB_SYMBOL_EXIT);
 L_IS_SOB_SYMBOL_TRUE:
  MOV(R0, IMM(1));
 L_IS_SOB_SYMBOL_EXIT:
  POP(FP);
  RETURN;

IS_SOB_STRING:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_STRING);
  JUMP_EQ(L_IS_SOB_STRING_TRUE);
  MOV(R0, IMM(0));
  JUMP(L_IS_SOB_STRING_EXIT);
 L_IS_SOB_STRING_TRUE:
  MOV(R0, IMM(1));
 L_IS_SOB_STRING_EXIT:
  POP(FP);
  RETURN;
  
IS_SOB_VECTOR:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_VECTOR);
  JUMP_EQ(L_IS_SOB_VECTOR_TRUE);
  MOV(R0, IMM(0));
  JUMP(L_IS_SOB_VECTOR_EXIT);
 L_IS_SOB_VECTOR_TRUE:
  MOV(R0, IMM(1));
 L_IS_SOB_VECTOR_EXIT:
  POP(FP);
  RETURN;

IS_SOB_NIL:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(R0,IMM(SOB_NIL));
  JUMP_EQ(L_IS_SOB_NIL_TRUE);
  MOV(R0, IMM(0));
  JUMP(L_IS_SOB_NIL_EXIT);
  L_IS_SOB_NIL_TRUE:
  MOV(R0, IMM(1));
  L_IS_SOB_NIL_EXIT:
  POP(FP);
  RETURN;
