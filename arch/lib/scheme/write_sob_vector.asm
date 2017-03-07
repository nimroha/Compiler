/* scheme/write_sob_vector.asm
 * Take a pointer to a Scheme vector object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_SOB_VECTOR:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(IMM('#'));
  CALL(PUTCHAR);
  MOV(R0, FPARG(0));
  PUSH(INDD(R0, 1));//length
  CALL(WRITE_INTEGER);
  PUSH(IMM('('));
  CALL(PUTCHAR);
  DROP(3);
  MOV(R0, FPARG(0));
  MOV(R1, INDD(R0, 1));//length
  CMP(R1, IMM(0));
  JUMP_EQ(L_WSV_EXIT);
  MOV(R2, R0);
  ADD(R2, IMM(2));
  PUSHAD;//save R1-4
  PUSH(IND(R2));
  CALL(WRITE_SOB);
  DROP(1);
  POPAD;//restore R1-4
  INCR(R2);
  DECR(R1);
 L_WSV_LOOP:
  CMP(R1, IMM(0));
  JUMP_EQ(L_WSV_EXIT);
  PUSH(IMM(' '));
  CALL(PUTCHAR);
  DROP(1);
  PUSHAD;//save R1-4
  PUSH(IND(R2));
  CALL(WRITE_SOB);
  DROP(1);
  POPAD;//restore R1-4
  INCR(R2);
  DECR(R1);
  JUMP(L_WSV_LOOP);  
 L_WSV_EXIT:
  PUSH(IMM(')'));
  CALL(PUTCHAR);
  DROP(1);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;

