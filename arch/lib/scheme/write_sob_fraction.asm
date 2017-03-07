/* scheme/write_sob_fraction.asm
 * Take a pointer to a Scheme fraction object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 */

 WRITE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  PUSH(INDD(R0, 2));
  PUSH(INDD(R0, 1));
  CALL(WRITE_INTEGER);
  DROP(1);
  POP(R1);
  CMP(R1,IMM(1));//only an integer
  JUMP_EQ(WRITE_SOB_FRACTION_EXIT);
  PUSH(IMM('/'));
  CALL(PUTCHAR);
  PUSH(R1);
  CALL(WRITE_INTEGER);
  DROP(2);
  WRITE_SOB_FRACTION_EXIT:
  POP(FP);
  RETURN;

