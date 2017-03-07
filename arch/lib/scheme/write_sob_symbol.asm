/* scheme/write_sob_SYMBOL.asm
 * Take a pointer to a Scheme SYMBOL object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 */

 WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(SOB_NIL));//start new frame
  PUSH(FPARG(0));//symbol
  PUSH(IMM(2));//argc
  PUSH(IMM(EMPTY_ENV));//env
  CALL(FUNC_SYMBOL_2_STRING);//get representing string
  DROP(4);
  ADD(R0,IMM(2));//reach start of string
  PUSH(R0);
  CALL(WRITE);//prints null terminated string
  DROP(1);
  POP(FP);
  RETURN;

