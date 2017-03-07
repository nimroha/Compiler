


POP(FP);
RETURN;

JUMP(STOP_MACHINE_LABEL)

L_error_lambda_args_count:
	POP(R15);
	POP(R14);
	fprintf(stderr,"TODO error message for: L_error_lambda_args_count. file: %s line: %ld\n",(char *)R14,R15);
	HALT

Lerror_incorr_num_of_args:
	MOV(R1,FPARG(1));
	POP(R15);
	POP(R14);
	fprintf(stderr,"Error: Arity mismatch, given %ld arguments (including SOB_NIL). file: %s line: %ld\n",R1,(char *)R14,R15);
	HALT

Lerror_incorr_type:
	POP(R15);
	POP(R14);
	fprintf(stderr,"Error: Given incorrect type. file: %s line: %ld\n",(char *)R14,R15);
	HALT

L_error_cannot_apply_non_clos:
	POP(R15);
	POP(R14);
	fprintf(stderr,"Error: Cannot apply non closure. file: %s line: %ld\n",(char *)R14,R15);
	HALT

TODO_label:
	//POP(R15);
	//POP(R14);
	fprintf(stderr,"Error: function not implemented. file: %s line: %ld\n",(char *)R14,R15);
	RETURN;

STOP_MACHINE_LABEL:
  STOP_MACHINE;

  return 0;
}


