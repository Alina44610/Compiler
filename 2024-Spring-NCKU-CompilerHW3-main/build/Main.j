.source Main.j
.class public Main
.super java/lang/Object

.method public static calculate_pi(I)F
.limit stack 100
.limit locals 100
	ldc 3.000000 ;
	fstore 1
	ldc 2.000000 ;
	fstore 2
	ldc 1 ;
	istore 3
	ldc 1 ;
	istore 4
f_Alpha_0:
	iload 4
	iload 0
	if_icmplt Loop_0_0
	iconst_0
	goto Loop_1_0
Loop_0_0:
	iconst_1
Loop_1_0:
ifeq f_Delta_0
	goto f_Gamma_0
f_beta_0:
	iload 4
	ldc 1
	iadd
	istore 4
	goto f_Alpha_0
f_Gamma_0:
	ldc 4 ;
	i2f
	fload 2
	iload 4
	i2f
	fmul ;
	fload 2
	iload 4
	i2f
	fmul ;
	ldc 1.000000 ;
	fadd ;
	fmul ;
	fload 2
	iload 4
	i2f
	fmul ;
	fload 2
	fadd ;
	fmul ;
	fdiv ;
	fstore 5
	iload 3
	ifeq If_alpha_1
	fload 1
	fload 5
	fadd
	fstore 1
	ldc 0 ;
	istore 3
	goto If_belta_1
If_alpha_1:
	fload 1
	fload 5
	fsub
	fstore 1
	ldc 1 ;
	istore 3
If_belta_1:
goto f_beta_0
f_Delta_0:
	fload 1
	freturn
.end method

.method public static main([Ljava/lang/String;)V
.limit stack 100
.limit locals 100
	ldc 100 ;
	istore 1
	iload 1
	invokestatic Main/calculate_pi(I)F
	fstore 2

	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "Approximation of Pi after " ;
	invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V


	getstatic java/lang/System/out Ljava/io/PrintStream;
	iload 1
	invokevirtual java/io/PrintStream/print(I)V


	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc " terms: " ;
	invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V


	getstatic java/lang/System/out Ljava/io/PrintStream;
	fload 2
	invokevirtual java/io/PrintStream/print(F)V


	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "\n"
	invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V

	ldc 0 ;
return
.end method
