
.class public collatz.collatz
.super java/lang/Object

.method public static write(I)V 
    .limit locals 1 
    .limit stack 2 
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    iload 0
    invokevirtual java/io/PrintStream/println(I)V   
    return 
.end method

.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

; COMPILED CODE STARTS   


	ldc 100
	istore 0
	ldc 2
	istore 1
Startwhile_17:
	iload 1
	iload 0
	if_icmpge Comp_f_19
	ldc 1
	goto Comp_end_20
Comp_f_19:
	ldc 0
Comp_end_20:
	ifeq Endwhile_18
	ldc 2
	istore 2
	ldc 0
	istore 3
Startwhile_5:
	iload 2
	iload 1
	ldc 2
	idiv
	ldc 1
	iadd
	if_icmpge Comp_f_11
	ldc 1
	goto Comp_end_12
Comp_f_11:
	ldc 0
Comp_end_12:
	ifeq Conj_false_7
	iload 3
	ldc 0
	if_icmpne Comp_f_9
	ldc 1
	goto Comp_end_10
Comp_f_9:
	ldc 0
Comp_end_10:
	goto Conj_end_8
Conj_false_7:
	iconst_0
Conj_end_8:
	ifeq Endwhile_6
	iload 1
	iload 2
	idiv
	iload 2
	imul
	iload 1
	if_icmpne Comp_f_3
	ldc 1
	goto Comp_end_4
Comp_f_3:
	ldc 0
Comp_end_4:
	ifeq Ifelse_1
	ldc 1
	istore 3
	goto Endif_2
Ifelse_1:
Endif_2:
	iload 2
	ldc 1
	iadd
	istore 2
	goto Startwhile_5
Endwhile_6:
	iload 3
	ldc 0
	if_icmpne Comp_f_15
	ldc 1
	goto Comp_end_16
Comp_f_15:
	ldc 0
Comp_end_16:
	ifeq Ifelse_13
	iload 1
	invokestatic collatz/collatz/write(I)V
	goto Endif_14
Ifelse_13:
Endif_14:
	iload 1
	ldc 1
	iadd
	istore 1
	goto Startwhile_17
Endwhile_18:

	return
.end method