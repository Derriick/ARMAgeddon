	ldi r1, 42
	subi r0, r1, 17
	jeq r0, r1, j1
	jlt r0, r1, j1
	jle r0, r1, j1
	cmpi r1, 42
	add r2, r0, r1, 1
	jeq r2, r1, end
j1:	sub r1, r0, r0, 2
	cmp r0, r0, 0
end:	jmp end
