	ldi r1, 42
	subi r0, r1, 17
	cmp r0, r1
	cmp r1, r0, 1
	add r2, r0, r1, 0
	sub r1, r0, r0, 2
end:	jmp end
