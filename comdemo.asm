 ;=============================================================================
 ; DOS .COM demo program
 ;
 ; By Dennis Katsonis, July 2020
 ; Written for DOS.  Compile with FASM.
 ;
 ; This is a basic example of a DOS .COM file.
 ; This uses the PC speaker for sound, direct writing to the video buffer
 ; and input.
 ; This does nothing more than print some user input in dazzling colour.
 ; and other stuff.
 ;
 ; Should run on a 8086 processor or higher.
 ; Thanks to The Revolutionary Guide to Assembly Language for the sound routine.
 ;
 ; Version 1.0
 ;=============================================================================


	org	100h			; code starts at offset 100h
	use16

	inputlen = 40

	mov	ah,09h
	mov	dx,intro
	int	21h      ; Print introduction message.
	mov	cx,10
prod:
	mov	bx,cx
	shl	bx,1      
	mov	di,[ds:bx+freq]  ; Move frequency to play to register di
	mov	bx,25            ; For duration of 25
	call	sound            ; Play tone
	loop	prod
		      
	mov	ah,09h
	mov	dx,namereq
	int	21h              ; Request name

	mov	cx,inputlen ; Input name
	mov	ax,3f00h    ; Input call
	xor	bx,bx       ; From keyboard (stdin)
	mov	dx,keybuff  ; Save at keybuff
	int	21h
	jc	endp
	sub	al,2  ; We won't include the CR/LF as part of the message.
	add	byte [messagelen], al

	mov	ah,02h	; Hide the cursor
	xor     bx,bx
	mov	dx,01900h  ; Row 25, column 0 in hex is where we put it, so it doesn't appear
	int	10h

	mov	ah,0fh    ; Get current video mode
	int	10h
	cmp	al,7      ; Check of CGA
	jne	.vga
	mov	ax,0b000h ; And set ES to this if CGA
	jmp	.vga2
.vga:
	mov	ax,0b800h ; Otherwise set to 0xb800
.vga2:
	mov	es,ax     ; ES now is the screen buffer segment address
	xor	di,di
	mov	cx,2000
	mov	ax,0357h
	rep	stosw ; Fill screen with cyan 'W's.  My younger daughter likes the letter "W".
	xor	di,di
	mov	ah,10h
	int	16h   ; Wait for keypress

	call	erasescreen

	mov	si,message ; Message is a set of ASCII characters, then followed by the user input
	mov	cx,2000
	xor	bx,bx
	xor	bp,bp
.writemessage2:
	mov	dl,[ds:si+bp] ; For each space on the screen, enter the next ASCII character
	mov	[es:bx],dl
	inc	bp            ; Point to the next ascii character to print
	add	bx,2          ; and add 2 to BX.  Each character space on the screen takes two bytes,
	      	      	      ; one for the ascii character, and one for the attributes.  We are just printing the
			      ; characters now, so we loop with a stride of 2.
	inc	al
	cmp	bp,[messagelen] ; If we have reached the end of the message, wrap back to the start.
	jl	.return2
	xor	bp,bp
.return2:
	dec	cx
	jnz	.writemessage2
	mov	al,1
.startloop:
	and     al,0fh 		; We only need the next number between 1 and 15.  We get this by a bit mask which only
	      	      	      	; returns the last 4 bits.
	jnz	.skip           ; If we haven't gone back to 0 skip
	or	al,1            ; Otherwise, set back to one.
.skip:
	mov	cx,2000
	mov	dl,al           ; Move the colour to register al
	mov	bx,1
.loop42:
	mov	byte [es:di+bx],dl  ; and write to video memory.
	inc	dl
	and     dl,0fh   	; Use the last 4 bits to get a number between 1 and 15
	jnz     .return        	; If zero
	or	dl,1            ; Make it one, as we don't want to use black.
.return:
	add	bx,2
	loop	.loop42
	inc	al
;	call	waitretrace
	call	delay
	push	ax
	mov	ah,0bh	; Set border color.  BL already contains colour
	mov	bl,dl
	xor	bh,bh
	int	10h
	mov	ah,01h
	int	16h  ; Check if we pressed a key
	pop	ax
	jz	.startloop  ; and go again, if no key.

	xor	ax,ax
	int	16h
	mov	ah,0bh	; Reset border to black
	xor	bx,bx
	int	10h

	call	erasescreen

	mov	ah,02h ;Set cursor
	xor	bx,bx
	xor	dx,dx
	int	10h

	mov	ah,09h
	mov	dx,bye
	int	21h
endp:
	mov	ax,4c00h	 ; Send exit code to dos
	int	21h		; Send command to DOS

;waitretrace:
;	push	dx
;	push	ax
;.retrace:
;	mov	dx,03dah
;	in	al,dx
;	test	al,08h
;	jnz	.retrace
;.retrace2:			;
;	in	al,dx
;	test	al,08h
;	jz	.retrace2
;	pop	ax
;	pop	dx
;	ret

delay:
	push	ax
	push	cx
	push	dx
	mov	ah,86h
	mov	cx,1
	mov	dx,0ffffh
	int	15h
	pop	dx
	pop	cx
	pop	ax
	ret

sound:
  ; Frequency in DI register
  ; Duration in BX register (hundredth of a second)
	push	ax
	push	cx
	push	dx
	push	ds
	push	es
	push	si

	in	al,61h  ; Read current port mode B (8255)
	mov	cl,al   ; Save the mode
	or	al,3    ; Enable speaker and timer.
	out	61h,al
	mov	al,0b6h  ; Set channel 2, which is the one connected to the PC speaker.
	out	43h,al  ; Command register 8253
	mov	dx,14h
	mov	ax,4f38h  ; Frequency divisor.
	div	di
	out	42h,al  ; Lower frequency byte.
	mov	al,ah
	out	42h,al  ; Higher frequency byte.

	mov	ax,91
	mul	bx    ; AX = BX * 91
	mov	bx,500 ; Divisor
	div	bx ; Resut is now in AX with remainder in DX.
	mov	bx,ax  ; Save the result.
	xor	ah,ah  ; Read time.
	int	1ah
	add	dx,bx
	mov	bx,dx
.cycle:
	int	1ah  ; As time elapsed?
	cmp	dx,bx
	jne	.cycle  
	in	al,61h  ; Read mode of port B (8255)
	mov	al,cl ; Previous mode
	and	al,0fch
	out	61h,al  ; Restore mode.
exits:
	pop	si
	pop	es
	pop	ds
	pop	dx
	pop	cx
	pop	ax
	ret


erasescreen:
	push	di ; Bottom row to erase
	push	si ; Top row to erase
	push	ax
	push	bx
	push	cx

	xor	ax,ax ; Row in al
	xor	si,si
	mov	cx,1000
	mov	di,4000-2
@@:
	mov	word [es:si],0720h
	mov	word [es:di],0720h
	add	si,2
	inc	ax

	sub	di,2
	cmp	ax,80
	jl	.skipit
	call	pauseerase
.skipit:
	loop	@b
	mov	di,0
   
	pop	cx
	pop	bx
	pop	ax
	pop	si
	pop	di
	ret
	
pauseerase:
	push	cx
	push	bx
	xor	ax,ax
	mov	cl,4
	mov	bx,30
	push	di
	shr	di,cl
	call	sound
	pop	di
	pop	bx
	pop	cx
	ret
  
	freq	dw  0,988,880,784,699,659,587,523,880,988,784
	bye	db  "Goodbye!",10,13,"By Dennis Katsonis, July 2020",10,13,24h
	intro	db  "Colourful name printing demo",13,10,24h
	namereq db  "What is your name? ",24h
	messagelen dw 7  ; Initial size of message
align 4
	message db 32,1,32,3,32,14,32
	keybuff rb inputlen  ; This must be immediately after message
