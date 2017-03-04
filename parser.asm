
data segment 
STRLENGTH db 0
LINE 	db 200 dup('$')
TOKENLENGTH db 0 
TOKENQUANTITY db 0
ENOARGUMENTS db 'Brak Argumentow $'
NEWLINE db 10,13,'$'
data ends

code segment
start:
	mov ax,seg top   	; poprawne zainicjowanie stosu
	mov ss,ax
	mov dx,offset top
	
	call parseArguments
	;mov ax,seg LINE
	;mov ds,ax
	;mov dx,offset LINE
	;call printStr
	call returnConsole
	
;---------------------------                    -------------------------------------;
                             ;Useful Procedures;
;---------------------------                    -------------------------------------;
printStr proc 			;procedura wypisuje znaki z DS:DX
	push ax 			;aby nie zmienic flag i wartosci akumulatora zapisujemy je na stosie
	pushf
	mov ah,9
	int 21h
	popf
	pop ax
	ret
printStr endp

endl proc
	push ax
	push dx
	pushf
	mov ax,seg NEWLINE
	mov ds,ax
	mov dx,offset NEWLINE
	mov ah,9
	int 21h
	popf
	pop dx
	pop ax
	ret
endl endp

returnConsole proc 		; konczy prace programu
	mov ah,4ch
	int 21h	
returnConsole endp

;---------------------------                    -------------------------------------;
                             ;Parsing Arguments;
;---------------------------                    -------------------------------------;

parseArguments proc
	call getStrLength 		; ilosc liter w CmdLN
	cmp strLength,0
	jle noArguments
	
	mov bx,0000
	mov ax,seg STRLENGTH
	mov ds,ax            	; segment dane w DS
	lea di,es:[82h] 		; bedzie poruszal sie po parsowanym tekscie
	mov si,-1 				; bedzie poruszac sie po tekscie w tablciy
parseLoop:
	call skipWhite 			; pozbadz sie bialych znakow
	call getTokenLength 	; znajdz dlugosc tokena
	call saveToken 			; zapisz go do tablicy
	cmp STRLENGTH,bl
	ja parseLoop
	
	ret
parseArguments endp

skipWhite proc
st1:mov al, es:[di+bx] 		; kolejny znak CmdLn do akumulatora
	cmp al,32
	je next
	cmp al,10
	je next
	cmp al,13
	je next
	cmp al,9
	je next
	inc si					; jedna spacja miedzy wyrazami
	ret
next:        				; okazal sie bialym znakiem
	inc bx
	jmp st1
skipWhite endp

getTokenLength proc
mov TOKENLENGTH,0
inc TOKENQUANTITY   		;ToManyArguments TODO
push bx
st2:mov al, es:[di+bx]
	cmp al,32
	je end2
	cmp al,10
	je end2
	cmp al,13
	je end2
	cmp al,9
	je end2
	inc TOKENLENGTH
	inc bx
	jmp st2
end2:pop bx
	ret
getTokenLength endp

saveToken proc
mov cl,TOKENLENGTH		;zapisanie dlugosci tokena
mov dx,offset LINE		;---wypisanie tokena ---
add dx,si
push dx					;----               ----
saveLoop:
	mov al, es:[di+bx]
	mov [LINE+si],al
	inc bx
	inc si
loop saveLoop
pop dx 					;------             -------
call printStr
call endl				;--------           --------
ret
saveToken endp

getStrLength proc		;dlugosc CMDLine
	push ax		
	push bx
	push ds
	mov si,80h 		; wskazanie na ProgramSegmentPrefix
	lodsb 			;wczytuje byte z DS:SI
	mov bl,al
	dec bl 			; zmniejszam o 1 gdyz bede zaczynal od znaku a nie od spacji
	mov ax,seg STRLENGTH
	mov ds ,ax
	mov STRLENGTH,bl
	pop es 			; nastepuje zamiana i teraz CmdLine jest w es a nie w ds
	pop bx 
	pop ax
	ret
getStrLength endp

;-------------------------                     --------------------------------------;
								;Errors;
;-------------------------                     --------------------------------------;
noArguments proc
	mov ax,seg ENOARGUMENTS
	mov ds,ax
	mov dx, offset ENOARGUMENTS
	call printStr
	call returnConsole
noArguments endp


code ends
stack1 segment stack
	db 200 dup(?)
top db ? 
stack1 ends

end start