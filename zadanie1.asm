assume ds:data
data segment 
STRLENGTH db 0
LINE 	db 256 dup('$')
LENGTHTABLE db 10 dup('0') 
TOKENQUANTITY dw 0
POS dw 0

TABLE db 153 dup(0)
ASCII db ' ','.','o','+','=','*','B','O','X','@','%','&','#','/','^'
BINARY db 16 dup(?)
db '$'

FRAME2 db'+-----------------+$'
FRAME1	db '+--[ RSA 1024]----+',10,13,'$'
COUTLINE db '|' 
		db 17 dup (' ')
		db '|'
		db 10 ,13 ,'$'
		
ENOARGUMENTS db 'Brak Argumentow $'
ETOMANYARGUMENTS db 'podano zla liczbe argumentow, poprawna ilosc: 2 $'
EARGUMENT1LENGTH	db 'Pierwszy argument ma nie poprawna dlugosc, poprawna dlugosc: 1 $'
EARGUMENT2LENGTH db ' Drugi argument ma niepoprawna dlugosc, poprawna dlugosc : 32 $'
EARGUMENT1 db 'Argument 1 ma niepoprawna skladnie, spodziewano sie cyfry 1 lub 0 $'
EARGUMENT2 db 'Argument 2 ma niepoprawna skladnie, spodziewano sie cyfr 0-9 a-f $'
NEWLINE db 10,13,'$'
data ends

code segment
start:
	mov ax,seg top   	; poprawne zainicjowanie stosu
	mov ss,ax
	mov dx,offset top
	call parseArguments
	call establishBinary
	call moveBishop
	call printSSH
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
	push ds
	pushf
	mov ax,seg NEWLINE
	mov ds,ax
	mov dx,offset NEWLINE
	mov ah,9
	int 21h
	popf
	pop ds
	pop dx
	pop ax
	ret
endl endp

returnConsole proc 		; konczy prace programu
	mov ah,4ch
	int 21h	
	ret
returnConsole endp
;---------------------------                    -------------------------------------;
                             ;Parsing Arguments;
;---------------------------                    -------------------------------------;
parseArguments proc
	push bx
	push cx
	push ax
	push dx
	call getStrLength 		; ilosc liter w CmdLN
	cmp strLength,0			;strlength <=0
	jle noArguments
	
	xor bx,bx
	mov ax,seg STRLENGTH
	mov ds,ax            	; segment dane w DS
	lea di,es:[82h] 		;  parsowanym tekst
	mov si,-1 				; bedzie poruszac sie po tekscie w tablciy
parseLoop:
	call skipWhite 			; pozbadz sie bialych znakow
	call getTokenLength 	; znajdz dlugosc tokena
	call saveToken 			; zapisz go do tablicy
	cmp STRLENGTH,bl		;strlength >bl
	ja parseLoop
	
	call properArguments
	pop dx
	pop ax
	pop cx
	pop bx
	ret
parseArguments endp

skipWhite proc
compare:mov al, es:[di+bx] 		; kolejny znak CmdLn do akumulatora
	cmp al,32 ;spacja
	je next
	cmp al,10 ; nowa lininia
	je next
	cmp al,13 ; powrot karetki
	je next
	cmp al,9 ; tabulacja
	je next
	
	inc si					; jedna spacja miedzy wyrazami
	ret
next:        				; okazal sie bialym znakiem
	inc bx
	jmp compare
skipWhite endp

getTokenLength proc ; sprawdza dlugosc wyrazu zlozonego z nie bialych znakow
xor cx,cx
inc TOKENQUANTITY   		
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
	inc cx
	inc bx
	jmp st2
end2:
	mov bx,TOKENQUANTITY  ; zapisanie dlugosci do LENGTHTABLE
	mov [LENGTHTABLE+bx-1],cl
	pop bx
	ret
getTokenLength endp

saveToken proc		
saveLoop:
	mov al, es:[di+bx]
	mov [LINE+si],al
	inc bx
	inc si
loop saveLoop					
ret
saveToken endp

getStrLength proc		;dlugosc CMDLine
	push ax		
	push bx
	push ds
	mov si,80h 		; wskazanie na ProgramSegmentPrefix
	lodsb 			;wczytuje byte z DS:SI do al
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
properArguments proc ; sprawdza poprawnosc podanych argumentow
	push ax
	push cx
	push bx
	xor ax,ax
	xor bx,bx
	mov cx,32
	cmp TOKENQUANTITY,2
	jne toManyArguments
	
	cmp LENGTHTABLE,1
	jne argument1Length
	
	cmp [LENGTHTABLE+1],32
	jne argument2Length
	
frstArg:	
	mov al,LINE
	cmp ax,'1'
	je scndArg
	cmp ax,'0'
	je scndArg
	jmp argument1
scndArg:
	inc bx
	mov al,ds:[LINE+bx+1]
	cmp ax,'0'
	jb argument2    ;ax<'0'
	cmp ax,'9'		
	jbe isProper	; ax <= '9'
	cmp al,'a' 
	jb argument2	;al<'a'
	cmp al,'f'
	ja argument2   ;al>'f'
isProper:
	loop scndArg
	pop bx
	pop cx
	pop ax
	ret
properArguments endp

noArguments proc
	mov ax,seg ENOARGUMENTS
	mov ds,ax
	mov dx, offset ENOARGUMENTS
	call printStr
	call returnConsole
	ret
noArguments endp

toManyArguments proc
	mov ax,seg ETOMANYARGUMENTS
	mov ds,ax
	mov dx, offset ETOMANYARGUMENTS
	call printStr
	call returnConsole
	ret
toManyArguments endp

argument1Length proc
	mov ax,seg EARGUMENT1LENGTH
	mov ds,ax
	mov dx, offset EARGUMENT1LENGTH
	call printStr
	call returnConsole
	ret
argument1Length endp

argument2Length proc
	mov ax,seg EARGUMENT2LENGTH
	mov ds,ax
	mov dx, offset EARGUMENT2LENGTH
	call printStr
	call returnConsole
	ret
argument2Length endp

argument1 proc
	mov ax,seg EARGUMENT1
	mov ds,ax
	mov dx, offset EARGUMENT1
	call printStr
	call returnConsole
	ret
argument1 endp

argument2 proc
	mov ax,seg EARGUMENT2
	mov ds,ax
	mov dx, offset EARGUMENT2
	call printStr
	call returnConsole
	ret
argument2 endp
;---------------------------                    -------------------------------------;
                             ;from hex to binary;
;---------------------------                    -------------------------------------;
establishBinary proc ; sklada dwie cyfry szesnastkowe w jeden bajt
push cx
push bx

mov cx,16
xor bx,bx			; bx po binarnym
xor si,si			;si po heksalnym
bin:						;16 razy biorac po 2 liczby
	mov al,[LINE+si+2]
	inc si
	sub al,48
	cmp al,10
	jb number 			; skok jezeli nie jest litera
	sub al,39
	
number:
	push cx
	mov cl,4
	sal ax,cl		; przesuniece o 4 w lewo
	pop cx
	
	mov [BINARY+bx],al
	mov al,[LINE+si+2]
	sub al,48
	cmp al,10
	jb number2
	sub al,39
	number2:
	add [BINARY+bx],al
	inc bx
	inc si
	loop bin
	
pop bx
pop cx
ret
establishBinary endp
;---------------------------                    -------------------------------------;
                             ;move bishop;
;---------------------------                    -------------------------------------;
moveBishop proc
	push cx
	push bx
	push ax
	
	xor si,si	
	mov cx,16	; ilosc bajtow
	mov si,76	; poruszanie sie po tablicy
	xor di,di ; poruszanie sie po kodzie binarnym
mainLoop:
push cx
mov cx,4 ; illosc skokow w jednym bajcie
moveLoop:
	xor bx,bx ; sluzy do wyluskiwania bitow
	shr [BINARY+di],1 ; przesuniecie w prawo o jeden bit
	adc bx,0	; do bx jest wrzucony bit ktory wypadl, sumuje bx,0 i cf
	horizontal:
		cmp bx,0
		je left	
		right:
			push cx ; odkladamy cx
			mov ax,si
			mov cl,17	;sprawdzamy podzielnosc zeby nie wypasc za zakres prawy
			div cl
			pop cx
			cmp ah,16
			je optional1
			add si,1
			jmp vertical
		left:
		push cx
			mov ax,si
			mov cl,17	;sprawdzamy podzielnosc zeby nie wypasc za zakres prawy
			div cl
			pop cx
			cmp ah,0
			je optional2
			sub si,1
	vertical:
		xor bx,bx
		shr [BINARY+di],1
		adc bx,0
		cmp bx,0
		je up
		down:
			cmp si,135 ; jezeli przekroczylismy zakres dolny slizgamy sie
			ja optional3
			add si,17
			jmp endloop
		up:
			cmp si,17 ; zakres gorny
			jb optional4
			sub si,17	
	endloop:
		add [TABLE+si],1
	loop moveLoop
pop cx
inc di ; kolejny byte
loop mainLoop
mov POS,si ; ostatni ruch

pop ax
pop bx 
pop cx
ret

optional1:			;wybiera sciezke zaleznie od ustawienia flagi
	cmp LINE,'0'
	je vertical
	jmp left
optional2:
	cmp LINE,'0'
	je vertical
	jmp right
optional3:
	cmp LINE,'0'
	je endloop
	jmp up
optional4:
	cmp LINE,'0'
	je endloop
	jmp down
moveBishop endp
;---------------------------                    -------------------------------------;
                             ;print SSHFINGERPRINT;
;---------------------------                    -------------------------------------;
printSsh proc
push dx
push cx
push bx
push ax

mov ax,seg TABLE
mov ds,ax

mov dx,offset FRAME1
call printStr
mov cx,9

xor ax,ax
xor di,di
xor bx,bx
xor si,si
prtMainLoop:
	push cx 
	mov cx,17 ; zapisanie 17 bajtow
	mov di,1
	prtLoop:
		mov bl,[TABLE+si] ; do bl biore numer odpowiadajacy w pseudo ASCII
		cmp si,76		; jezeli pocztek
		jne last
		mov bl,'S'
		mov [COUTLINE+di],bl
		jmp inclement
		last:				;jezeli ostatni
			;mov ax,POS
			cmp POS,si
			jne over
			mov bl,'E'
			mov [COUTLINE+di],bl
			jmp inclement
		over:			; przekroczyl ilosc 14 nalezy go skrocic
			cmp bx,15
			jb project
			mov bx,14
		project:		; przypisanie kodu pseudo ASCII
		mov al,[ASCII+bx]
		mov [COUTLINE+di],al
		inclement:
			inc si
			inc di
	loop prtLoop
	mov dx,offset COUTLINE
	call printStr
	pop cx
loop prtMainLoop

mov dx,offset FRAME2
call printStr
pop ax
pop bx 
pop cx
pop dx
ret
printSsh endp

code ends
stack1 segment stack
	db 200 dup(?)
top db ? 
stack1 ends
end start