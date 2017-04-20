assume ds:data
data segment 
KOD db 100 dup('$')
KODLENGTH db ?
VERSION db 0
INPUT db 20 dup(0) 
OUTPUT db 20 dup(0)
BUFFER db 16384 dup(?)
POSITION db 0

READ dw 0
IHANDLER dw 0
OHANDLER dw 0

STRLENGTH db 0
LINE 	db 256 dup('$')
LENGTHTABLE db 10 dup('0')
TOKENLENGTH db 0 
TOKENQUANTITY dw 0
napis db 'lol $'
WRITEERROR db ' nastapil blad przy zapisywaniu lancucha znakow do pliku $'
CLOSEERROR db 'nastapil blad przy zamykaniu pliku $'
READERROR db 'nastapil blad przy wczytywaniu lancucha tekstu $'
OPERROR db 'problem podczas otwierania pliku $'
ENOARGUMENTS db 'Brak Argumentow $'
TOOMANYARGUMENTS db 'Podano zla liczbe argumentow $'
MAKEFILEERRORR db ' nastapil blad przy tworzeniu nowego pliku $'
NEWLINE db '-',10,13,'$'
data ends

code1 segment
start:
	mov ax,seg top   	; poprawne zainicjowanie stosu
	mov ss,ax
	mov dx,offset top
	call parseArguments
	call establishParameters
	call encipherFile
	
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
	lea di,es:[82h] 		; bedzie poruszal sie po parsowanym tekscie
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
properArguments proc ;sprawdzam wersje i przy okazji ilosc slow w comandline 
	push ax
	push dx
	mov ax,seg data
	mov ds,ax
	cmp ds:[LengthTable],2 ; sprawdzam wersje
	jne	version0
	cmp ds:[LINE],'-'
	jne version0
	cmp ds:[LINE+1],'d'
	jne version0
	mov ax,1
	mov ds:[Version],al
	cmp ds:[TOKENQUANTITY],4 ; 4 wyrazy z opcja -d
	je proper
	jmp tooManyArg
version0: 
	cmp ds:[TOKENQUANTITY],3 ; 3 wyrazy bez opcji -d
	je	proper
tooManyArg:	
	mov dx,offset TOOMANYARGUMENTS
	call ErrorMessage
proper:	
	pop dx
	pop ax
	ret
properArguments endp

noArguments proc
	mov ax,seg ENOARGUMENTS
	mov ds,ax
	mov dx, offset ENOARGUMENTS
	call ErrorMessage
noArguments endp

ErrorMessage proc
	mov ax,seg data
	mov ds,ax
	call printStr
	call returnConsole
ErrorMessage endp

;-------------------------                     --------------------------------------;
								;EstablishParameters;
;-------------------------                     --------------------------------------;

establishParameters proc
	push ax
	push cx
	push bx
	push dx
	push si
	
	xor cx,cx
	xor bx,bx
	mov si,offset LINE ; w ds:si linia komend
	mov ax,seg data
	mov ds,ax
	
	mov ax,seg INPUT ; es:di miejsce w ktorym bedziemy zapisywac dane
	mov es,ax
	mov di,offset INPUT
	
	cmp ds:[VERSION],1
	jne saveInput
	add si,3; -d$ przesuwam zeby miec pierwsza literke
	inc bx
saveInput: ; zapisuje nazwe pliku wejsciowego
	mov cl,ds:[LENGTHTABLE+bx]
	rep movsb; z ds:si do es:di
	inc bx
saveOutput: ; zapisuje nazwe pliku wyjsciowego
	inc si
	mov di,offset OUTPUT
	mov cl,ds:[LENGTHTABLE+bx]
	rep movsb ; z ds:si do es:di
	inc bx
saveKod: ; zapisuje kod i jego dlugosc
	inc si
	mov di,offset KOD
	mov cl,ds:[LENGTHTABLE+bx]
	mov ds:[KODLENGTH],cl
	rep movsb; z ds:si do es:di
	
	pop si
	pop dx
	pop bx
	pop cx
	pop ax
	ret	
establishParameters endp
;-------------------------                     --------------------------------------;
								;Vigenere;
;-------------------------                     --------------------------------------;

encipher proc
push ax
push dx
push si
xor dx,dx
	mov al,ds:[BUFFER+bx] ; wczytanie liczby do zaszyfrowania/odszyfrowania
	mov dl,ds:[POSITION]
	cmp dl,ds:[KODLENGTH] ; sprawdzam czy nie przekroczylem zakres klucza
	jne whichVersion
	xor dx,dx		;przekroczylem
	mov ds:[POSITION],dl
whichVersion:
	inc ds:[POSITION]; przygotwanie do kolejnego odczytu
	mov si,dx
	mov dl,ds:[KOD+si]	;w dl mamy literke szyfrujaca
	cmp ds:[VERSION],1
	je decipher
cipher:
	add al,dl ;; dodajemy gdyz al ma tylko 255 nie wymaga to operacji mod
	jmp write
decipher:	
	sub al,dl ; tak samo przy odejmowaniu gdyz al przechowuje tylko dodatnie wartosci
write:
	mov ds:[BUFFER+bx],al ;przepisanie do buffera
	inc bx
pop si
pop dx
pop ax
	ret
encipher endp
;-------------------------                     --------------------------------------;
								;FILE OPERATIONS;
;-------------------------                     --------------------------------------;
openFiles proc
push ax
push bx
push cx
push dx
	mov ax,seg data
	mov ds,ax
	mov dx,offset INPUT ; nazwa pliku 
	mov ah,3dh ;otwarcie pliku
	mov al,0 ; tryb otwarcia do odczytu
	int 21h
	jc openError ; jesli problem zwrocony prze int21h
	mov bx,ax ; uchwyt pliku do bx
	mov ds:[IHANDLER],bx
	
	mov dx,offset OUTPUT ; nazwa pliku 
	mov al,1 ; tryb otwarcia do zapisu
	mov ah,3dh ;otwarcie pliku
	int 21h
	jc openError ; jesli problem zwrocony prze int21h
	mov bx,ax ; uchwyt pliku do bx
	mov ds:[OHANDLER],bx
pop dx
pop cx
pop bx
pop ax	
	ret
openError:
mov dx,offset OPERROR
call errorMessage
openFiles endp


closeFiles proc
push bx
push ax
	mov bx,ds:[IHANDLER] ; handler do zamykanego pliku
	mov ah,3Eh
	int 21h
	jc closeingError
	
	mov bx,ds:[OHANDLER] ; handler do zamykanego pliku
	int 21h
	jc closeingError	
	
pop ax
pop bx
	ret
closeingError:
mov dx,offset CLOSEERROR
call errorMessage
closeFiles endp

fillBuffer proc
push ax
push bx
push cx
push dx

	mov bx,ds:[IHANDLER] ;handler do pliku
	mov cx,16384 ; ile znakow
	mov dx,offset BUFFER ; gdzie
	mov ah,3FH ; czytanie
	int 21h
	jc readingError ;jezeli blade zwrocony przez int 21 h
	mov ds:[READ],ax ; ile znakow rzeczywiscie przeczytal
	
pop dx
pop cx
pop bx
pop ax
ret
readingError:
mov dx,offset READERROR
call errorMessage
fillBuffer endp

copyBuffer proc ; do cx wrzucamy ilosc liter ktore chcemy zapisac
push ax
push bx
push dx
	mov ax,seg data
	mov ds,ax
	mov dx,offset BUFFER ; gdzie kopiowac
	mov bx,ds:[OHANDLER] ; handler do pliku
	mov ah,40h 				; zapis
	int 21h
	jc writeingError
pop dx
pop bx
pop ax
ret
writeingError:
mov dx,offset WRITEERROR
call errorMessage
copyBuffer endp

makeFile proc
	push ax
	push cx
	
	mov ax ,seg data
	mov ds ,ax
	mov dx,offset OUTPUT ; nazwa pliku do stworzenia
	mov ah,3Ch
	mov cl,0 ; atrybuty pliku do odczytu
	int 21h
	jc makeFileError
	
	pop cx
	pop ax
	ret
makeFileError:
mov dx,offset MAKEFILEERRORR
call errorMessage
makeFile endp

;-------------------------                     --------------------------------------;
								;encipherFile
;-------------------------                     --------------------------------------;

encipherFile proc
push bx
push cx
	call makeFile ; tworze plik jesli istnieje zostanie usuniety i stworzony nowy
	call openFiles ;; otwieram plik input w trybie do odczytu output w trybie zapisu
mainLoop:
	xor bx,bx
	call fillBuffer ; wpisuje ciag do buffera
	xor cx,cx
	mov cx,ds:[READ]	; ile znakow wczytalo
	encpiherLoop:
		call encipher	;szyfrowanie-odszyfrowywanie
		loop encpiherLoop
	mov cx,ds:[READ]	; do cx ilosc znakow wczytanych
	call copyBuffer	; przez cs zostalo tu wyslane ile znakow bufora ma byc przepisanych do pliku
	cmp ds:[READ],16384 ; jesli buffer byl pelny wczytuje znow jesli nie to koncze prace
	je mainLoop
	
	call closeFiles ;zamykanie plikow
pop cx
pop bx
ret
encipherFile endp


code1 ends
stack1 segment stack
	db 200 dup(?)
top db ? 
stack1 ends

end start