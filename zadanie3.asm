assume ds:data
.387 ; dla fpu
data segment 
STRLENGTH db 0
LINE 	db 256 dup('$')
LENGTHTABLE db 10 dup('0')
TOKENLENGTH db 0 
TOKENQUANTITY dw 0

ITERNUM db 0 , '$'
LEN db 0 , '$'
LSYSLEN dw 0

TEMP dw 0
THREE dw 3
KAT dt 0
X dt 0
Y dt 0 ;dt = 10 bajtow 8*10 bitow=80
KOLOR db 7
X1 dw 60d
Y1 dw 150d

LSYSTEM db 7200 dup('$')
TSYSTEM db 'F++F++F', 7200 dup('$')
SUBSTITUDE  db 'F-F++F-F'

ENOARGUMENTS db 'Brak Argumentow $'
TOOMANYARGUMENTS db 'Podano zla liczbe argumentow $'
TOBIGITERNUM db 'podane iternum jest zbyt duze prosze podac bardziej realna liczbe $'
NUMERROR db 'spodziewano sie cyfry otrzymano cos innego $'
NEWLINE db '-',10,13,'$'
TOBIGLEN db 'LEN jest zbyt duze dla kazdej wartosci iternum wypadnie poza ekran $'
ARG db 'Argumenty maja zla wielkosc pierwszy argument maksymalnie jedna cyfra drugi maksymalnie 2 cyfry $'
data ends

code1 segment
start:
	mov ax,seg top   	; poprawne zainicjowanie stosu
	mov ss,ax
	mov sp,offset top
	call parseArguments
	call setSystem
	
	finit
	mov ah,00
	mov al ,13h
	int 10h
	call Kocha
	
	xor ax,ax						;oczekiwanie na znak ax,00h
	int 16h
	mov ax,3						;wyjscie z trybu graficznego.
	int 10h
	
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
	
	cmp ds:[TOKENQUANTITY],2 ; czy podano dwa argumenty
	jne tokenq
	
	cmp ds:[LENGTHTABLE],1 ; pierwszy argument nie powinien byc dluzszy niz 1
	jne lengthofarg
	
	cmp ds:[LENGTHTABLE+1],2 ; drugi argument nie powinien byc dluzszy niz 2
	ja lengthofarg
	
	mov al,ds:[LINE] ; wrzucenie iternum do zmiennej 
	sub al,48d
	cmp al,9 ; czy jest to liczba
	ja numErr
	mov ds:[ITERNUM],al
	
	cmp al,5
	ja tokenl ; iternum powinno byc <=5
	
	mov al,ds:[LINE] ; stworzenie LSYSTEMU
	call establishLength
	
	cmp ds:[LEN],60 ; LEN nie powinno przkeraczac 60
	ja lennumber
	
	pop dx
	pop ax
ret
lengthofarg:
	mov dx,offset ARG
	call ErrorMessage
tokenq:
	mov dx,offset TOOMANYARGUMENTS
	call ErrorMessage
lennumber:
	mov dx,offset TOBIGLEN
	call ErrorMessage
tokenl:
	mov dx,offset TOBIGITERNUM 
	call ErrorMessage
numERR:
	mov dx,offset NUMERROR
	call ErrorMessage
properArguments endp

noArguments proc
	mov ax,seg ENOARGUMENTS
	mov ds,ax
	mov dx, offset ENOARGUMENTS
	call ErrorMessage
	call returnConsole
noArguments endp

ErrorMessage proc
	mov ax,seg data
	mov ds,ax
	call printStr
	call returnConsole
ErrorMessage endp
;-------------------------                     --------------------------------------;
								;establishLength;
;-------------------------                     --------------------------------------;
establishLength proc
push ax
push bx
push cx
push dx
push si
xor cx,cx
	mov ax,seg data
	mov ds,ax
	mov cl,[LENGTHTABLE+1]	; ile razy bedziemy przechodzic bo kazdej "literce"
	mov bx,1			;mnoznik
	mov si,offset LINE; tekst ktory bedzie zwijany do liczby
	inc si	;pomijamy pierwszy argument
	add si,cx	;bedziemy zaczynali od ostatniej cyfry zmniejszajac si az do najbardcziej znaczacej
	xor ax,ax
getnumber:
	mov al,ds:[si]	;wrzucamy literke do al
	sub al,48	; usuwamy '0'
	cmp al,9	; sprawdzamy czy na pewno jest cyfra
	ja numErr ; nie jest
	mul bl	; mnozymy ax=al*bl
	add ds:[LEN],al	; dodajemy do sumy al
	xor ax,ax ; zerujemy ax
	mov al,bl ; teraz bedziemy chcieli zwiekszyc mnoznik *10
	mov bl,10d
	mul bl
	mov bl,al
	xor ax,ax	; zerujemy ax przed kolejnym wywolanie
	dec si ; przechodzimy do bardziej znaczacej cyfry
loop getnumber
pop si
pop dx 
pop cx
pop bx
pop ax
ret
numERR:
	mov dx,offset NUMERROR
	call ErrorMessage
establishLength endp
;-------------------------                     --------------------------------------;
								;setSystem;
;-------------------------                     --------------------------------------;
setSystem proc
push ax
push bx
push cx
push dx
push si
push di
	xor cx,cx ; zerowanie
	xor dx,dx
	mov ax,seg data
	mov es,ax
	mov ax,seg data
	mov ds,ax
	mov cl,ds:[ITERNUM] ; w cl ile razy mamy zrobic iteracje po calosci
	mov ax,7		; w ax trzymamy dlugosc przetwarzanego tekstu
lsysloop:
	push cx		
	mov di,offset LSYSTEM ; bedziemy uzywa movsb wiec ds:[si] - skad es:[di] dokad
	mov bx,offset TSYSTEM ; bx bedzie poruszal sie po starym tekscie
	mov cx,ax	; ile mamy do przetworzenia aktualnie tekstu
	iterate:
		mov dx,es:[bx] ; w dx literka 
		cmp dl,'F' 
		jne notF
		mov si,offset SUBSTITUDE ; 8 literowy ciag kopiowany operacja lancuchowa
		push cx
		mov cx,8 ; bo 8 liter
		rep movsb
		pop cx
		inc bx ; przejscie do kolejnej literki
		add ax,7 ; dodalismy 7 liter
		jmp next
		notF:
		mov ds:[di],dx ;; normalne skopiwoanie czegosc oc nie jest literka F
		inc di
		inc bx
	next:
	loop iterate
	call cpySystem ; przygotowanie TSYSTEM do kolejnej iteracji
	pop cx
loop lsysloop
mov LSYSLEN,ax
pop di
pop si
pop dx 
pop cx
pop bx
pop ax
ret
setSystem endp
;-------------------------                     --------------------------------------;
								;cpySystem;
;-------------------------                     --------------------------------------;
cpySystem proc ; kopiuje tekst s LSYSTEM do TSYSTEM
push ax
push cx
push si
push di
	mov cx,ax ; kopiuje ax razy czuyli dlugosc ciagu znakow z ds:si do es:di
	mov ax, seg data
	mov ds ,ax
	mov ax , seg data
	mov es ,ax
	mov di,offset TSYSTEM
	mov si,offset LSYSTEM
	rep movsb
pop di
pop si
pop cx
pop ax
ret
cpySystem endp
;---------------------------                    -------------------------------------;
                             ;KOCHA;
;---------------------------                    -------------------------------------;
Kocha proc
push ax
push cx
push si
	mov ax,seg data
	mov ds,ax
	FLDPI ;zaladuj pi na stos
	FIDIV THREE ; podziel st(0) przez 3 st(0)= pi/3 
	FLD KAT
	FLD Y
	FIADD ds:[Y1]
	FLD X ; aktualnie na stosie X,Y,KAT,pi/3
	FIADD ds:[X1]
	mov cx,LSYSLEN
	mov si,offset LSYSTEM
drawing:
	mov ah,ds:[si]
	cmp ah,'-'
	je addKAT
	cmp ah,'+'
	je subKAT
	jmp Fcase
addKAT:
	FXCH st(2) ; kat na wierzcholek stosu
	FADD st(0),st(3) ; dodaj do wierzcholka sotsu pi/3
	FXCH st(2) ; zamien do poprzedniego ustawienia
	jmp nextiter
subKAT:
	FXCH st(2) ; kat na wierzcholek stosu
	FSUB st(0),st(3) ; odejmij od wierzcholka sotsu pi/3
	FXCH st(2) ; zamien do poprzedniego ustawienia
	jmp nextiter
Fcase:	
	call print
nextiter:
	inc si
loop drawing
pop si
pop cx
pop ax
ret
Kocha endp
;---------------------------                    -------------------------------------;
                             ;print;
;---------------------------                    -------------------------------------;
print proc ; najpierw bedziemy sprawdzac czy mozemy rysowac, rysowac, przygotowujemy kolejne rysowanko
push ax
push cx
push di ; ulozenie na stosie : x,y,KAT,pi/3
mov ax,seg data
mov ds,ax
xor cx,cx
FLD st(2) ; kopia KAT na wierzcholek gdyz kolejna funkcja sciagnie go ze stosu
FSINCOS  ;ulozenie na stosie : cos,sin,x,y,KAT,pi/3
mov cl,ds:[LEN]
prt:	;bedziemy chcieli narysowac LEN pikseli a w spolrzedne uzykamy w nastepujacy sposob:;y += sin(KAT) , a x += cos(KAT)
	mov ax,0A000h ; poczatek obszaru pamieci graficznej 320x200
	mov es,ax
	FXCH st(3) ; zamienia tak ze na wierzcholku mamy y
	FIST word ptr TEMP ; y leci do zmiennej temp
	FXCH st(3) ; powrot do normalnosci
	
	cmp TEMP,199d ; jesli wyszlo poza zakres dolny nie rysuj !
	ja outofbounds
	; przepis na narysowanie piksela 320 * Y + X 
	mov ax,ds:[TEMP]
	mov di,320
	mul di ; 320 * Y 
	FXCH st(2) ;   wyciagamy X
	FIST word ptr TEMP ; ladujemy go do zmiennej temp
	FXCH st(2)
	
	cmp TEMP, 319d ; przekroczenie zakresu prawego
	ja outofbounds
	
	add ax,ds:[TEMP] ; dodajemy X
	mov di,ax ; wyliczony adres do di
	mov al,ds:[KOLOR] ; kolor narysowanego piksela domyslnie bialy
	mov byte ptr es:[di],al
outofbounds: ; przygotowanie kolejnej zmiennej
	FXCH st(3) ; y na wierzch cos na miejsce 3
	FADD st(0),st(1) ; y += sin(KAT)
	FXCH st(3)
	
	FXCH st(2);x na wierzch cos na miejsce 2
	FADD st(0),st(2) ;x += cos(KAT)
	FXCH st(2)
loop prt
	
	FISTP word ptr ds:[TEMP] ; usuwanie sinusa i cosinusa poprzez zrzucenie ich do zmiennej temp 
	FISTP word ptr ds:[TEMP]
	
pop di
pop cx
pop ax
ret
print endp

code1 ends
stack1 segment stack
	db 200 dup(?)
top db ? 
stack1 ends

end start