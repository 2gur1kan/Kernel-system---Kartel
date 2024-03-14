     name "Kartel"
; this is a very basic example
; of a tiny operating system.
;
; this is kernel module!
;
; it is assumed that this machine
; code is loaded by 'micro-os_loader.asm'
; from floppy drive from:
;   cylinder: 0
;   sector: 2
;   head: 0


;=================================================
; how to test micro-operating system:
;   1. compile micro-os_loader.asm
;   2. compile micro-os_kernel.asm
;   3. compile writebin.asm
;   4. insert empty floppy disk to drive a:
;   5. from command prompt type:
;        writebin loader.bin
;        writebin kernel.bin /k
;=================================================

; directive to create bin file:
#make_bin#

; where to load? (for emulator. all these values are saved into .binf file)
#load_segment=0800#
#load_offset=0000#

; these values are set to registers on load, actually only ds, es, cs, ip, ss, sp are
; important. these values are used for the emulator to emulate real microprocessor state 
; after micro-os_loader transfers control to this kernel (as expected).
#al=0b#
#ah=00#
#bh=00#
#bl=00#
#ch=00#
#cl=02#
#dh=00#
#dl=00#
#ds=0800#
#es=0800#
#si=7c02#
#di=0000#
#bp=0000#
#cs=0800#
#ip=0000#
#ss=07c0#
#sp=03fe#



; this macro prints a char in al and advances
; the current cursor position:
putc    macro   char
        push    ax
        mov     al, char
        mov     ah, 0eh
        int     10h     
        pop     ax
endm


; sets current cursor position:
gotoxy  macro   col, row
        push    ax
        push    bx
        push    dx
        mov     ah, 02h
        mov     dh, row
        mov     dl, col
        mov     bh, 0
        int     10h
        pop     dx
        pop     bx
        pop     ax
endm


print macro x, y, attrib, sdat
LOCAL   s_dcl, skip_dcl, s_dcl_end
    pusha
    mov dx, cs
    mov es, dx
    mov ah, 13h
    mov al, 1
    mov bh, 0
    mov bl, attrib
    mov cx, offset s_dcl_end - offset s_dcl
    mov dl, x
    mov dh, y
    mov bp, offset s_dcl
    int 10h

    popa
    jmp skip_dcl
    s_dcl DB sdat
    s_dcl_end DB 0
    skip_dcl:    
endm



; kernel is loaded at 0800:0000 by micro-os_loader
org 0000h
 
; skip the data and function delaration section:
jmp start 
; The first byte of this jump instruction is 0E9h
; It is used by to determine if we had a sucessful launch or not.
; The loader prints out an error message if kernel not found.
; The kernel prints out "F" if it is written to sector 1 instead of sector 2.
           



;==== data section ===================== 
starttime db "0000"

; welcome message:
msg  db "KARTEL ",152,159,"letim sistemine Ho",159,"geldin", 0 

cmd_size        equ 10    ; size of command_buffer
command_buffer  db cmd_size dup("b")
clean_str       db cmd_size dup(" "), 0
prompt          db ">", 0

; commands:
chelp    db "help", 0
chelp_tail:
ccls     db "cls", 0
ccls_tail:
cquit    db "quit", 0
cquit_tail:
cexit    db "exit", 0
cexit_tail:
creboot  db "reboot", 0
creboot_tail:
             
csaat  db "saat", 0
csaat_tail:

cuptime  db "uptime", 0
cuptime_tail:            

cwhoami  db "whoami", 0
cwhoami_tail: 

cversion  db "version", 0
cversion_tail:

ccreators  db "creators", 0
ccreators_tail:

csnake  db "snake", 0
csnake_tail:  

crun  db "run", 0
crun_tail: 
           
help_msg db "KARTEL progrm",141,"n",141," tercih etti",167,"iniz i",135,"in te",159,159,"ek",154,"rler", 0Dh,0Ah
         db "Komutlar",141,"n Listesi", 0Dh,0Ah
         db "help    - Komut listesini yazd",141,"r",141,"r.", 0Dh,0Ah
         db "cls     - Ekran",141,"temizler", 0Dh,0Ah
         db "reboot  - Yeniden ba",159,"lat",141,"r.", 0Dh,0Ah
         db "quit    - Reboot ile ayn",141,"i",159,"levde.", 0Dh,0Ah 
         db "exit    - Quitin ayn",141,"s",141,".", 0Dh,0Ah
         db "saat    - A",135,141,"k kalma zaman",141,"n",141," g",148,"sterir", 0Dh,0Ah 
         db "uptime  - Zaman",141," g",148,"sterir", 0Dh,0Ah
         db "whoami  - Sistemin ad",141,"n",141," s",153,"yler.", 0Dh,0Ah
         db "creators- Haz",141,"rlayanlar.", 0Dh,0Ah
         db "snake   - Y",141,"lan oyunu", 0Dh,0Ah 
         db "run     - Ka",135,141,159," oyunu", 0Dh,0Ah
         db "Fazlas",141," gelecektir!", 0Dh,0Ah, 0  
         
quit_msg db " l",154,"tfen t",154,"m disketleri Ã§",135,141,"kar",141,"n "
         db " ve yeniden ba",159,"latmak i",135,"in herhangi bir tu",159,"a bas",141,"n... "

whoami_msg db "Kullan",141,"c",141,0Dh,0Ah, 0
version_msg db "KARTEL i",159,"letim sistemi s",129,"r",129,"m 1.0",0Dh,0Ah, 0
creators_msg db "haz",141,"rlayanlar:", 0Dh,0Ah
             db "G",154,"rkan G",154,"r", 0Dh,0Ah
             db "Muhammed Yasin Temur", 0Dh,0Ah
             db "Musa Mert ",153,"z", 0Dh,0Ah, 0 
             
snake_msg 	db "==== how to play ====", 0dh,0ah
	db "press any key to start...$", 0dh,0ah,0 

unknown  db "bilinmeyen komut: " , 0    

;======================================

start:

; set data segment:
push    cs
pop     ds

; set default video mode 80x25:
mov     ah, 00h
mov     al, 03h
int     10h

; blinking disabled for compatibility with dos/bios,
; emulator and windows prompt never blink.
mov     ax, 1003h
mov     bx, 0      ; disable blinking.
int     10h

 ;---------------------------------- 
    
    mov ah,00h
    int 1ah  
    
    mov ax,dx
    mov dx,cx
    mov bx,1092
    div bx
    
    xor dx,dx
    mov bx,60
    div bx
          
    mov ch,al
    mov cl,dl
    
    mov si,0
    
    mov starttime[si],cl
    inc si
    
    mov starttime[si],ch 
    
    ;---------------------------------------



; *** the integrity check  ***
cmp [0000], 0E9h
jz integrity_check_ok
integrity_failed:  
mov     al, 'F'
mov     ah, 0eh
int     10h  
; wait for any key...
mov     ax, 0
int     16h
; reboot...
mov     ax, 0040h
mov     ds, ax
mov     w.[0072h], 0000h
jmp	0ffffh:0000h	 
integrity_check_ok:
nop
; *** ok ***
              


; clear screen:
call    clear_screen
                     
                       
; print out the message:
lea     si, msg
call    print_string


eternal_loop:
call    get_command

call    process_cmd

; make eternal loop:
jmp eternal_loop


;===========================================
get_command proc near

; set cursor position to bottom
; of the screen:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]

gotoxy  0, al

; clear command line:
lea     si, clean_str
call    print_string

gotoxy  0, al

; show prompt:
lea     si, prompt 
call    print_string


; wait for a command:
mov     dx, cmd_size    ; buffer size.
lea     di, command_buffer
call    get_string


ret
get_command endp
;===========================================

process_cmd proc    near

;//// check commands here ///
; set es to ds
push    ds
pop     es

cld     ; forward compare.

; compare command buffer with 'help'
lea     si, command_buffer
mov     cx, chelp_tail - offset chelp   ; size of ['help',0] string.
lea     di, chelp
repe    cmpsb
je      help_command

; compare command buffer with 'saat'
lea     si, command_buffer
mov     cx, csaat_tail - offset csaat   ; size of ['saat',0] string.
lea     di, csaat
repe    cmpsb
je      saat_command 

; compare command buffer with 'uptime'
lea     si, command_buffer
mov     cx, cuptime_tail - offset cuptime   ; size of ['saat',0] string.
lea     di, cuptime
repe    cmpsb
je      uptime_command


; compare command buffer with 'whoami'
lea     si, command_buffer
mov     cx, cwhoami_tail - offset cwhoami  ; size of ['whoami',0] string.
lea     di, cwhoami
repe    cmpsb
je      whoami_command


; compare command buffer with 'version'
lea     si, command_buffer
mov     cx, cversion_tail - offset cversion  ; size of ['version',0] string.
lea     di, cversion
repe    cmpsb
je      version_command 

; compare command buffer with 'creators'
lea     si, command_buffer
mov     cx, ccreators_tail - offset ccreators  ; size of ['whoami',0] string.
lea     di, ccreators
repe    cmpsb
je      creators_command


; compare command buffer with 'snake'
lea     si, command_buffer
mov     cx, csnake_tail - offset csnake  ; size of ['snake',0] string.
lea     di, csnake
repe    cmpsb
je      snake_command 

; compare command buffer with 'run'
lea     si, command_buffer
mov     cx, crun_tail - offset crun  ; size of ['run',0] string.
lea     di, crun
repe    cmpsb
je      run_command

; compare command buffer with 'cls'
lea     si, command_buffer
mov     cx, ccls_tail - offset ccls  ; size of ['cls',0] string.
lea     di, ccls
repe    cmpsb
jne     not_cls
jmp     cls_command
not_cls:

; compare command buffer with 'quit'
lea     si, command_buffer
mov     cx, cquit_tail - offset cquit ; size of ['quit',0] string.
lea     di, cquit
repe    cmpsb
je      reboot_command

; compare command buffer with 'exit'
lea     si, command_buffer
mov     cx, cexit_tail - offset cexit ; size of ['exit',0] string.
lea     di, cexit
repe    cmpsb
je      reboot_command

; compare command buffer with 'reboot'
lea     si, command_buffer
mov     cx, creboot_tail - offset creboot  ; size of ['reboot',0] string.
lea     di, creboot
repe    cmpsb
je      reboot_command

; ignore empty lines
cmp     command_buffer, 0
jz      processed


;////////////////////////////

; if gets here, then command is
; unknown...

mov     al, 1
call    scroll_t_area

; set cursor position just
; above prompt line:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]
dec     al
gotoxy  0, al

lea     si, unknown
call    print_string

lea     si, command_buffer
call    print_string

mov     al, 1
call    scroll_t_area

jmp     processed

; +++++ 'help' command ++++++
help_command:

; scroll text area 9 lines up:
mov     al, 15
call    scroll_t_area

; set cursor position 9 lines
; above prompt line:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]
sub     al, 15
gotoxy  0, al

lea     si, help_msg
call    print_string

mov     al, 1
call    scroll_t_area

jmp     processed 


; +++++ 'version' command ++++++
version_command:

; scroll text area 1 lines up:
mov     al, 1
call    scroll_t_area

; set cursor position 1 lines
; above prompt line:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]
sub     al, 1
gotoxy  0, al

lea     si, version_msg
call    print_string

mov     al, 1
call    scroll_t_area

jmp     processed 

; +++++ 'whoami' command ++++++
whoami_command:

; scroll text area 1 lines up:
mov     al, 1
call    scroll_t_area

; set cursor position 1 lines
; above prompt line:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]
sub     al, 1
gotoxy  0, al

lea     si, whoami_msg
call    print_string

mov     al, 1
call    scroll_t_area

jmp     processed


; +++++ 'creators' command ++++++
creators_command:

; scroll text area 1 lines up:
mov     al, 7
call    scroll_t_area

; set cursor position 1 lines
; above prompt line:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]
sub     al, 7
gotoxy  0, al

lea     si, creators_msg
call    print_string

mov     al, 1
call    scroll_t_area

jmp     processed

;++++++++++++saat Command++++++++++++

saat_command: 

   
   mov     al, 1
   call    scroll_t_area 
    
    ; above prompt line:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]
sub     al, 1
gotoxy  0, al
  
	;mov ds, ax
                          
    ;mov ah, 00H
    ;mov al, 03H                   
    ;int 10H                        
    mov al, ch 
      
    mov ah,00h
    int 1ah  
     
    ;bolme
    
    mov ax,dx
    mov dx,cx
    mov bx,1092
    div bx
    
    xor dx,dx
    mov bx,60
    div bx
          
    mov ch,al
    mov cl,dl     
          
    ;yazdirma
    
    ;push ax                                           
     
    mov bl, 10
    div bl 
    mov cx, ax
    
    mov al, cl
    add al, 48                 
    mov ah, 0eh
    int 10h 
    
    mov al, ch
    add al, 48 
    mov ah, 0eh
    int 10h
    
    ;iki nokta
    mov al, ':'       
    mov ah, 0eh
    int 10h
    
    ;dakika
    
    mov ax,dx
    mov bl, 10
    div bl 
    mov cx, ax
       
    add al, 48                 
    mov ah, 0eh
    int 10h   
       
    mov al, ch
    add al, 48 
    mov ah, 0eh
    int 10h

    ;bi tusa basildiginda gec
    ;mov ah, 0 
    ;int 16h
    
    mov     al, 1
    call    scroll_t_area

    jmp     processed
    ;ret   
        
   ;SAAT ENDP 

;++++++++++++uptime command+++++++++++++++++
                                
uptime_command:
              
    mov     al, 1
   call    scroll_t_area 
    
    ; above prompt line:
    mov     ax, 40h
    mov     es, ax
    mov     al, es:[84h]
    sub     al, 1
    gotoxy  0, al 
    
    xor ax,ax         
              
        mov ah,00h
    int 1ah  
    
    mov ax,dx
    mov dx,cx
    mov bx,1092
    div bx
    
    xor dx,dx
    mov bx,60
    div bx
          
    mov ch,al
    mov cl,dl 
    
    mov si,0
    mov al,starttime[si]
    sub cl,al
    
    inc si
    
    mov al, starttime[si]
    sub ch,al
              
    ;yazirma
    
    mov al, ch
    
    mov bl, 10
    div bl 
    
    mov al, ch
    add al, 48                 
    mov ah, 0eh
    int 10h 
    
    mov al, ch
    add al, 48 
    mov ah, 0eh
    int 10h
    
    ;iki nokta 
    
    mov al, ':'       
    mov ah, 0eh
    int 10h
    
    ;dakika
     
    xor ax,ax 
    mov al,cl
    mov bl, 10
    div bl 
       
    add al, 48                 
    mov ah, 0eh
    int 10h   
       
    mov al, cl
    add al, 48 
    mov ah, 0eh
    int 10h   
   
    mov     al, 1
    call    scroll_t_area

    jmp     processed
    
;++++++++++snake Command ++++++++++

snake_command:

jmp     start2

s_size  equ     7

snake dw s_size dup(0)

tail    dw      ?

left    equ     4bh
right   equ     4dh
up      equ     48h
down    equ     50h

cur_dir db      right

wait_time dw    0


start2:

print 10,7,1100_1111b," Yilan Oyununa hos geldiniz       "
print 10,9,1100_1111b," Baslamak icin enter'a bas        "
print 10,11,1100_1111b," kapatmak icin esc                "
print 10,13,1100_1111b," oyun sadece ilk acilista calisir "                                           
                                                           
mov ah, 00h
int 16h

mov     ah, 1
mov     ch, 2bh
mov     cl, 0bh
int     10h           


game_loop:

;kuyruk fonksiyonu Ã§alismadigi icin elden gecirdik---------- 

mov     dx, snake[12]

mov     ah, 02h
int     10h

mov     al, '*'
mov     ah, 09h
mov     bl, 0eh 
mov     cx, 1  
int     10h          
           
mov     dx, snake[10]

mov     ah, 02h
int     10h

mov     al, '*'
mov     ah, 09h
mov     bl, 0eh 
mov     cx, 1  
int     10h 

mov     dx, snake[8]

mov     ah, 02h
int     10h

mov     al, '*'
mov     ah, 09h
mov     bl, 0eh 
mov     cx, 1   
int     10h  

mov     dx, snake[6]

mov     ah, 02h
int     10h

mov     al, '*'
mov     ah, 09h
mov     bl, 0eh 
mov     cx, 1   
int     10h
            
mov     dx, snake[4]

mov     ah, 02h
int     10h

mov     al, '*'
mov     ah, 09h
mov     bl, 0eh 
mov     cx, 1   
int     10h  

mov     dx, snake[2]

mov     ah, 02h
int     10h

mov     al, '*'
mov     ah, 09h
mov     bl, 0eh 
mov     cx, 1   
int     10h  

mov     dx, snake[0]

mov     ah, 02h
int     10h

mov     al, 'O'
mov     ah, 09h
mov     bl, 0eh
mov     cx, 1  
int     10h 
         
;-----------------

call    move_snake

mov     al, ' '
mov     ah, 09h
mov     bl, 0eh 
mov     cx, 1   
int     10h



check_for_key:

mov     ah, 01h
int     16h
jz      no_key

mov     ah, 00h
int     16h

cmp     al, 1bh    
je      stop_game  

mov     cur_dir, ah

no_key:

mov     ah, 00h
int     1ah
cmp     dx, wait_time
jb      check_for_key
add     dx, 4
mov     wait_time, dx

jmp     game_loop


stop_game: 

mov     ah, 1
mov     ch, 0bh
mov     cl, 0bh
int     10h

mov     al, 1
call    scroll_t_area
jmp     processed

move_snake proc near
 
mov     ax, 40h
mov     es, ax

  mov   di, s_size * 2 - 2

  mov   cx, s_size-1
move_array:
  mov   ax, snake[di-2]
  mov   snake[di], ax
  sub   di, 2
  loop  move_array


cmp     cur_dir, left
  je    move_left
cmp     cur_dir, right
  je    move_right
cmp     cur_dir, up
  je    move_up
cmp     cur_dir, down
  je    move_down

jmp     stop_move  


move_left:
  mov   al, b.snake[0]
  dec   al
  mov   b.snake[0], al
  cmp   al, -1
  jne   stop_move       
  mov   al, es:[4ah]   
  dec   al
  mov   b.snake[0], al  
  jmp   stop_move

move_right:
  mov   al, b.snake[0]
  inc   al
  mov   b.snake[0], al
  cmp   al, es:[4ah]     
  jb    stop_move
  mov   b.snake[0], 0  
  jmp   stop_move

move_up:
  mov   al, b.snake[1]
  dec   al
  mov   b.snake[1], al
  cmp   al, -1
  jne   stop_move
  mov   al, es:[84h]   
  mov   b.snake[1], al 
  jmp   stop_move

move_down:
  mov   al, b.snake[1]
  inc   al
  mov   b.snake[1], al
  cmp   al, es:[84h]   
  jbe   stop_move
  mov   b.snake[1], 0 
  jmp   stop_move 

stop_move:   

jmp cls_command


;++++++++++run Command ++++++++++
 
run_command:
; jump over data section:
jmp     start3

; ------ data section ------

s_size1  equ     7

; the snake coordinates
; (from head to tail)
; low byte is left, high byte
; is top - [top, left]
run dw s_size1 dup(0)

tail1    dw      ?

; direction constants
;          (bios key codes):
left1    equ     4bh
right1  equ     4dh
up1      equ     48h
down1   equ     50h

; current snake direction:
cur_dir1 db      right

wait_time1 dw    0

; welcome message


; ------ code section ------

start3:

; print welcome message:
print 10,7,0101_1110b," Kacis Oyununa hos geldiniz "
print 10,8,0101_1110b," çç'ç'ççççççççççç'ççççççççç "
print 10,9,0101_1110b," Baslamak icin enter'a bas  "
print 10,10,0101_1110b," çç'ççççççç'çççççççççççççççç"
print 10,11,0101_1110b," kapatmak icin esc          "
print 10,12,0101_1110b," çççççççççç'çççççççççççççççç"
; wait for any key:
mov ah, 00h
int 16h


; hide text cursor:
mov     ah, 1
mov     ch, 2bh
mov     cl, 0bh
int     10h           


game_loop1:

;----Enemys------------------------
mov     dx, run[10]

; set cursor at dl,dh
mov     ah, 02h
int     10h

; print '*' at the location:
mov     al, '*'
mov     ah, 09h
mov     bl, 0eh ; attribute.
mov     cx, 1   ; single char.
int     10h

mov     dx, run[1]

; set cursor at dl,dh
mov     ah, 02h
int     10h

; print '*' at the location:
mov     al, '*'
mov     ah, 09h
mov     bl, 0eh ; attribute.
mov     cx, 1   ; single char.
int     10h 

mov     dx, run[-1]

; set cursor at dl,dh
mov     ah, 02h
int     10h

; print '*' at the location:
mov     al, '*'
mov     ah, 09h
mov     bl, 0eh ; attribute.
mov     cx, 1   ; single char.
int     10h 

mov     dx, run[5]

; set cursor at dl,dh
mov     ah, 02h
int     10h

; print '*' at the location:
mov     al, '*'
mov     ah, 09h
mov     bl, 0eh ; attribute.
mov     cx, 1   ; single char.
int     10h

;---------biz---------------------------  

mov     dx, run[0]


mov     ah, 02h
int     10h

mov     al, 'O'
mov     ah, 09h
mov     bl, 0eh 
mov     cx, 1   
int     10h 

; === keep the tail:
mov     ax, run[s_size1 * 2 - 2]
mov     tail1, ax

call    move_run


; === hide old tail:
mov     dx, tail1

; set cursor at dl,dh
mov     ah, 02h
int     10h

; print ' ' at the location:
mov     al, ' '
mov     ah, 09h
mov     bl, 0eh ; attribute.
mov     cx, 1   ; single char.
int     10h



check_for_key1:

; === check for player commands:
mov     ah, 01h
int     16h
jz      no_key1

mov     ah, 00h
int     16h

cmp     al, 1bh    ; esc - key?
je      stop_game1  ;

mov     cur_dir1, ah

no_key1:



; === wait a few moments here:
; get number of clock ticks
; (about 18 per second)
; since midnight into cx:dx
mov     ah, 00h
int     1ah
cmp     dx, wait_time1
jb      check_for_key1
add     dx, 4
mov     wait_time1, dx



; === eternal game loop:
jmp     game_loop1


stop_game1:


mov     ah, 1
mov     ch, 0bh
mov     cl, 0bh
int     10h

mov     al, 1
call    scroll_t_area
jmp     processed

move_run proc near
  
mov     ax, 40h
mov     es, ax

  mov   di, s_size1 * 2 - 2

  mov   cx, s_size1-1
  
move_array1:
  mov   ax, run[di-2]
  mov   run[di], ax
  sub   di, 2
  loop  move_array1


cmp     cur_dir1, left1
  je    move_left1
cmp     cur_dir1, right1
  je    move_right1
cmp     cur_dir1, up1
  je    move_up1
cmp     cur_dir1, down1
  je    move_down1

jmp     stop_move1       ; no direction.


move_left1:
  mov   al, b.run[0]
  dec   al
  mov   b.run[0], al
  cmp   al, -1
  jne   stop_move1       
  mov   al, es:[4ah]    ; col number.
  dec   al
  mov   b.run[0], al  ; return to right.
  jmp   stop_move1

move_right1:
  mov   al, b.run[0]
  inc   al
  mov   b.run[0], al
  cmp   al, es:[4ah]    ; col number.   
  jb    stop_move1
  mov   b.run[0], 0   ; return to left.
  jmp   stop_move1

move_up1:
  mov   al, b.run[1]
  dec   al
  mov   b.run[1], al
  cmp   al, -1
  jne   stop_move1
  mov   al, es:[84h]    ; row number -1.
  mov   b.run[1], al  ; return to bottom.
  jmp   stop_move1

move_down1:
  mov   al, b.run[1]
  inc   al
  mov   b.run[1], al
  cmp   al, es:[84h]    ; row number -1.
  jbe   stop_move1
  mov   b.run[1], 0   ; return to top.
  jmp   stop_move1

stop_move1: 



;+++++++++++++++++++++++++++++++++
; +++++ 'cls' command ++++++
cls_command:
call    clear_screen
jmp     processed



; +++ 'quit', 'exit', 'reboot' +++
reboot_command:
call    clear_screen 

print 5,2,0000_1100b,"Lutfen disketleri cikar                       ç"
print 5,3,0000_1100b,"ç                 'ç                          ç"
print 5,4,0000_1100b,"Yeniden baslatmak icin herhangi bir tusa bas"
print 5,5,0000_1100b,"çççççççççç'çççççççç'çççççççççççççççççççççççç"


mov ax, 0  ; wait for any key....                                    
int 16h

; store magic value at 0040h:0072h:
;   0000h - cold boot.
;   1234h - warm boot.
mov     ax, 0040h
mov     ds, ax
mov     w.[0072h], 0000h ; cold boot.
jmp	0ffffh:0000h	 ; reboot!

; ++++++++++++++++++++++++++

processed:
ret
;process_cmd endp  ;snake icin kapattim

;===========================================

; scroll all screen except last row
; up by value specified in al

scroll_t_area   proc    near

mov dx, 40h
mov es, dx  ; for getting screen parameters.
mov ah, 06h ; scroll up function id.
mov bh, 07  ; attribute for new lines.
mov ch, 0   ; upper row.
mov cl, 0   ; upper col.
mov di, 84h ; rows on screen -1,
mov dh, es:[di] ; lower row (byte).
dec dh  ; don't scroll bottom line.
mov di, 4ah ; columns on screen,
mov dl, es:[di]
dec dl  ; lower col.
int 10h

ret
scroll_t_area   endp

;===========================================




; get characters from keyboard and write a null terminated string 
; to buffer at DS:DI, maximum buffer size is in DX.
; 'enter' stops the input.
get_string      proc    near
push    ax
push    cx
push    di
push    dx

mov     cx, 0                   ; char counter.

cmp     dx, 1                   ; buffer too small?
jbe     empty_buffer            ;

dec     dx                      ; reserve space for last zero.


;============================
; eternal loop to get
; and processes key presses:

wait_for_key:

mov     ah, 0                   ; get pressed key.
int     16h

cmp     al, 0Dh                 ; 'return' pressed?
jz      exit


cmp     al, 8                   ; 'backspace' pressed?
jne     add_to_buffer
jcxz    wait_for_key            ; nothing to remove!
dec     cx
dec     di
putc    8                       ; backspace.
putc    ' '                     ; clear position.
putc    8                       ; backspace again.
jmp     wait_for_key

add_to_buffer:

        cmp     cx, dx          ; buffer is full?
        jae     wait_for_key    ; if so wait for 'backspace' or 'return'...

        mov     [di], al
        inc     di
        inc     cx
        
        ; print the key:
        mov     ah, 0eh
        int     10h

jmp     wait_for_key
;============================

exit:

; terminate by null:
mov     [di], 0

empty_buffer:

pop     dx
pop     di
pop     cx
pop     ax
ret
get_string      endp




; print a null terminated string at current cursor position, 
; string address: ds:si
print_string proc near
push    ax      ; store registers...
push    si      ;

next_char:      
        mov     al, [si]
        cmp     al, 0
        jz      printed
        inc     si
        mov     ah, 0eh ; teletype function.
        int     10h
        jmp     next_char
printed:

pop     si      ; re-store registers...
pop     ax      ;

ret
print_string endp



; clear the screen by scrolling entire screen window,
; and set cursor position on top.
; default attribute is set to white on blue.
clear_screen proc near
        push    ax      ; store registers...
        push    ds      ;
        push    bx      ;
        push    cx      ;
        push    di      ;

        mov     ax, 40h
        mov     ds, ax  ; for getting screen parameters.
        mov     ah, 06h ; scroll up function id.
        mov     al, 0   ; scroll all lines!
        mov     bh, 0000_1111b  ; attribute for new lines.
        mov     ch, 0   ; upper row.
        mov     cl, 0   ; upper col.
        mov     di, 84h ; rows on screen -1,
        mov     dh, [di] ; lower row (byte).
        mov     di, 4ah ; columns on screen,
        mov     dl, [di]
        dec     dl      ; lower col.
        int     10h

        ; set cursor position to top
        ; of the screen:
        mov     bh, 0   ; current page.
        mov     dl, 0   ; col.
        mov     dh, 0   ; row.
        mov     ah, 02
        int     10h

        pop     di      ; re-store registers...
        pop     cx      ;
        pop     bx      ;
        pop     ds      ;
        pop     ax      ;

        ret
clear_screen endp




