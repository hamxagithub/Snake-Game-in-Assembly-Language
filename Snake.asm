[ORG 0x100]
jmp start                    ; Jump to the 'start' label

    msg1: db 'WELCOME TO THE SNAKE GAME',0 ; Welcome message
    m1: db 'Shaban Aftab',0
    m2: db 'Hamza Awan',0
    m3: db 'Credit To',0
    scr: db "SCORE = ",0          ; String 'SCORE = '
    roll1: db "22F-8806",0         ; Roll number 1
    roll2: db "22F-3275",0         ; Roll number 2
    msg2: db " SNAKE GAME",0 ; Game title message
    score: dw 0                  ; Game score
    msg3: db "Khtm Tata Goodbye. Gaya !!!",0 ; Retry message
    Location_of_food: dw 0         ; Position of the food
    My_snake: db 243,'++++++++'    ; Snake representation
    delay: dd 0
    constant_way: db 2
    length: dw 8                 ; Length of the snake
    head_of_snake: dw 0          ; Position of the snake's head
    row: dw 0                    ; Current row position
    col: dw 0                    ; Current column position
    w: db 'Press_key w: UPWARD MOVEMENT',0  ; String for upward movement
    s: db 'Press_key s: DOWNWARD MOVEMENT',0 ; String for downward movement
    a: db 'Press_key a: LEFT MOVEMENT',0    ; String for left movement
    d: db 'Press_key d: RIGHT MOVEMENT',0   ; String for right movement
    b: db 'Press_key b: Back TO Menu',0   ; String for right movement




    menu1: db 'Press_key i to : INSTRUCTIONS',0
    menu2: db 'Press_key s to :  START',0
    menu3: db 'Press_key e to : EXIT',0


    ;===========================================================;
    ;===========================================================;
    ;===========================================================;
    ;===========================================================;
    ;===========================================================;



clrscr:                       ; Clear the screen routine begins
    push ax                  ; Push ax register onto the stack
    push di                  ; Push di register onto the stack
    push es                  ; Push es register onto the stack
    mov ax,0xb800             ; Set ax register with video memory address
    mov es,ax                 ; Set es register with video memory segment
    mov di,0                  ; Set di register to 0

nextloc:
    mov word[es:di],0x0720   ; Set each character on the screen to space with color attribute 0x07 (white on black)
    add di,2                  ; Move to the next character
    cmp di,4000               ; Compare di with the end of the video memory
    jne nextloc               ; Jump to nextloc if di is not equal to 4000

    pop es                       ; Pop es register from the stack
    pop di                       ; Pop di register from the stack
    pop ax                       ; Pop ax register from the stack
    ret                          ; Return from the subroutine


;==================================================================================================;



    printstr:                    ; Subroutine to print a null-terminated string
        push bp                  ; Push bp register onto the stack
        mov bp,sp                 ; Set bp register as the stack pointer
        push es                  ; Push es register onto the stack
        push ax                  ; Push ax register onto the stack
        push cx                  ; Push cx register onto the stack
        push si                  ; Push si register onto the stack
        push di                  ; Push di register onto the stack

        push ds                  ; Push ds register onto the stack
        pop es                   ; Pop es register with ds register
        mov di,[bp+4]            ; Set di register with the address of the string
        mov cx,0xffff            ; Set cx register with maximum count value
        xor al,al                ; Clear al register
        repne scasb               ; Search for the null terminator in the string
        mov ax,0xffff            ; Set ax register with maximum count value
        sub ax,cx                ; Calculate the length of the string
        sub ax,1                 ; Adjust length by subtracting 1
        cmp ax,0                 ; Check if the length is zero
        jz exit                ; Jump to exitch if the length is zero
        mov cx,ax                ; Set cx register with the length of the string

        mov ax,0xb800            ; Set ax register with video memory address
        mov es,ax                ; Set es register with video memory segment
        mov ax,80                 ; Set ax register with the number of columns in a row
        mul byte[bp+8]           ; Multiply ax by the row value
        add ax,[bp+10]           ; Add the column value to ax
        shl ax,1                 ; Multiply ax by 2 to get the offset in video memory
        mov di,ax                ; Set di register with the offset in video memory
        mov si,[bp+4]            ; Set si register with the address of the string
        mov ah,[bp+6]            ; Set ah register with the color attribute

        cld                       ; Set the constant_way flag to forward
    nextchar:
        lodsb                     ; Load the next character from the string into al
        stosw                     ; Store the character and attribute in video memory
        loop nextchar             ; Loop until all characters are processed

    exit:  
        pop di                    ; Pop di register from the stack
        pop si                    ; Pop si register from the stack
        pop cx                    ; Pop cx register from the stack
        pop ax                    ; Pop ax register from the stack
        pop es                    ; Pop es register from the stack
        pop bp                    ; Pop bp register from the stack
    ret 8                        ; Return from the subroutine, discarding parameters

;==================================================================================================;


 Printing_The_Score:                   ; Subroutine to display the score on the screen
        push bp                  ; Push bp register onto the stack
        mov bp,sp                 ; Set bp register as the stack pointer
        push ax                  ; Push ax register onto the stack
        push bx                  ; Push bx register onto the stack
        push cx                  ; Push cx register onto the stack
        push dx                  ; Push dx register onto the stack
        push es                  ; Push es register onto the stack
        push di                  ; Push di register onto the stack

        mov ax,[score]           ; Load the score into ax register
        mov bx,10                 ; Set bx register with the base for conversion
        mov cx,0                  ; Initialize cx register to 0
    loop1:
        mov dx,0                  ; Clear dx register
        div bx                    ; Divide ax by bx, result in ax, remainder in dx
        add dl,0x30               ; Convert remainder to ASCII
        push dx                   ; Push the ASCII character onto the stack
        inc cx                    ; Increment the digit count
        cmp ax,0                  ; Check if ax is zero
        jnz loop1                 ; Jump to loop1 if ax is not zero

        mov ax,0xb800             ; Set ax register with video memory address
        mov es,ax                 ; Set es register with video memory segment
        mov ax,80                  ; Set ax register with the number of columns in a row
        mul byte[bp+6]            ; Multiply ax by the row value
        add ax,[bp+4]             ; Add the column value to ax
        shl ax,1                  ; Multiply ax by 2 to get the offset in video memory
        mov di,ax                 ; Set di register with the offset in video memory

    nextnum:
        pop dx                    ; Pop the digit from the stack
        mov dh,0x12               ; Set dh register with the color attribute
        mov [es:di],dx            ; Store the digit and attribute in video memory
        add di,2                  ; Move to the next character
        loop nextnum              ; Loop until all digits are processed

        pop di                    ; Pop di register from the stack
        pop es                    ; Pop es register from the stack
        pop dx                    ; Pop dx register from the stack
        pop cx                    ; Pop cx register from the stack
        pop bx                    ; Pop bx register from the stack
        pop dx                    ; Pop dx register from the stack
        pop bp                    ; Pop bp register from the stack
    ret 4                        ; Return from the subroutine, discarding parameters


;==================================================================================================;

Printing_The_Snake:               ; Subroutine to print the snake on the screen
        push bp                ; Push bp register onto the stack
        mov bp,sp               ; Set bp register as the stack pointer
        push ax                ; Push ax register onto the stack
        push bx                ; Push bx register onto the stack
        push cx                ; Push cx register onto the stack
        push dx                ; Push dx register onto the stack
        push si                ; Push si register onto the stack
        push di                ; Push di register onto the stack
        push es                ; Push es register onto the stack
        mov si,[bp+6]          ; Set si register with the address of the snake string
        mov cx,[bp+8]          ; Set cx register with the length of the snake
        sub cx,2               ; Subtract 2 from cx to exclude the null terminator
        mov ax,80              ; Set ax register with the number of columns in a row
        mov dx,9               ; Set dx register with the row offset
        mul dx                 ; Multiply ax by dx to get the row position
        add ax,22              ; Add 22 to ax to get the column position
        shl ax,1               ; Multiply ax by 2 to get the offset in video memory
        mov di,ax              ; Set di register with the offset in video memory
        mov ax,0xb800          ; Set ax register with video memory address
        mov es,ax              ; Set es register with video memory segment
        mov bx,[bp+4]          ; Set bx register with the color attribute

    Snake_loop:
        mov al,[si]           ; Load the next character from the snake string into al
        mov [es:di],ax        ; Store the character and attribute in video memory
        mov [bx],di           ; Store the current position in memory for future use
        inc si                ; Move to the next character in the snake string
        add bx,2              ; Move to the next position in memory
        add di,2              ; Move to the next character in video memory
        dec cx                ; Decrement the character count
        jnz Snake_loop   ; Jump to Snake_loop if characters remain

    pop es                   ; Pop es register from the stack
    pop di                   ; Pop di register from the stack
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6

;==================================================================================================;



board_print:
         push ax                          ; Preserve registers
         push bx
         push cx
         push dx
         push es
         push di

              mov ax, 0xb800               ; Set ax with the starting address of the video memory
              mov es, ax                   ; Set es with the video memory segment
              mov di, 320                   ; Set di to the starting position for the left border
              mov ax, 0x02db                ; Set ax with the character and attribute for the left border

              left:
                 mov word [es:di], ax       ; Move the character and attribute to the video memory
                 add di, 160                ; Move to the next row
                 cmp di, 4000               ; Compare di with the end of the left border
                 jb left                    ; Jump to left if di is below the end

             mov di, 478                    ; Set di to the starting position for the right border
             right:
                 mov word [es:di], ax       ; Move the character and attribute to the video memory
                 add di, 160                ; Move to the next row
                 cmp di, 4000               ; Compare di with the end of the right border
                 jb right                   ; Jump to right if di is below the end

            mov al, 0x16                    ; Set al with the character for the horizontal border
            mov di, 3840                   ; Set di to the starting position for the bottom border
            down:
                  mov word [es:di], ax      ; Move the character and attribute to the video memory
                  add di, 2                 ; Move to the next column
                  cmp di, 4000               ; Compare di with the end of the bottom border
                  jb down                   ; Jump to down if di is below the end

            mov di, 320                     ; Set di to the starting position for the top border
            up:
                 mov word [es:di], ax       ; Move the character and attribute to the video memory
                 add di, 2                  ; Move to the next column
                 cmp di, 480                ; Compare di with the end of the top border
                 jb up                     ; Jump to up if di is below the end
 
           mov di, 0                        ; Set di to the starting position for clearing the center space
           mov ax, 0xb800                   ; Set ax with the starting address of the video memory
           mov es, ax                      ; Set es with the video memory segment
           mov ax, 0x0720                  ; Set ax with the character and attribute for a blank space
           next_space:
               mov [es:di], ax              ; Move the blank space to the video memory
               add di, 2                    ; Move to the next column
               cmp di, 318                   ; Compare di with the end of the center space
               jnz next_space               ; Jump to next_space if di is below the end
   
           mov ax, 33                       ; Set ax with the row for printing the message
           push ax                          ; Push ax on the stack
           mov ax, 2                        ; Set ax with the column for printing the message
           push ax                          ; Push ax on the stack
           mov ax, 0x0A                     ; Set ax with the attribute for the message
           push ax                          ; Push ax on the stack
           mov ax, msg2                     ; Set ax with the message string
           push ax                          ; Push ax on the stack
           call printstr                    ; Call the printstr function to print the message

           
           mov ax, 1                        ; Set ax with the row for printing the "SCORE:" label
           push ax                          ; Push ax on the stack
           mov ax, 1                        ; Set ax with the column for printing the "SCORE:" label
           push ax                          ; Push ax on the stack
           mov ax, 0x09                     ; Set ax with the attribute for the "SCORE:" label
           push ax                          ; Push ax on the stack
           mov ax, scr                      ; Set ax with the "SCORE:" label string
           push ax                          ; Push ax on the stack
           call printstr                    ; Call the printstr function to print the "SCORE:" label

           mov ax, 1                        ; Set ax with the row for printing the initial score
           push ax                          ; Push ax on the stack
           mov ax, 8                        ; Set ax with the column for printing the initial score
           push ax                          ; Push ax on the stack
           call Printing_The_Score                   ; Call the Printing_The_Score function to print the initial score

        pop di                              ; Restore registers
        pop es
        pop dx
        pop cx
        pop bx
        pop ax
  ret                                     ; Return from the function



;==================================================================================================;


Ending:
    call clrscr
    mov ax,34
    push ax
    mov ax,11
    push ax
    mov ax,0x41
    push ax
    mov ax,msg3
    push ax
    call printstr

 mov ax,0x4c00
 int 0x21


;==================================================================================================;



UPWARD:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push es
    push si
    push di

    mov bx, [bp + 4]
    mov dx, [bx]
    mov cx, [bp + 8]
    sub dx, 160

UP_Collision:
    cmp dx, [bx]
    jne exit_up1
    call Ending
    exit_up1:
        add bx, 2
        dec cx
        jnz UP_Collision

upping:
    mov si, [bp + 6]
    mov bx, [bp + 4]
    mov dx, [bx]
    sub dx, 160
    mov di, dx

    mov ax, 0xb800
    mov es, ax

    mov ah, 0x12
    mov al, [si]
    mov [es:di], ax
    mov cx, [bp + 8]
    mov di, [bx]
    inc si

    mov ah, 0x12
    mov al, [si]
    mov [es:di], ax

Printing_UP:
    mov ax, [bx]
    mov [bx], dx
    mov dx, ax
    add bx, 2
    dec cx
    jnz Printing_UP

    mov di, dx
    mov ax, 0x0720
    mov [es:di], ax

    push di
    sub di, 160
    cmp word[es:di], 0x3124
    je a1
    mov [es:di], ax

a1:
    sub di, 160
    cmp word[es:di], 0x3124
    je b1
    mov [es:di], ax

b1:
    pop di
    push di
    add di, 160
    cmp word[es:di], 0x3124
    je c1
    mov [es:di], ax

c1:
    add di, 160
    cmp word[es:di], 0x3124
    je d1
    mov [es:di], ax

d1:
    pop di
    push di
    add di, 2
    cmp word[es:di], 0x3124
    je e1
    mov [es:di], ax

e1:
    add di, 2
    cmp word[es:di], 0x3124
    je f1
    mov [es:di], ax

f1:
    pop di
    push di
    sub di, 2
    cmp word[es:di], 0x3124
    je g1
    mov [es:di], ax

g1:
    sub di, 2
    cmp word[es:di], 0x3124
    je h1
    mov [es:di], ax

h1:
    pop di
    call board_print
    jmp up_exit

up_exit:
    pop di
    pop si
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6
;==================================================================================================;



downward_movement:            ; Handles the downward movement of the snake
    push bp                   ; Save bp register on the stack
    mov bp, sp                ; Set bp to the current stack pointer
    push ax                   ; Save ax register on the stack
    push bx                   ; Save bx register on the stack
    push cx                   ; Save cx register on the stack
    push dx                   ; Save dx register on the stack
    push es                   ; Save es register on the stack
    push si                   ; Save si register on the stack
    push di                   ; Save di register on the stack
   
    mov bx, [bp+4]            ; Load the address of the snake's head into bx
    mov dx, [bx]              ; Load the current row position of the snake's head into dx
    mov cx, [bp+8]            ; Load the length of the snake into cx
    add dx, 160               ; Move one row down for the new head position
   
    down_hit:
        cmp dx, [bx]              ; Compare the new head position with the body positions
        jne exit_down1            ; Jump to exit_down1 if there is no collision
        call Ending             ; Call Ending if there is a collision

    exit_down1:
        add bx, 2                 ; Move to the next body position
        dec cx                    ; Decrease the remaining body positions
        jnz down_hit              ; Jump to down_hit if there are remaining body positions

    down_mov:
        mov si, [bp+6]            ; Load the address of the My_snake pattern into si
        mov bx, [bp+4]            ; Load the address of the snake's head into bx
        mov dx, [bx]              ; Load the current row position of the snake's head into dx
        add dx, 160               ; Move one row down for the new head position
        mov di, dx                ; Copy the new head position to di

        mov ax, 0xb800            ; Set ax register with the base address of the video memory
        mov es, ax                ; Set es register with the video memory segment
        mov ah, 0x12              ; Set ah register with the attribute for the snake

        mov al, [si]              ; Load the character of the My_snake pattern into al
        mov [es:di], ax           ; Store the My_snake pattern in the new head position
        mov cx, [bp+8]            ; Load the length of the snake into cx
        mov di, [bx]              ; Load the address of the snake's head into di
        inc si                    ; Move to the next character in the My_snake pattern
       
        mov ah, 0x12              ; Set ah register with the attribute for the snake
        mov al, [si]              ; Load the character of the My_snake pattern into al
        mov [es:di], ax           ; Store the My_snake pattern in the new head position

    down_printing:
        mov ax, [bx]              ; Load the address of the current body position into ax
        mov [bx], dx              ; Update the body position with the new head position
        mov dx, ax                ; Copy the current body position to dx
        add bx, 2                 ; Move to the next body position
        dec cx                    ; Decrease the remaining body positions
        jnz down_printing         ; Jump to down_printing if there are remaining body positions

        mov di, dx                ; Copy the new head position to di
        mov ax, 0x0720            ; Set ax register with the character and attribute for a blank space
        mov [es:di], ax           ; Store the blank space in the old head position

        push di                   ; Save di register on the stack
        sub di, 160               ; Move one row up
        cmp word[es:di], 0x3124   ; Compare the character and attribute with the food
        je a2                      ; Jump to a2 if there is a collision with the food
        mov [es:di], ax           ; Store the blank space in the new head position

    a2:
        sub di, 160               ; Move one row up
        cmp word[es:di], 0x3124   ; Compare the character and attribute with the food
        je b2                      ; Jump to b2 if there is a collision with the food
        mov [es:di], ax           ; Store the blank space in the new head position

    b2:
        pop di                    ; Restore di register from the stack
        push di                   ; Save di register on the stack
        add di, 160               ; Move one row down
        cmp word[es:di], 0x3124   ; Compare the character and attribute with the food
        je c2                      ; Jump to c2 if there is a collision with the food
        mov [es:di], ax           ; Store the blank space in the new head position

    c2:
        add di, 160               ; Move one row down
        cmp word[es:di], 0x3124   ; Compare the character and attribute with the food
        je d2                      ; Jump to d2 if there is a collision with the food
        mov [es:di], ax           ; Store the blank space in the new head position

    d2:
        pop di                    ; Restore di register from the stack
        push di                   ; Save di register on the stack
        add di, 2                 ; Move one column to the right
        cmp word[es:di], 0x3124   ; Compare the character and attribute with the food
        je e2                      ; Jump to e2 if there is a collision with the food
        mov [es:di], ax           ; Store the blank space in the new head position

    e2:
        add di, 2                 ; Move one column to the right
        cmp word[es:di], 0x3124   ; Compare the character and attribute with the food
        je f2                      ; Jump to f2 if there is a collision with the food
        mov [es:di], ax           ; Store the blank space in the new head position

    f2:
        pop di                    ; Restore di register from the stack
        push di                   ; Save di register on the stack
        sub di, 2                 ; Move one column to the left
        cmp word[es:di], 0x3124   ; Compare the character and attribute with the food
        je g2                      ; Jump to g2 if there is a collision with the food
        mov [es:di], ax           ; Store the blank space in the new head position

    g2:
        sub di, 2                 ; Move one column to the left
        cmp word[es:di], 0x3124   ; Compare the character and attribute with the food
        je h2                      ; Jump to h2 if there is a collision with the food
        mov [es:di], ax           ; Store the blank space in the new head position

    h2:
        pop di                    ; Restore di register from the stack
        call board_print              ; Call the board_print function to redraw the game border
        jmp down_exit             ; Jump to down_exit

down_exit:
    pop di                    ; Restore di register from the stack
    pop si                    ; Restore si register from the stack
    pop es                    ; Restore es register from the stack
    pop dx                    ; Restore dx register from the stack
    pop cx                    ; Restore cx register from the stack
    pop bx                    ; Restore bx register from the stack
    pop ax                    ; Restore ax register from the stack
    pop bp                    ; Restore bp register from the stack
    ret 6                     ; Return with the appropriate stack cleanup

   
;==================================================================================================;




leftward_movement:           ; Handles the leftward movement of the snake
        push bp                  ; Save bp register on the stack
        mov bp, sp               ; Set bp to the current stack pointer
        push ax                  ; Save ax register on the stack
        push bx                  ; Save bx register on the stack
        push cx                  ; Save cx register on the stack
        push dx                  ; Save dx register on the stack
        push es                  ; Save es register on the stack
        push si                  ; Save si register on the stack
        push di                  ; Save di register on the stack
       
        mov bx, [bp+4]           ; Load the address of the snake's head into bx
        mov dx, [bx]             ; Load the current row position of the snake's head into dx
        mov cx, [bp+8]           ; Load the length of the snake into cx
        sub dx, 2                ; Move two columns to the left for the new head position
       
    left_hit:
        cmp dx, [bx]             ; Compare the new head position with the body positions
        jne exit_left1           ; Jump to exit_left1 if there is no collision
        call Ending            ; Call Ending if there is a collision

    exit_left1:
        add bx, 2                ; Move to the next body position
        dec cx                   ; Decrease the remaining body positions
        jnz left_hit             ; Jump to left_hit if there are remaining body positions

    left_mov:
        mov si, [bp+6]           ; Load the address of the My_snake pattern into si
        mov bx, [bp+4]           ; Load the address of the snake's head into bx
        mov dx, [bx]             ; Load the current row position of the snake's head into dx
        sub dx, 2                ; Move two columns to the left for the new head position
        mov di, dx               ; Copy the new head position to di

        mov ax, 0xb800           ; Set ax register with the base address of the video memory
        mov es, ax               ; Set es register with the video memory segment
        mov ah, 0x12             ; Set ah register with the attribute for the snake

        mov al, [si]             ; Load the character of the My_snake pattern into al
        mov [es:di], ax          ; Store the My_snake pattern in the new head position
        mov cx, [bp+8]           ; Load the length of the snake into cx
        mov di, [bx]             ; Load the address of the snake's head into di
        inc si                   ; Move to the next character in the My_snake pattern
       
        mov ah, 0x12             ; Set ah register with the attribute for the snake
        mov al, [si]             ; Load the character of the My_snake pattern into al
        mov [es:di], ax          ; Store the My_snake pattern in the new head position

    left_printing:
        mov ax, [bx]             ; Load the address of the current body position into ax
        mov [bx], dx             ; Update the body position with the new head position
        mov dx, ax               ; Copy the current body position to dx
        add bx, 2                ; Move to the next body position
        dec cx                   ; Decrease the remaining body positions
        jnz left_printing        ; Jump to left_printing if there are remaining body positions

        mov di, dx               ; Copy the new head position to di
        mov ax, 0x0720           ; Set ax register with the character and attribute for a blank space
        mov [es:di], ax          ; Store the blank space in the old head position

        push di                  ; Save di register on the stack
        sub di, 160              ; Move one row up
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je a3                     ; Jump to a3 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    a3:
        sub di, 160              ; Move one row up
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je b3                     ; Jump to b3 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    b3:
        pop di                   ; Restore di register from the stack
        push di                  ; Save di register on the stack
        add di, 160              ; Move one row down
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je c3                     ; Jump to c3 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    c3:
        add di, 160              ; Move one row down
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je d3                     ; Jump to d3 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    d3:
        pop di                   ; Restore di register from the stack
        push di                  ; Save di register on the stack
        add di, 2                ; Move one column to the right
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je e3                     ; Jump to e3 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    e3:
        add di, 2                ; Move one column to the right
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je f3                     ; Jump to f3 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    f3:
        pop di                   ; Restore di register from the stack
        push di                  ; Save di register on the stack
        sub di, 2                ; Move one column to the left
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je g3                     ; Jump to g3 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    g3:
        sub di, 2                ; Move one column to the left
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je h3                     ; Jump to h3 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    h3:
        pop di                   ; Restore di register from the stack
        call board_print             ; Call the board_print function to redraw the game border
        jmp left_exit             ; Jump to left_exit

left_exit:
    pop di                   ; Restore di register from the stack
    pop si                   ; Restore si register from the stack
    pop es                   ; Restore es register from the stack
    pop dx                   ; Restore dx register from the stack
    pop cx                   ; Restore cx register from the stack
    pop bx                   ; Restore bx register from the stack
    pop ax                   ; Restore ax register from the stack
    pop bp                   ; Restore bp register from the stack
    ret 6                    ; Return with the appropriate stack cleanup

;==================================================================================================;


rightward_movement:          ; Handles the rightward movement of the snake
        push bp                  ; Save bp register on the stack
        mov bp, sp               ; Set bp to the current stack pointer
        push ax                  ; Save ax register on the stack
        push bx                  ; Save bx register on the stack
        push cx                  ; Save cx register on the stack
        push dx                  ; Save dx register on the stack
        push es                  ; Save es register on the stack
        push si                  ; Save si register on the stack
        push di                  ; Save di register on the stack
       
        mov bx, [bp+4]           ; Load the address of the snake's head into bx
        mov dx, [bx]             ; Load the current row position of the snake's head into dx
        mov cx, [bp+8]           ; Load the length of the snake into cx
        add dx, 2                ; Move two columns to the right for the new head position
       
    right_hit:
        cmp dx, [bx]             ; Compare the new head position with the body positions
        jne exit_right1          ; Jump to exit_right1 if there is no collision
        call Ending            ; Call Ending if there is a collision

    exit_right1:
        add bx, 2                ; Move to the next body position
        dec cx                   ; Decrease the remaining body positions
        jnz right_hit            ; Jump to right_hit if there are remaining body positions

    right_mov:
        mov si, [bp+6]           ; Load the address of the My_snake pattern into si
        mov bx, [bp+4]           ; Load the address of the snake's head into bx
        mov dx, [bx]             ; Load the current row position of the snake's head into dx
        add dx, 2                ; Move two columns to the right for the new head position
        mov di, dx               ; Copy the new head position to di

        mov ax, 0xb800           ; Set ax register with the base address of the video memory
        mov es, ax               ; Set es register with the video memory segment
        mov ah, 0x12             ; Set ah register with the attribute for the snake

        mov al, [si]             ; Load the character of the My_snake pattern into al
        mov [es:di], ax          ; Store the My_snake pattern in the new head position
        mov cx, [bp+8]           ; Load the length of the snake into cx
        mov di, [bx]             ; Load the address of the snake's head into di
        inc si                   ; Move to the next character in the My_snake pattern
       
        mov ah, 0x12             ; Set ah register with the attribute for the snake
        mov al, [si]             ; Load the character of the My_snake pattern into al
        mov [es:di], ax          ; Store the My_snake pattern in the new head position

    right_printing:
        mov ax, [bx]             ; Load the address of the current body position into ax
        mov [bx], dx             ; Update the body position with the new head position
        mov dx, ax               ; Copy the current body position to dx
        add bx, 2                ; Move to the next body position
        dec cx                   ; Decrease the remaining body positions
        jnz right_printing       ; Jump to right_printing if there are remaining body positions

        mov di, dx               ; Copy the new head position to di
        mov ax, 0x0720           ; Set ax register with the character and attribute for a blank space
        mov [es:di], ax          ; Store the blank space in the old head position

        push di                  ; Save di register on the stack
        sub di, 160              ; Move one row up
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je a4                     ; Jump to a4 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    a4:
        sub di, 160              ; Move one row up
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je b4                     ; Jump to b4 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    b4:
        pop di                   ; Restore di register from the stack
        push di                  ; Save di register on the stack
        add di, 160              ; Move one row down
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je c4                     ; Jump to c4 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    c4:
        add di, 160              ; Move one row down
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je d4                     ; Jump to d4 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    d4:
        pop di                   ; Restore di register from the stack
        push di                  ; Save di register on the stack
        add di, 2                ; Move one column to the right
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je e4                     ; Jump to e4 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    e4:
        add di, 2                ; Move one column to the right
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je f4                     ; Jump to f4 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    f4:
        pop di                   ; Restore di register from the stack
        push di                  ; Save di register on the stack
        sub di, 2                ; Move one column to the left
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je g4                     ; Jump to g4 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    g4:
        sub di, 2                ; Move one column to the left
        cmp word[es:di], 0x3124  ; Compare the character and attribute with the food
        je h4                     ; Jump to h4 if there is a collision with the food
        mov [es:di], ax          ; Store the blank space in the new head position

    h4:
        pop di                   ; Restore di register from the stack
        call board_print             ; Call the board_print function to redraw the game border
        jmp right_exit            ; Jump to right_exit

right_exit:
    pop di                   ; Restore di register from the stack
    pop si                   ; Restore si register from the stack
    pop es                   ; Restore es register from the stack
    pop dx                   ; Restore dx register from the stack
    pop cx                   ; Restore cx register from the stack
    pop bx                   ; Restore bx register from the stack
    pop ax                   ; Restore ax register from the stack
    pop bp                   ; Restore bp register from the stack
    ret 6                    ; Return with the appropriate stack cleanup




;==================================================================================================;




keyboard_print:                  ; Keyboard_print input handling
        push ax                ; Save ax register on the stack
        push bx                ; Save bx register on the stack
        push cx                ; Save cx register on the stack
        push dx                ; Save dx register on the stack

     repeat:
       mov dword[delay],100000
    re:
        dec dword[delay]
        cmp dword[delay],0
        jne re
    Press_key:

        mov ah, 01h
        int 16h
        jz No_input
        mov ah,0
        int 16h
     
        cmp ah, 0x11         ; Compare ah register with the scan code for the 'W' key
        je uper           ; Jump to uper if the 'W' key is Press_keyed

        cmp ah, 0x1e           ; Compare ah register with the scan code for the 'A' key
        je left_ko         ; Jump to left_ko if the 'A' key is Press_keyed

        cmp ah, 0x20            ; Compare ah register with the scan code for the 'D' key
        je right_ko        ; Jump to right_ko if the 'D' key is Press_keyed

        cmp ah, 0x1f            ; Compare ah register with the scan code for the 'S' key
        je neecha         ; Jump to neecha if the 'S' key is Press_keyed
        cmp ah, ' '
        jne repeat  

        mov ax, 0x4c00          ; Set ax register with the exit code for the program
       

    ;-----------------      exit condition    ---------------;
        pop dx                  ; Restore dx register from the stack
        pop cx                  ; Restore cx register from the stack
        pop bx                  ; Restore bx register from the stack
        pop ax                  ; Restore ax register from the stack
        ret                     ; Return with the appropriate stack cleanup


   
       

            No_input:
                cmp byte[constant_way], 0
                je uper
                cmp byte[constant_way], 1
                je neecha
                cmp byte[constant_way], 2
                je left_ko
                cmp byte[constant_way], 3
                je right_ko
               
        uper:

            mov byte[constant_way],0
            push word[length]       ; Push the length of the snake onto the stack
            mov bx, My_snake      ; Load the address of the My_snake pattern into bx
            push bx                 ; Push the address of the My_snake pattern onto the stack
            mov bx, head_of_snake   ; Load the address of the snake's head into bx
            push bx                 ; Push the address of the snake's head onto the stack
            call UPWARD    ; Call the UPWARD function
            jmp checking            ; Jump to checking

        neecha:
            mov byte[constant_way],1

            push word[length]       ; Push the length of the snake onto the stack
            mov bx, My_snake      ; Load the address of the My_snake pattern into bx
            push bx                 ; Push the address of the My_snake pattern onto the stack
            mov bx, head_of_snake   ; Load the address of the snake's head into bx
            push bx                 ; Push the address of the snake's head onto the stack
            call downward_movement  ; Call the downward_movement function
            jmp checking            ; Jump to checking

        left_ko:
                mov byte[constant_way],2

            push word[length]       ; Push the length of the snake onto the stack
            mov bx, My_snake      ; Load the address of the My_snake pattern into bx
            push bx                 ; Push the address of the My_snake pattern onto the stack
            mov bx, head_of_snake   ; Load the address of the snake's head into bx
            push bx                 ; Push the address of the snake's head onto the stack
            call leftward_movement  ; Call the leftward_movement function
            jmp checking            ; Jump to checking

        right_ko:
            mov byte[constant_way],3

            push word[length]       ; Push the length of the snake onto the stack
            mov bx, My_snake      ; Load the address of the My_snake pattern into bx
            push bx                 ; Push the address of the My_snake pattern onto the stack
            mov bx, head_of_snake   ; Load the address of the snake's head into bx
            push bx                 ; Push the address of the snake's head onto the stack
            call rightward_movement ; Call the rightward_movement function
            jmp checking            ; Jump to checking

        checking:
            call Chech_if_dead  ; Call the Chech_if_dead function
            push word[Location_of_food] ; Push the position of the food onto the stack
            push word[length]       ; Push the length of the snake onto the stack
            mov ax, My_snake      ; Load the address of the My_snake pattern into ax
            push ax                 ; Push the address of the My_snake pattern onto the stack
            mov ax, head_of_snake   ; Load the address of the snake's head into ax
            push ax                 ; Push the address of the snake's head onto the stack
            call check_food         ; Call the check_food function
            ;jmp Press_key              ; Jump back to Press_key to continue processing keyboard_print input
            jmp repeat
   
;==================================================================================================;




Chech_if_dead:            ; Check if the snake's head collides with the body or borders
        push ax                ; Save ax register on the stack
        push bx                ; Save bx register on the stack
        push cx                ; Save cx register on the stack
        push dx                ; Save dx register on the stack
        push di                ; Save di register on the stack
        push si                ; Save si register on the stack
        push es                ; Save es register on the stack

        r1:
        mov dx, 158            ; Initialize dx with the column value for the right border
        rcoll:
            add dx, 160          ; Move to the next column to the right
            cmp dx, 4000         ; Check if the right border is reached
            jae l1               ; Jump to l1 if the right border is reached
            cmp [head_of_snake], dx ; Compare the snake's head position with the current column
            je finish            ; Jump to finish if collision is detected
            ja rcoll            ; Jump back to rcoll if there is no collision and move to the next column

    l1:
        mov dx, 0               ; Reset dx to 0 for the left border
        lcoll:
            add dx, 160          ; Move to the next column to the left
            cmp dx, 4000         ; Check if the left border is reached
            jae dddd             ; Jump to dddd if the left border is reached
            cmp [head_of_snake], dx ; Compare the snake's head position with the current column
            je finish            ; Jump to finish if collision is detected
            ja lcoll            ; Jump back to lcoll if there is no collision and move to the next column

    u1:    
        mov dx, 320              ; Initialize dx with the row value for the top border
        upcoll:
            add dx, 2            ; Move to the next row upwards
            cmp dx, 480          ; Check if the top border is reached
            jae dddd             ; Jump to dddd if the top border is reached
            cmp [head_of_snake], dx ; Compare the snake's head position with the current row
            je finish            ; Jump to finish if collision is detected
            ja upcoll           ; Jump back to upcoll if there is no collision and move to the next row

        dddd:
        mov dx, 3840             ; Initialize dx with the row value for the bottom border
        dcoll:
            add dx, 2            ; Move to the next row downwards
            cmp dx, 4000         ; Check if the bottom border is reached
            jae end              ; Jump to end if the bottom border is reached
            cmp [head_of_snake], dx ; Compare the snake's head position with the current row
            je finish            ; Jump to finish if collision is detected
            jb dcoll            ; Jump back to dcoll if there is no collision and move to the next row

finish:
    mov ax, 34                ; Set ax register with the color attribute for the message
    push ax                  ; Push the color attribute onto the stack
    mov ax, 12                ; Set ax register with the length of the message
    push ax                  ; Push the length of the message onto the stack
    mov ax, 0x0f              ; Set ax register with the display page
    push ax                  ; Push the display page onto the stack
    mov ax, msg3              ; Set ax register with the address of the message
    push ax                  ; Push the address of the message onto the stack
    call printstr            ; Call the printstr function to display the message

       pop es                 ; Restore es register from the stack
       pop si                 ; Restore si register from the stack
       pop di                 ; Restore di register from the stack
       pop dx                 ; Restore dx register from the stack
       pop cx                 ; Restore cx register from the stack
       pop bx                 ; Restore bx register from the stack
       pop ax                 ; Restore ax register from the stack

  mov ax, 0x4c00             ; Set ax register with the exit code for the program
  int 0x21                   ; Call interrupt 0x21 to exit the program

end:
       pop es                 ; Restore es register from the stack
       pop si                 ; Restore si register from the stack
       pop di                 ; Restore di register from the stack
       pop dx                 ; Restore dx register from the stack
       pop cx                 ; Restore cx register from the stack
       pop bx                 ; Restore bx register from the stack
       pop ax                 ; Restore ax register from the stack
ret                           ; Return from the function


;==================================================================================================;




Random_pos_generator:                          ; Generate a Random_pos_generator position for the food
        push ax                     ; Save ax register on the stack
        push bx                     ; Save bx register on the stack
        push cx                     ; Save cx register on the stack
        push dx                     ; Save dx register on the stack
        push si                     ; Save si register on the stack
        push di                     ; Save di register on the stack
        push es                     ; Save es register on the stack

        inloop:
            mov ah, 00h               ; Generate a Random_pos_generator number help by chatgpt
            int 1ah

            mov ax, dx
            mov dx, 0                  ; Clear dx
            mov cx, 25                 ; Set cx with the maximum number of rows
            div cx                     ; Divide the Random_pos_generator number by the number of rows

            mov [row], dx              ; Save the result in the 'row' variable

            mov ah, 00h
            int 1ah

            mov ax, dx
            mov dx, 0                  ; Clear dx
            mov cx, 80                 ; Set cx with the maximum number of columns
            div cx                     ; Divide the Random_pos_generator number by the number of columns

            mov [col], dx              ; Save the result in the 'col' variable

        mov ax, 80
        mov bx, [row]
        mul bx                      ; Multiply the row by the maximum number of columns
        mov bx, [col]
        add ax, bx
        shl ax, 1                   ; Multiply the result by 2 to get the position in the video memory

    not_at_up:
        mov di, 0                   ; Initialize di for the top border
        loop_up:
            cmp di, ax               ; Compare di with the calculated position
            je inloop                ; Jump to inloop if there is a match
            add di, 2                 ; Move to the next position in the row
            cmp di, 480               ; Check if the bottom border is reached
            jb loop_up                ; Jump back to loop_up if not reached

        not_at_down:
        mov di, 3840               ; Initialize di for the bottom border
        loop_down:
            cmp di, ax              ; Compare di with the calculated position
            je inloop               ; Jump to inloop if there is a match
            add di, 2               ; Move to the next position in the row
            cmp di, 4000            ; Check if the bottom border is reached
            jb loop_down             ; Jump back to loop_down if not reached

        not_at_left:
            mov di, 0                  ; Initialize di for the left border
            loop_left:
                cmp di, ax             ; Compare di with the calculated position
                je inloop              ; Jump to inloop if there is a match
                add di, 160             ; Move to the next row
                cmp di, 4000           ; Check if the right border is reached
                jb loop_left            ; Jump back to loop_left if not reached

        not_at_right:
            mov di, 158                ; Initialize di for the right border
            loop_right:
                cmp di, ax               ; Compare di with the calculated position
                je inloop                ; Jump to inloop if there is a match
                add di, 160               ; Move to the next row
                cmp di, 4000             ; Check if the right border is reached
                jb loop_right            ; Jump back to loop_right if not reached

        cmp ax, 4000                   ; Check if the calculated position is beyond the screen borders
    jg inloop                       ; Jump to inloop if beyond the borders

    mov word [Location_of_food], ax   ; Save the calculated position in the food position variable
    cmp word [Location_of_food], 0x0f6f ; Compare with the snake's body positions
    je inloop                      ; Jump to inloop if there is a collision with the snake's body

    pop es                         ; Restore es register from the stack
    pop di                         ; Restore di register from the stack
    pop si                         ; Restore si register from the stack
    pop dx                         ; Restore dx register from the stack
    pop cx                         ; Restore cx register from the stack
    pop bx                         ; Restore bx register from the stack
    pop ax                         ; Restore ax register from the stack
    ret                              ; Return from the function


;==================================================================================================;



food:                                 ; Print the food on the screen
      push ax                        ; Save ax register on the stack
      push bx                        ; Save bx register on the stack
      push cx                        ; Save cx register on the stack
      push dx                        ; Save dx register on the stack
      push di                        ; Save di register on the stack
      push es                        ; Save es register on the stack

           call Random_pos_generator                ; Call the Random_pos_generator function to generate a Random_pos_generator position
         
         mov ax, 80                    ; Set ax with the width of each row
         mov cx, [row]                 ; Load cx with the Random_pos_generator row
         mul cx                         ; Multiply ax by cx to get the offset within the row
         mov cx, [col]                 ; Load cx with the Random_pos_generator column
         add ax, cx                    ; Add cx to ax to get the total offset in video memory
         shl ax, 1                     ; Multiply ax by 2 to get the actual position in video memory
         mov di, ax                    ; Move the position to di

         mov ax, 0xb800                ; Set ax with the starting address of video memory
         mov es, ax                    ; Move ax to es to set the segment of video memory

         mov ax, 0x3124                ; Set ax with the character and attribute for the food
         mov word [es:di], ax          ; Store the character and attribute in the video memory

     pop es                           ; Restore es register from the stack
     pop di                           ; Restore di register from the stack
     pop dx                           ; Restore dx register from the stack
     pop cx                           ; Restore cx register from the stack
     pop bx                           ; Restore bx register from the stack
     pop ax                           ; Restore ax register from the stack

 ret                                 ; Return from the function


;==================================================================================================;

check_food:                          ; Check if the snake head collides with the food
        push bp                          ; Save bp register on the stack
        mov bp, sp                       ; Set bp as the stack pointer
        push ax                          ; Save ax register on the stack
        push bx                          ; Save bx register on the stack
        push cx                          ; Save cx register on the stack
        push dx                          ; Save dx register on the stack
        push es                          ; Save es register on the stack
        push di                          ; Save di register on the stack
        push si                          ; Save si register on the stack

        mov ax, 0xb800                   ; Set ax with the starting address of video memory
        mov es, ax                     ; Move ax to es to set the segment of video memory
        mov ax, 0x0720                  ; Set ax with the character and attribute for a blank space
        mov di, 0                       ; Initialize di to point to the first row in video memory

        firstline:
            mov word [es:di], ax        ; Store the blank space in the entire first line of the screen
            add di, 2                    ; Move to the next position in the row
            cmp di, 158                   ; Check if the end of the first line is reached
            jne firstline                ; Jump back to firstline if not reached

        mov bx, [bp + 4]                  ; Load bx with the address of the snake's head position
        mov dx, [bp + 10]                 ; Load dx with the food position

        cmp [bx], dx                      ; Compare the snake's head position with the food position
        jne not_change                    ; Jump to not_change if no collision

        add word [score], 1               ; Increase the score by 1
            mov ax, 1                   ; Set ax with the row for printing the score
            push ax                     ; Push ax on the stack
            mov ax, 8                   ; Set ax with the column for printing the score
            push ax                     ; Push ax on the stack
            call Printing_The_Score              ; Call the Printing_The_Score function to print the updated score

        mov cx, [bp + 8]                  ; Load cx with the snake length
        dec cx
        shl cx, 1                         ; Multiply cx by 2 to get the offset of the last body position
        add bx, cx                        ; Move bx to the last body position
        mov dx, [bx]                      ; Load dx with the last body position
        sub dx, [bx - 2]                  ; Subtract the second last body position from the last position

        mov ax, [bx]                      ; Load ax with the last body position
        add ax, dx                        ; Add the difference to ax to get the new head position
        mov dx, ax                        ; Move ax to dx

    shr cx, 1                          ; Divide cx by 2 to get the count of body positions
    inc cx                             ; Increment cx to include the head position
        add word[length], 1               ; Increase the length of the snake by 1

        add bx, 2                         ; Move bx to the next body position
        mov [bx], dx                      ; Update the body position with the new head position
        mov si, [bp + 6]                  ; Load si with the address of the snake's body characters

        mov ax, 0xb800                    ; Set ax with the starting address of video memory
        mov es, ax                        ; Move ax to es to set the segment of video memory
        mov di, dx                        ; Move dx to di to set the position in video memory
        mov ah, 0x0f                      ; Set ah with the attribute for the snake's head
        mov al, [si]                      ; Load al with the character for the snake's head

        mov [es:di], ax                   ; Store the character and attribute in the video memory


        mov ax, 0xb800                    ; Set ax with the starting address of video memory
        mov es, ax                        ; Move ax to es to set the segment of video memory

        call food                         ; Call the food function to print a new food

    not_change:
        pop si                            ; Restore si register from the stack
        pop di                            ; Restore di register from the stack
        pop es                            ; Restore es register from the stack
        pop dx                            ; Restore dx register from the stack
        pop cx                            ; Restore cx register from the stack
        pop bx                            ; Restore bx register from the stack
        pop ax                            ; Restore ax register from the stack
        pop bp                            ; Restore bp register from the stack
    ret 8                               ; Return from the function

;==================================================================================================;



menu_display:

call clrscr                              ; Clear the screen
   mov ax, 25                               ; Set ax with the row for printing the instructions
   push ax                                  ; Push ax on the stack
   mov ax, 9                                ; Set ax with the column for printing the instructions
   push ax                                  ; Push ax on the stack
   mov ax, 0x09                             ; Set ax with the attribute for the instructions
   push ax                                  ; Push ax on the stack
   mov ax, menu2                                ; Set ax with the message string for the instructions
   push ax                                  ; Push ax on the stack
   call printstr                            ; Call the printstr function to print the instructions

   mov ax, 25                               ; Set ax with the row for printing 'S'
   push ax                                  ; Push ax on the stack
   mov ax, 10                               ; Set ax with the column for printing 'S'
   push ax                                  ; Push ax on the stack
   mov ax, 0x0719                           ; Set ax with the message string for 'S'
   push ax                                  ; Push ax on the stack
    mov ax, menu1                                ; Set ax with the attribute for 'S'
   push ax                                  ; Push ax on the stack
   call printstr                            ; Call the printstr function to print 'S'

   mov ax, 25                               ; Set ax with the row for printing 'A'
   push ax                                  ; Push ax on the stack
   mov ax, 11                               ; Set ax with the column for printing 'A'
   push ax                                  ; Push ax on the stack
   mov ax, 0x0c                             ; Set ax with the attribute for 'A'
   push ax                                  ; Push ax on the stack
   mov ax, menu3                              ; Set ax with the message string for 'A'
   push ax                                  ; Push ax on the stack
   call printstr                            ; Call the printstr function to print 'A'

 
   ret


;==================================================================================================;

Starting_the_game:

call clrscr                              ; Clear the screen
   call board_print                             ; Draw the game board_print

   push word[length]                        ; Push the length of the snake on the stack
   mov ax, My_snake                       ; Set ax with the address of the snake's body characters
   push ax                                  ; Push ax on the stack
   mov ax, head_of_snake                    ; Set ax with the address of the snake's head position
   push ax                                  ; Push ax on the stack
   call Printing_The_Snake                         ; Call the Printing_The_Snake function to print the snake
   call food                                ; Call the food function to print the initial food
   call keyboard_print                            ; Call the keyboard_print function to handle user input


INSTRU:

;=====================    INSTRUCTION DISPLAY    ==============;

   call clrscr                              ; Clear the screen
   mov ax, 25                               ; Set ax with the row for printing the instructions
   push ax                                  ; Push ax on the stack
   mov ax, 9                                ; Set ax with the column for printing the instructions
   push ax                                  ; Push ax on the stack
   mov ax, 0x09                             ; Set ax with the attribute for the instructions
   push ax                                  ; Push ax on the stack
   mov ax, w                                ; Set ax with the message string for the instructions
   push ax                                  ; Push ax on the stack
   call printstr                            ; Call the printstr function to print the instructions

   mov ax, 25                               ; Set ax with the row for printing 'S'
   push ax                                  ; Push ax on the stack
   mov ax, 10                               ; Set ax with the column for printing 'S'
   push ax                                  ; Push ax on the stack
   mov ax, 0x0719                           ; Set ax with the message string for 'S'
   push ax                                  ; Push ax on the stack
    mov ax, s                                ; Set ax with the attribute for 'S'
   push ax                                  ; Push ax on the stack
   call printstr                            ; Call the printstr function to print 'S'

   mov ax, 25                               ; Set ax with the row for printing 'A'
   push ax                                  ; Push ax on the stack
   mov ax, 11                               ; Set ax with the column for printing 'A'
   push ax                                  ; Push ax on the stack
   mov ax, 0x0c                             ; Set ax with the attribute for 'A'
   push ax                                  ; Push ax on the stack
   mov ax, a                                ; Set ax with the message string for 'A'
   push ax                                  ; Push ax on the stack
   call printstr                            ; Call the printstr function to print 'A'

   mov ax, 25                               ; Set ax with the row for printing 'D'
   push ax                                  ; Push ax on the stack
   mov ax, 12                               ; Set ax with the column for printing 'D'
   push ax                                  ; Push ax on the stack
   mov ax, 0x07                             ; Set ax with the attribute for 'D'
   push ax                                  ; Push ax on the stack
   mov ax, d                                ; Set ax with the message string for 'D'
   push ax                                  ; Push ax on the stack
   call printstr                            ; Call the printstr function to print 'D'

    mov ax, 25                               ; Set ax with the row for printing 'D'
   push ax                                  ; Push ax on the stack
   mov ax, 13                               ; Set ax with the column for printing 'D'
   push ax                                  ; Push ax on the stack
   mov ax, 0x07                             ; Set ax with the attribute for 'D'
   push ax                                  ; Push ax on the stack
   mov ax, b                                ; Set ax with the message string for 'D'
   push ax                                  ; Push ax on the stack
   call printstr                            ; Call the printstr function to print 'D'

    mov ah,0
    int 16h
    cmp al,'b'
     call S
   
   ret

Intro:

 mov ax, 25                  ;x            ; Set ax with the row for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 9                     ;y           ; Set ax with the column for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 0x0f                             ; Set ax with the attribute for the title
   push ax                                  ; Push ax on the stack
   mov ax, msg1                             ; Set ax with the message string for the title
   push ax                                  ; Push ax on the stack
   
   call printstr                            ; Call the printstr function to print the title


   mov ax, 25                  ;x            ; Set ax with the row for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 11                     ;y           ; Set ax with the column for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 0x0f                             ; Set ax with the attribute for the title
   push ax                                  ; Push ax on the stack
   mov ax, m3                             ; Set ax with the message string for the title
   push ax                                  ; Push ax on the stack
   
   call printstr                            ; Call the printstr function to print the title


   mov ax, 25                  ;x            ; Set ax with the row for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 13                    ;y           ; Set ax with the column for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 0x0f                             ; Set ax with the attribute for the title
   push ax                                  ; Push ax on the stack
   mov ax, m1                             ; Set ax with the message string for the title
   push ax                                  ; Push ax on the stack
   
   call printstr                            ; Call the printstr function to print the title

   mov ax, 25                  ;x            ; Set ax with the row for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 14                     ;y           ; Set ax with the column for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 0x0f                             ; Set ax with the attribute for the title
   push ax                                  ; Push ax on the stack
   mov ax, m2                             ; Set ax with the message string for the title
   push ax                                  ; Push ax on the stack
   
   call printstr                            ; Call the printstr function to print the title
 
   mov ax, 40                  ;x            ; Set ax with the row for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 13                     ;y           ; Set ax with the column for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 0x0f                             ; Set ax with the attribute for the title
   push ax                                  ; Push ax on the stack
   mov ax, roll2                             ; Set ax with the message string for the title
   push ax                                  ; Push ax on the stack
   
   call printstr                            ; Call the printstr function to print the title

    mov ax, 40                  ;x            ; Set ax with the row for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 14                     ;y           ; Set ax with the column for printing the title
   push ax                                  ; Push ax on the stack
   mov ax, 0x0f                             ; Set ax with the attribute for the title
   push ax                                  ; Push ax on the stack
   mov ax, roll1                             ; Set ax with the message string for the title
   push ax                                  ; Push ax on the stack
   
   call printstr                            ; Call the printstr function to print the title


   mov ah, 0x1                              ; Wait for a key Press_key
   int 0x21                                 ; DOS interrupt
    ret;




start:

   call clrscr                              ; Clear the screen

   call Intro

;=====================    INSTRUCTION call    ==============;


S:  
    call clrscr         ; Clear the screen
    call menu_display
    mov ah, 0
    int 16h             ; Get the character

    cmp al, 's'
    je Starting_the_game

    cmp al, 'i'
    je INSTRU

    cmp al, 'e'
    je e

    jmp S               ; If the pressed key is not 's', 'i', or 'e', go back to the menu

   ;------------------------------------------------------------------

 e:  
   mov ax, 0x4c00                           ; Set ax with the exit code for DOS
   int 0x21        
