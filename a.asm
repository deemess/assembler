;docs:
;http://www.z80.info/decoding.htm
;https://clrhome.org/table/
        
        DEVICE ZXSPECTRUM128
        include "../_sdk/sys_h.asm"

        org PROGSTART
begin
        call initstdio

        ld hl, hello
        call println

        ld hl, COMMANDLINE ;command line
        ld de, wordbuf
        call getword    ;skip application name
        call skipspaces
        ld de, wordbuf
        call getword    ;get assembler file name
        call skipspaces
        ld de, line
        call getword    ;get binary file name

        ld hl, asmfile  ;print assembler file name
        call print
        ld hl, wordbuf
        call println

        ld hl, binfile  ;print binary file name
        call print
        ld hl, line
        call println

        ;открываем ассемблерный файл
        ld de, wordbuf
        OS_OPENHANDLE   ; de = file name
        or a
        jp nz, errorquit
        ld a,b
        ld (hfile),a
        
        ;создаем выходной бинарный файл
        ld de, line
        OS_CREATEHANDLE   ; de = file name
        or a
        jr nz, errorquit
        ld a,b
        ld (hbinfile),a

readloop 
        ld hl, line
        call readline ; читаем строку из файла
        or a
        jr nz, endfile

        ld hl, line
        call println ; печатаем строку
        
        ld hl, line
        call strlen
        ld bc, hl
        ld hl, line
        call strdelcomments ; удаляем комментарии
        ld hl, line
        call skipspaces ; пропускаем пробелы
        call strisempty ; если пустая - читаем следующую
        jr z, readloop
        
        ld de, operand
        call getword
        call skipspaces
        ld (linepos), hl    ; сохраняем текущую позицию в строке, для дальнейшего парсинга
        ld de, operand
        ld hl, operand
        call uppercase  ; операнд в верхний регистр
        
        ld de, operand
        call parseoperand   ; парсим операнд
        cp -1
        jp z,errorquit
        jr readloop

endfile        
        ;закрываем асм файл
        ld a,(hfile)
        ld b,a
        OS_CLOSEHANDLE
        
        ;сохраняем ассемблированный бинарь в файл
        ld a,(hbinfile)
        ld b,a
        ld de,binary
        ld hl,(binsize)
        OS_WRITEHANDLE
        or a
        jp nz,errorquit    ;обработка ошибок
                
        ;закрываем бинарный файл
        ld a,(hbinfile)
        ld b,a
        OS_CLOSEHANDLE
        
        ld hl,0
        QUIT
        
errorquit
;выходим если произошла ошибка
        ld hl, errormsg
        call println
        ld hl, line
        call println
        ld l,-1
        ld h,0
        QUIT


parseoperand
;вход de - строка операнда
;выход 
;   для найденого операнда из таблицы вызывается процедура с кодом операнда в bc
;   для не найденого операнда просто пока возращается -1
        ld hl,operandlist ;list of internal commands
strcpexec0
        ld c,(hl)
        inc hl
        ld b,(hl) ;код операнда для процедуры
        inc hl
        push bc ; сохраняем код операнда
        ld c,(hl)
        inc hl
        ld b,(hl) ;адрес процедуры, соответствующей этой команде
        inc hl
        ld a,b
        cp -1
        ret z;jp z,fail_syntax ;ret z ;jr z,strcpexec_tryrun ;a!=0: no such command
        push hl
        push de ;сохраняем указатель на операнд
        call strcp
        pop de
        pop hl
        jr nz,strcpexec_fail
        ld h,b  ;адрес процедуры
        ld l,c
        pop bc  ;восстанавливаем код операнда
        jp (hl) ;run internal command
strcpexec_fail
        pop bc ; вытаскиваем адрес процедуры для очистки стэка
        ld b,-1 ;чтобы точно найти терминатор
        xor a
        cpir ;найдём обязательно
        jr strcpexec0
        ret

        
readline
;читает 1 строку из файла, удаляет 0D 0A в конце, и добавляет 0 (конец строки)
        ld de, line
read
        push de
        ld hl, 1
        ld a,(hfile)
        ld b,a
        OS_READHANDLE   ;de = dest, hl = count, b = file handle
        pop de
        or a
        ret nz
        ld a,h
        or l
        jr z, readquit
        ld a,(de)
        inc de
        cp 0x0a
        jr nz, read
        dec de
        dec de
readquit
        xor a
        ld (de), a   ; end line
        ret
        
print
;hl=text
        push hl
        call strlen ;hl=length
        pop de ;de=text
        jp sendchars    ;in: de=buf, hl=size, out: A=error

println
;hl=text
        push hl
        call strlen ;hl=length
        pop de ;de=text
        call sendchars  ;in: de=buf, hl=size, out: A=error
        ld de, newline
        ld hl, 2
        jp sendchars

printhexln
;a = hex number
        ld hl, hexbuf
        push af
        and 0xf0
        rrca
        rrca
        rrca
        rrca
        cp 10
        jr c, printhexln1 ; If A < 10, jump
        add a, 'A' - 10 ; Convert to 'A'-'F'
        jr printhexln2
printhexln1:
        add a, '0' ; Convert to '0'-'9'
printhexln2
        ld (hl), a
        inc hl
        pop af
        and 0x0f
        cp 10
        jr c, printhexln3 ; If A < 10, jump
        add a, 'A' - 10 ; Convert to 'A'-'F'
        jr printhexln4
printhexln3:
        add a, '0' ; Convert to '0'-'9'
printhexln4
        ld (hl), a
        ld de, hexbuf
        ld hl, 2
        call sendchars
        ld de, newline
        ld hl, 2
        jp sendchars
hexbuf  dw 0

strlen
;hl=str
;out: hl=length
        xor a
        ld b,a
        ld c,a ;чтобы точно найти терминатор
        cpir ;найдём обязательно, если длина=0, то bc=-1 и т.д.
        ld hl,-1
        or a
        sbc hl,bc
        ret

strisempty
;hl=str
;out: Z - пустая
        ld a,(hl)
        or a
        ret

strdelcomments
;hl=str
;bc=str len
;out: если в строке найден символ комментария ; то на это место записывается 0
        ld a, ';'
        cpir 
        ret nz
        dec hl
        xor a
        ld (hl),a 
        ret


strcp
;hl=s1
;de=s2
;out: Z (equal, hl=terminator of s1+1, de=terminator of s2+1), NZ (not equal, hl=erroraddr in s1, de=erroraddr in s2)
strcp0.
        ld a,[de] ;s2
        cp [hl] ;s1
        ret nz
        inc hl
        inc de
        or a
        jp nz,strcp0.
        ret ;z

skipspaces
;hl=string
;out: hl=after last space
        ld a,(hl)
        cp ' '
        ret nz
        inc hl
        jr skipspaces

getword
;hl=string
;de=wordbuf
;out: hl=terminator/space addr
getword0
        ld a,(hl)
        or a
        jr z,getwordq
        sub ' '
        jr z,getwordq
        ldi
        jp getword0
getwordq
        ;xor a
        ld (de),a
        ret

uppercase
;hl=исходная строка
;de=целевая строка (может быть равна hl)
        ld a, (hl)
        or a
        ret z
        cp 'a'
        jr c, uppercase_store
        cp 'z' + 1
        jr nc, uppercase_store
        sub 'a' - 'A'
uppercase_store
        ld (de), A
        inc hl
        inc de
        jr uppercase

op_push
        push bc; сохраняем операнд
        call readnextword
        call parserp2
        cp -1
        jp z,errorquit
        pop bc;
        or c
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret

op_pop
        push bc; сохраняем операнд
        call readnextword
        call parserp2
        cp -1
        jp z,errorquit
        pop bc;
        or c
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret

op_alu
        push bc; сохраняем операнд
        ld hl,(linepos)
        call parse_number_byte;пытаемся прочитать число
        cp -1
        jp z,op_alu_reg
        ld a,c;прочитанное число, только младший байт
        pop bc
        push af
        ld a,c
        or 0x46; alu операция с числом
        push af
        call printhexln
        pop af
        call pushcode
        pop af
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret
op_alu_reg; alu операция с регистром
        call readnextword
        call parse_rz;пытаемся прочитать регистр
        cp -1
        jp z,errorquit
        ld hl,wordbuf
        pop bc
        or c
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret
        
op_bytes
        ld hl,(linepos)
op_bytes_next
        call skipspaces
        call parse_number_byte;пытаемся прочитать число
        cp -1
        jp z,errorquit
        push hl; сохраняем позицию в строке
        ld a,c; распаршенное число
        push af
        call printhexln
        pop af
        call pushcode
        pop hl
        call skipspaces
        ld a,(hl)
        inc hl
        cp ','
        jr z, op_bytes_next
        xor a
        ret

op_inc
        push bc; сохраняем операнд
        call readnextword
        call parserp
        pop bc;
        cp -1
        jp z,op_inc_reg
        or c
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret
op_inc_reg
        call parse_ry
        cp -1
        jp z,errorquit
        ld c, 0x04; inc reg opcode
        or c
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret

op_dec
        push bc; сохраняем операнд
        call readnextword
        call parserp
        pop bc;
        cp -1
        jp z,op_dec_reg
        or c
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret
op_dec_reg
        call parse_ry
        cp -1
        jp z,errorquit
        ld c, 0x05; dec reg opcode
        or c
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret

op_ret; TODO: поддержка флагов (ret z...)
        ld a, c; 0xC9
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret

op_rst; TODO: поддержка кодов (RST 0 ...)
        ld a, c; 0xC7
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret

op_onebyte; однобайтовые операции
        ld a, c
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret
        
op_twobyte; двухбайтовые операции
        push bc
        ld a, b
        push af
        call printhexln
        pop af
        call pushcode
        pop bc
        ld a, c
        push af
        call printhexln
        pop af
        call pushcode
        xor a
        ret


readnextword
        ld hl,(linepos)
        ld de,wordbuf
        call getword
        ld hl,wordbuf
        ld de,wordbuf
        call uppercase
        ret

parserp2
        ld hl,rp2list ;лист регистровых пар
        ld de,wordbuf
        jp findwordinlist

parserp
        ld hl,rplist ;лист регистровых пар
        ld de,wordbuf
        jp findwordinlist
        
parse_rz
        ld hl,rzlist ;лист регистровых пар
        ld de,wordbuf
        jp findwordinlist

parse_ry
        ld hl,rylist ;лист регистровых пар
        ld de,wordbuf
        jp findwordinlist


;in hl=лист de=word
;out a=значение из листа
findwordinlist
        ld c,(hl); rp2 index
        inc hl
        ld a,c
        cp -1
        ret z;jp z,fail_syntax ;ret z ;jr z,strcpexec_tryrun ;a!=0: no such internal command
        push hl
        call strcp
        pop hl
        jr nz,findwordinlist_fail
        ld a,c ; возвращяем индекс rp2
        ret
findwordinlist_fail
        ld b,-1 ;чтобы точно найти терминатор
        xor a
        cpir ;найдём обязательно
        jr findwordinlist
        ret

;in hl=строка с числом
;out c=число a=-1 если ошибка иначе 0
parse_number_byte
    ld (parse_number_byte_hl),hl
    call parse_hex_byte
    cp -1
    ret nz
    ld hl,(parse_number_byte_hl)
    call parse_char
    cp -1
    ret nz
    ld hl,(parse_number_byte_hl)
    call parse_number
    ret
parse_number_byte_hl dw 0

;in hl=строка с числом
;out bc=число de=колличество прочитанных чисел a=-1 если ошибка иначе 0
parse_number:
    ld bc, 0        ; bc will hold the parsed number
    ld de, 0
parse_loop:
    ld a, (hl)      ; load the next character from the string
    or a            ; check if it's the null terminator
    jr z, parse_done ; if null terminator, we're done

    ; convert character to number
    sub '0'         ; convert ascii '0'-'9' to 0-9
    jr c, parse_done ; if carry is set, it's an invalid character
    cp 10           ; check if the number is greater than 9
    jr nc, parse_done ; if not carry, it's an invalid character

    ; multiply the current number by 10
    push hl
    ld h, b         ; move b to d
    ld l, c         ; move c to e
    add hl, hl      ; multiply hl by 2
    add hl, hl      ; multiply hl by 2 again (total x4)
    add hl, bc      ; add de to hl (total x5)
    add hl, hl      ; multiply hl by 2 (total x10)

    ; add the new digit to the number
    ld b,0
    ld c,a
    add hl, bc      ; add the current number to hl
    ld b, h         ; move the high byte to b
    ld c, l         ; move the low byte to c
    inc e

    ; move to the next character
    pop hl
    inc hl          ; increment hl to point to the next character
    jr parse_loop   ; repeat the loop
parse_done:
    ld a,e; если прочитал 0 цифр - то ошибка
    cp 0
    jr z,parse_error
    xor a
    ret             ; return with the number in bc
parse_error:
    ld a,-1
    ret

;in hl=string
;out c=number, a=-1 если ошибка иначе 0
parse_char
    ld a,(hl)
    inc hl
    cp 0x27; char "'"
    jr nz, parse_char_error
    ld a,(hl)
    inc hl
    ld c,a
    ld a,(hl)
    inc hl
    cp 0x27; char "'"
    jr nz, parse_char_error
    xor a
    ret
parse_char_error
    ld a,-1
    ret

;in hl=string
;out c=number, a=-1 если ошибка иначе 0
parse_hex_byte
    ld a,(hl)
    inc hl
    cp '0'
    jr nz, parse_hex_byte_error
    ld a,(hl)
    inc hl
    cp 'x'
    jr nz, parse_hex_byte_error
    call parsehexnumber
    jr c, parse_hex_byte_error
    ld c,a
    xor a
    ret
parse_hex_byte_error
    ld a,-1
    ret

;in hl=string
;out bc=number, a=-1 если ошибка иначе 0
parse_hex_word
    ld a,(hl)
    inc hl
    cp '0'
    jr nz, parse_hex_word_error
    ld a,(hl)
    inc hl
    cp 'x'
    jr nz, parse_hex_word_error
    call parsehexnumber
    jr c, parse_hex_word_error
    ld b,a
    call parsehexnumber
    jr c, parse_hex_word_error
    ld c,a
    xor a
    ret
parse_hex_word_error
    ld a,-1
    ret


;in hl=string
;out a=number, флаг C если ошибка
parsehexnumber:
    ld a, (hl)      ; load the next character from the string
    inc hl
    call parsehexnumber_char
    jr c, parsehexnumber_error ; if carry is set, it's an invalid character
    rlca
    rlca
    rlca
    rlca
    push af
    ld a,(hl)
    inc hl
    call parsehexnumber_char
    jr c, parsehexnumber_error ; if carry is set, it's an invalid character
    ld c, a
    pop af
    or c
    ret
parsehexnumber_error
    pop af; восстанавилваем стэк
    scf
    ret
parsehexnumber_char
    ; convert ascii character in a to hex digit
    cp '0'          ; check if a is '0' to '9'
    jr c, parsehexnumber_invalid_char ; if less than '0', it's invalid
    cp '9' + 1      ; check if a is greater than '9'
    jr nc, parsehexnumber_char_nextchar ; if less than or equal to '9', it's a digit
    sub '0'         ; convert ascii '0'-'9' to 0-9
    ret
parsehexnumber_char_nextchar
    cp 'A'          ; check if a is 'a' to 'f'
    jr c, parsehexnumber_invalid_char ; if less than 'a', it's invalid
    cp 'F' + 1      ; check if a is greater than 'f'
    jr nc, parsehexnumber_invalid_char ; if less than or equal to 'f', it's a digit
    sub 'A' - 10    ; convert ascii 'a'-'f' to 10-15
    ret
parsehexnumber_invalid_char:
    scf             ; set carry flag to indicate error
    ret


pushcode
        ld de, (pc)
        ld hl, (pc_addr)
        ld (hl), a
        inc hl
        inc de
        ld (pc), de
        ld (pc_addr), hl
        ld hl, (binsize)
        inc hl
        ld (binsize), hl
        ret

operandlist; таблица операндов (код операнда, адрес процедуры, строка операнда)
        dw 0x00C5, op_push
        db "PUSH",0
        dw 0x00C1, op_pop
        db "POP",0
        dw 0x00C7, op_rst
        db "RST",0
        dw 0x00C9, op_ret
        db "RET",0
        dw 0x00A0, op_alu
        db "AND",0
        dw 0x00A8, op_alu
        db "XOR",0
        dw 0x00B0, op_alu
        db "OR",0
        dw 0x00B8, op_alu
        db "CP",0
        dw 0x0003, op_inc
        db "INC",0
        dw 0x000B, op_dec
        db "DEC",0
        dw 0x0000, op_bytes
        db "DB",0
        dw 0x0000, op_onebyte
        db "NOP",0
        dw 0x000F, op_onebyte
        db "RRCA",0
        dw 0x001F, op_onebyte
        db "RRA",0
        dw 0x0007, op_onebyte
        db "RLCA",0
        dw 0x0017, op_onebyte
        db "RLA",0
        dw 0x00D9, op_onebyte
        db "EXX",0
        dw 0x003F, op_onebyte
        db "CCF",0
        dw 0x002F, op_onebyte
        db "CPL",0
        dw 0x0037, op_onebyte
        db "SCF",0
        dw 0x0027, op_onebyte
        db "DAA",0
        dw 0x0076, op_onebyte
        db "HALT",0
        dw 0x00F3, op_onebyte
        db "DI",0
        dw 0x00FB, op_onebyte
        db "EI",0
        dw 0xedA0, op_twobyte
        db "LDI",0
        dw 0xedA1, op_twobyte
        db "CPI",0
        dw 0xedA2, op_twobyte
        db "INI",0
        dw 0xedA3, op_twobyte
        db "OUTI",0
        dw 0xedA8, op_twobyte
        db "LDD",0
        dw 0xedA9, op_twobyte
        db "CPD",0
        dw 0xedAA, op_twobyte
        db "IND",0
        dw 0xedAB, op_twobyte
        db "OUTD",0
        dw 0xedB0, op_twobyte
        db "LDIR",0
        dw 0xedB1, op_twobyte
        db "CPIR",0
        dw 0xedB2, op_twobyte
        db "INIR",0
        dw 0xedB3, op_twobyte
        db "OTIR",0
        dw 0xedB8, op_twobyte
        db "LDDR",0
        dw 0xedB9, op_twobyte
        db "CPDR",0
        dw 0xedBA, op_twobyte
        db "INDR",0
        dw 0xedBB, op_twobyte
        db "OTDR",0
        dw 0xed44, op_twobyte
        db "NEG",0
        dw 0xed45, op_twobyte
        db "RETN",0
        dw 0xed4D, op_twobyte
        db "RETI",0
        dw 0xed67, op_twobyte
        db "RRD",0
        dw 0xed6F, op_twobyte
        db "RLD",0
        dw 0xed76, op_twobyte; TODO ??? дизасемблируется в im 1
        db "SLP",0
        dw -1, -1 ;конец таблицы
        
rp2list; таблица регистровых пар с AF
        db 0x00,"BC",0
        db 0x10,"DE",0
        db 0x20,"HL",0
        db 0x30,"AF",0
        db -1 ;конец таблицы

rplist; таблица регистровых пар с SP
        db 0x00,"BC",0
        db 0x10,"DE",0
        db 0x20,"HL",0
        db 0x30,"SP",0
        db -1 ;конец таблицы

rzlist; таблица регистров для r[z]
        db 0x00,"B",0
        db 0x01,"C",0
        db 0x02,"D",0
        db 0x03,"E",0
        db 0x04,"H",0
        db 0x05,"L",0
        db 0x06,"(HL)",0
        db 0x07,"A",0
        db -1 ;конец таблицы

rylist; таблица регистров для r[y]
        db %00000000,"B",0
        db %00001000,"C",0
        db %00010000,"D",0
        db %00011000,"E",0
        db %00100000,"H",0
        db %00101000,"L",0
        db %00110000,"(HL)",0
        db %00111000,"A",0
        db -1 ;конец таблицы


hello
        db "Hello from assembler",0
asmfile
        db "Assembling file: ",0
binfile
        db "Targer binary file: ",0
newline
        db 0x0d,0x0a,0
errormsg
        db "ERROR!",0
hfile   db 0
hbinfile db 0
pc      dw 0x0100
pc_addr dw binary
binsize dw 0
        
linepos dw 0
wordbuf
        ds 64
line
        ds 255
operand
        ds 64

        include "../_sdk/stdio.asm"
        
binary
        db 0
end
    savebin "a.com",begin,end-begin

    LABELSLIST "../../us/user.l",1
