; VIC-20 emulator for SAM Coupe and ZX Spectrum, by Simon Owen
;
; Version 1.1 (23/8/2008)
;
; WWW: http://simonowen.com/sam/vic20emu/

base:          equ  &a000

spectrum_mode: equ  0               ; 0=SAM, 1=Spectrum

status:        equ  &f9             ; Status and extended keyboard port
lmpr:          equ  &fa             ; Low Memory Page Register
hmpr:          equ  &fb             ; High Memory Page Register
vmpr:          equ  &fc             ; Video Memory Page Register
keyboard:      equ  &fe             ; Keyboard port
border:        equ  &fe             ; Border port
saa_reg:       equ  &01ff           ; SAA 1099 (register select)
ay_reg:        equ  &fffd           ; AY-3-8912 (register select)
kempston:      equ  &1f             ; Kempston joystick (Spectrum)

rom0_off:      equ  %00100000       ; LMPR bit to disable ROM0
vmpr_mode1:    equ  %00000000       ; Mode 1
vmpr_mode2:    equ  %00100000       ; Mode 2

attr_table:    equ  &3400           ; look-up table for attribute address
data_table:    equ  &3c00           ; look-up table for screen address

low_page:      equ  3               ; LMPR during emulation (&10000 in BASIC)
high_page:     equ  1               ; HMPR during emulation (&8000 in BASIC)
screen_page:   equ  low_page+1      ; VMPR during emulation

bord_invalid:  equ  2               ; Invalid instruction (red)

m6502_nmi:     equ  &fffa           ; nmi vector address
m6502_reset:   equ  &fffc           ; reset vector address
m6502_int:     equ  &fffe           ; int vector address (also for BRK)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               org  base
               dump $
               autoexec

start:         di

               in   a,(lmpr)
               ld   d,a
               in   a,(vmpr)
               ld   e,a

               ld   a,low_page+rom0_off
               out  (lmpr),a
IF spectrum_mode
               ld   a,screen_page+vmpr_mode1
ELSE
               ld   a,screen_page+vmpr_mode2
ENDIF
               out  (vmpr),a

               ld   hl,vic_palette+&0f
               ld   bc,&10f8
               otdr

IF spectrum_mode
;              jr   $               ; stop to allow memory exporting
ENDIF
               ld   (old_stack+1),sp
               ld   sp,stack_top
               push de              ; save original LMPR+VMPR

               call setup_im2       ; enable IM 2
               call init_sound

               ei
               call load_state
               call execute_loop    ; GO!
               call save_state

               di
               pop  de              ; restore HMPR
old_stack:     ld   sp,0
               ld   a,d
               out  (lmpr),a
               ld   a,e
               out  (vmpr),a
               im   1
               ei
               ret

IF spectrum_mode
vic_palette:   defb 0,17,34,51,68,85,102,120,0,17,34,59,68,93,110,127
ELSE
vic_palette:   defb 0,127,32,87,49,70,24,104,36,98,47,95,63,101,31,111
ENDIF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Interrupt handling

setup_im2:     ld   hl,im2_table
               ld   a,im2_jp/256
im2_fill:      ld   (hl),a
               inc  l
               jr   nz,im2_fill
               inc  h
               ld   (hl),a          ; complete the final entry
               ld   a,im2_table/256
               ld   i,a
               im   2               ; set interrupt mode 2
               ret

im2_handler:   push af
               push bc
               push de
               push hl
               call read_keyboard
               call read_joystick
               ld   a,&c9           ; RET opcode
               ld   (main_loop),a   ; stop at next instruction
no_int:        pop  hl
               pop  de
               pop  bc
               pop  af
               ei
               reti


               ; key presses are slow to process, to speed up checking which is pressed
key_press:     push de
               ld   h,a     ; save the row state
               push hl
               ld   a,l     ; row number
               add  a,a
               add  a,a
               add  a,a     ; *8
               add  a,c     ; add column offset (*2)
               add  a,a     ; *16
               add  a,keymap\256
               ld   l,a

               ld   a,&7f   ; B N M Sym Space
               in   a,(keyboard)
               bit  1,a
               jr   nz,not_sym_map
               set  7,l     ; sym keymap in upper 128 bytes
not_sym_map:
               ld   h,keymap/256
               ld   d,vic_key_rows/256
               ld   e,(hl)  ; VIC row offset
               bit  7,e     ; shift toggle?
               jr   z,not_shift_tog
               ld   a,(vic_key_rows+4)
               xor  %00000010   ; toggle shift
               ld   (vic_key_rows+4),a
               res  7,e
not_shift_tog: inc  l
               ld   a,(de)
               and  (hl)
               ld   (de),a
               pop  hl
               ld   a,h
               ret

key_b0:        ld   de,key_b0_ret
               ld   c,7
               jp   key_press
key_b1:        ld   de,key_b1_ret
               ld   c,6
               jp   key_press
key_b2:        ld   de,key_b2_ret
               ld   c,5
               jp   key_press
key_b3:        ld   de,key_b3_ret
               ld   c,4
               jp   key_press
key_b4:        ld   de,key_b4_ret
               ld   c,3
               jp   key_press
IF spectrum_mode == 0
key_b5:        ld   de,key_b5_ret
               ld   c,2
               jp   key_press
key_b6:        ld   de,key_b6_ret
               ld   c,1
               jp   key_press
key_b7:        ld   de,key_b7_ret
               ld   c,0
               jp   key_press
ENDIF

read_keyboard: ld   hl,vic_key_rows+7
               ld   a,&ff
               ld   (hl),a
               dec  l
               ld   (hl),a
               dec  l
               ld   (hl),a
               dec  l
               ld   (hl),a
               dec  l
               ld   (hl),a
               dec  l
               ld   (hl),a
               dec  l
               ld   (hl),a
               dec  l           ; L is now zero for below
               ld   (hl),a

               ld   b,&fe
keyrow_loop:
IF spectrum_mode == 0
               ld   c,status
               in   a,(c)       ; read extended SAM keys
               rla
               jr   nc,key_b7
key_b7_ret:    rla
               jr   nc,key_b6
key_b6_ret:    rla
               jr   nc,key_b5
ENDIF
key_b5_ret:    ld   c,keyboard
               in   a,(c)
               rra
               jr   nc,key_b0
key_b0_ret:    rra
               jr   nc,key_b1
key_b1_ret:    rra
               jr   nc,key_b2
key_b2_ret:    rra
               jr   nc,key_b3
key_b3_ret:    rra
               jr   nc,key_b4
key_b4_ret:    inc  l
               rlc  b
               jp   c,keyrow_loop

               ld   h,vic_key_rows/256

               ld   a,&fe   ; V C X Z Shift
               in   a,(keyboard)
               rra
               jr   c,not_shift

IF spectrum_mode == 0
               ld   a,&ef   ; DEL + -
               in   a,(status)
               rla
               rla
               jr   c,not_shift_plus
               ld   l,R4
               set  1,(hl)  ; release shift
               ld   l,R7
               set  5,(hl)  ; release +
               dec  l
               res  6,(hl)  ; press *
not_shift_plus:
               rla
               jr   c,not_shift_minus
               ld   l,R0
               set  5,(hl)  ; release -
               ld   l,R4
               set  1,(hl)  ; release shift
               res  6,(hl)  ; press /
not_shift_minus:
               ld   a,&df
               in   a,(status)
               bit  5,a
               jr   nz,not_shift_equals
               ld   l,R2
               set  6,(hl)  ; release =
               ld   l,R6
               res  6,(hl)  ; press * (_)
not_shift_equals:
ENDIF
               ld   a,&ef   ; 6 7 8 9 0
               in   a,(keyboard)
               rra
               jr   c,not_shift_0
               ld   l,R0
               set  4,(hl)  ; release 0
IF spectrum_mode
               ld   l,R4
               set  1,(hl)  ; release shift
               ld   l,R7
               res  7,(hl)  ; press Del
ELSE
               inc  l
               res  6,(hl)  ; press ^ (~ = PI)
ENDIF
not_shift_0:

IF spectrum_mode == 0
               ld   a,&f7   ; 5 4 3 2 1
               in   a,(keyboard)
               bit  1,a
               jr   nz,not_shift_2
               ld   l,R4
               set  1,(hl)  ; release shift
               ld   l,R0
               set  0,(hl)  ; release 2
               inc  l
               res  5,(hl)  ; press @
not_shift_2:
ENDIF

not_shift:
               ld   a,(&911e)       ; Interrupt Enable Register
               rla                  ; check CA1
               ret  nc              ; return if disabled
               ld   l,R4
               bit  0,(hl)          ; check RUN/STOP
               ret  nz              ; return if not pressed
               bit  1,(hl)          ; check Restore (Shift)
               ret  nz              ; return if not pressed
               ld   a,(&911d)       ; Interrupt Flag Register
               or   %10000010       ; set IRQ and CA1
               ld   (&911d),a
               ld   a,&c9           ; RET opcode
               ld   (main_loop),a
               ld   a,&18           ; JR opcode
               ld   (nmi_patch),a   ; NMI on next instruction
               ret


read_joystick: ld   hl,&911f
               ld   a,&ff
               ld   (hl),a          ; nothing pressed
IF spectrum_mode
               in   a,(kempston)    ; Kempston joystick
               rra
               rra
               jr   c,joy_b1
joy_b1_ret:    rra
               jr   c,joy_b2
joy_b2_ret:    rra
               jr   c,joy_b3
joy_b3_ret:    rra
               ret  nc
joy_b4:        res  5,(hl)          ; fire
               ret
joy_b3:        res  2,(hl)          ; up
               jp   joy_b3_ret
joy_b2:        res  3,(hl)          ; down
               jp   joy_b2_ret
joy_b1:        res  4,(hl)          ; left
               jp   joy_b1_ret
ELSE
               in   a,(keyboard)    ; &FFFE (cursor keys)
               rra
               jr   nc,joy_b0
joy_b0_ret:    rra
               jr   nc,joy_b1
joy_b1_ret:    rra
               jr   nc,joy_b2
joy_b2_ret:    rra
               ret  c
joy_b3:        res  4,(hl)          ; left
               ret
joy_b2:        res  3,(hl)          ; down
               jp   joy_b2_ret
joy_b1:        res  2,(hl)          ; up
               jp   joy_b1_ret
joy_b0:        res  5,(hl)          ; fire
               jp   joy_b0_ret
ENDIF


; IM 2 table must be aligned to 256-byte boundary
               defs -$\256
im2_table:     defs 257

; IM 2 vector must have LSB==MSB
               defs $/256-1
stack_top:                          ; stack in slack space
im2_jp:        jp   im2_handler


; Key tables must be in the same 256-byte page
               defs -$\256

; Normal key map (no modifiers)
keymap:
    defb    R2,&7f, R4,&7f, R3,&7f,  R4,&f7, R3,&fb, R4,&fb, R3,&fd, R4,&fd  ; F3 F2 F1  V C X Z Shift
    defb    R5,&7f, R1,&7f, S5,&7f,  R5,&f7, R2,&fb, R5,&fb, R2,&fd, R5,&fd  ; F6 F5 F4  G F D S A
    defb    R0,&bf, S4,&7f, R0,&7f,  R1,&fb, R6,&fb, R1,&fd, R6,&fd, R1,&fe  ; F9 F8 F7  T R E W Q
    defb    RX,&ff, R5,&fe, R4,&fe,  R7,&fb, R0,&fd, R7,&fd, R0,&fe, R7,&fe  ; Caps Tab Esc  5 4 3 2 1
    defb    R7,&7f, R7,&df, R0,&df,  R0,&fb, R7,&f7, R0,&f7, R7,&ef, R0,&ef  ; DEL + -  6 7 8 9 0
    defb    RX,&ff, S0,&fe, R2,&bf,  R6,&f7, R1,&f7, R6,&ef, R1,&ef, R6,&df  ; F0 " =  Y U I O P
    defb    R3,&bf, R2,&df, R5,&bf,  R2,&f7, R5,&ef, R2,&ef, R5,&df, R6,&7f  ; Edit : ;  H J K L Return
    defb    R2,&fe, R3,&df, R4,&df,  R3,&f7, R4,&ef, R3,&ef, RX,&ff, R3,&fe  ; Inv . ,  B N M Sym Space

; Key map with Symbol pressed
IF spectrum_mode
symmap:
    defb    RX,&ff, RX,&ff, RX,&ff,  R4,&bf, S4,&bf, R7,&bf, R2,&df, R4,&fd  ; - - -  V C X Z Shift
    defb    RX,&ff, RX,&ff, RX,&ff,  RX,&ff, RX,&ff, RX,&ff, R0,&df, S1,&bf  ; - - -  G F D S A
    defb    RX,&ff, RX,&ff, RX,&ff,  S3,&df, S4,&df, RX,&ff, RX,&ff, RX,&ff  ; - - -  T R E W Q
    defb    RX,&ff, RX,&ff, RX,&ff,  S7,&fb, S0,&fd, S7,&fd, R1,&df, S7,&fe  ; - - -  5 4 3 2 1
    defb    RX,&ff, RX,&ff, RX,&ff,  S0,&fb, S7,&f7, S0,&f7, S7,&ef, S6,&bf  ; - - -  6 7 8 9 0
    defb    RX,&ff, RX,&ff, RX,&ff,  S2,&df, S5,&bf, RX,&ff, R5,&bf, S0,&fe  ; - - -  Y U I O P
    defb    RX,&ff, RX,&ff, RX,&ff,  R1,&bf, R0,&df, R7,&df, R2,&bf, R2,&fe  ; - - -  H J K L Return
    defb    RX,&ff, RX,&ff, RX,&ff,  R6,&bf, R4,&df, R3,&df, RX,&ff, R4,&fe  ; - - -  B N M Sym Space
ELSE
    defb    RX,&ff, RX,&ff, RX,&ff,  RX,&ff, RX,&ff, S4,&bf, RX,&ff, RX,&ff  ; F3 F2 F1  V C X Z Shift
    defb    RX,&ff, RX,&ff, RX,&ff,  RX,&ff, RX,&ff, RX,&ff, RX,&ff, RX,&ff  ; F6 F5 F4  G F D S A
    defb    RX,&ff, RX,&ff, RX,&ff,  S5,&bf, S2,&df, RX,&ff, S3,&df, S4,&df  ; F9 F8 F7  T R E W Q
    defb    RX,&ff, RX,&ff, RX,&ff,  R1,&7f, S2,&7f, R2,&7f, S3,&7f, R3,&7f  ; Caps Tab Esc  5 4 3 2 1
    defb    RX,&ff, RX,&ff, RX,&ff,  S1,&7f, R0,&7f, S0,&7f, S0,&df, RX,&ff  ; DEL + -  6 7 8 9 0
    defb    RX,&ff, RX,&ff, RX,&ff,  RX,&ff, RX,&ff, RX,&ff, RX,&ff, RX,&ff  ; F0 " =  Y U I O P
    defb    RX,&ff, RX,&ff, RX,&ff,  R1,&bf, RX,&ff, RX,&ff, R7,&bf, RX,&ff  ; Edit : ;  H J K L Return
    defb    RX,&ff, RX,&ff, RX,&ff,  RX,&ff, RX,&ff, RX,&ff, RX,&ff, RX,&ff  ; Inv . ,  B N M Sym Space
ENDIF

;               defs -$\256      ; We should already be aligned, but just in case

vic_key_rows:  defs 9            ; 8 key rows + 1 scratch entry

; LSB of key row to apply mask to
R0:            equ  vic_key_rows\256
R1:            equ  R0+1
R2:            equ  R0+2
R3:            equ  R0+3
R4:            equ  R0+4
R5:            equ  R0+5
R6:            equ  R0+6
R7:            equ  R0+7
RX:            equ  R0+8    ; dead keys

; Same as above, but toggles VIC shift state
S0:            equ  R0+&80
S1:            equ  R1+&80
S2:            equ  R2+&80
S3:            equ  R3+&80
S4:            equ  R4+&80
S5:            equ  R5+&80
S6:            equ  R6+&80
S7:            equ  R7+&80

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 65C02 emulation

execute_loop:  ld   a,&1a           ; LD A,(DE)
               ld   (main_loop),a   ; restore start of loop
               call main_loop

IF spectrum_mode == 0
               ld   a,&df
               in   a,(status)      ; read extended keys
               rla                  ; check F0
               ret  nc              ; exit to BASIC if pressed
ENDIF
               ld   a,(de)
               cp   &db             ; STP? (CPU halted)
               jr   z,execute_loop
               cp   &cb             ; WAI? (waiting for interrupt)
               jr   nz,not_wai
               inc  de              ; wait satisfied
not_wai:

nmi_patch:     ld   a,nmi_handler-$-2   ; patched with JR for NMI

               exx
               bit  2,d             ; check I
               exx
               jr   nz,execute_loop ; maskable interrupts disabled

               ld   a,d
               cp   &ea             ; around screen scrolling?
               jr   nz,not_scroll
               ld   a,(vic_key_rows+6)
               rla                  ; return pressed?
               jr   nc,execute_loop ; skip interrupt
not_scroll:
               ld   hl,(m6502_int)  ; fetch interrupt handler
do_interrupt:  ld   a,d
               exx
               ld   (hl),a          ; push PCH
               dec  l
               exx
               ld   a,e
               exx
               ld   (hl),a          ; push PCL
               dec  l
               res  4,d             ; clear B
               res  3,d             ; clear D [65C02]
               exx
               call i_php           ; push P
               exx
               set  2,d             ; set I (disable interrupts)
               exx
               ex   de,hl
               jr   execute_loop

nmi_handler:   ld   a,&3e           ; LD A,nn opcode
               ld   (nmi_patch),a
               ld   hl,(m6502_nmi)  ; fetch NMI handler
               jp   do_interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

i_undoc_3:     inc  de              ; 3-byte NOP
i_undoc_2:     inc  de              ; 2-byte NOP
i_undoc_1:     ld   a,bord_invalid
               out  (border),a
               jp   (ix)

read_write_loop:
write_loop:    ld   a,h
               cp   &1e
               jr   nc,write_trap

main_loop:     ld   a,(de)          ; 7/7/15  - fetch opcode
               inc  de              ; 6/7/11  - PC++
               ld   l,a             ; 4/6/6   - LSB is opcode
               ld   h,msb_table/256 ; 7/7/15  - look-up table
               ld   h,(hl)          ; 7/8/16  - opcode MSB
               jp   (hl)            ; 4/5/9   - execute!
                                    ; = 35T (official) / 40T (off-screen) / 72T (on-screen)

write_trap:    cp   &20
               jr   nc,not_scrdata

               ; &1e00-1fff write (character RAM)
               push de
               ld   a,(hl)
               set  5,h
               ld   e,(hl)
               res  1,h
               ld   d,(hl)
               add  a,a
               ld   l,a
               ld   a,0
charset_div4:  adc  a,&20
               ld   h,a
               add  hl,hl
               add  hl,hl
               bit  5,h
               jr   z,chr_nowrap
               ld   a,h
               xor  %10100000       ; wrap to &8000
               ld   h,a
chr_nowrap:
IF spectrum_mode
               ld   a,(hl)
               ld   (de),a
               inc  l
               inc  d
               ld   a,(hl)
               ld   (de),a
               inc  l
               inc  d
               ld   a,(hl)
               ld   (de),a
               inc  l
               inc  d
               ld   a,(hl)
               ld   (de),a
               inc  l
               inc  d
               ld   a,(hl)
               ld   (de),a
               inc  l
               inc  d
               ld   a,(hl)
               ld   (de),a
               inc  l
               inc  d
               ld   a,(hl)
               ld   (de),a
               inc  l
               inc  d
               ld   a,(hl)
               ld   (de),a
ELSE
               ex   de,hl
               push bc
               ld   bc,32
               ld   a,(de)
               ld   (hl),a
               add  hl,bc
               inc  e
               ld   a,(de)
               ld   (hl),a
               add  hl,bc
               inc  e
               ld   a,(de)
               ld   (hl),a
               add  hl,bc
               inc  e
               ld   a,(de)
               ld   (hl),a
               add  hl,bc
               inc  e
               ld   a,(de)
               ld   (hl),a
               add  hl,bc
               inc  e
               ld   a,(de)
               ld   (hl),a
               add  hl,bc
               inc  e
               ld   a,(de)
               ld   (hl),a
               add  hl,bc
               inc  e
               ld   a,(de)
               ld   (hl),a
               pop  bc
ENDIF
off_scrdata:   pop  de
               jp   (ix)

not_scrdata:   cp   &96
               jr   c,not_scrattr
               cp   &98
               jr   nc,main_loop

               ; &9600-97ff write (colour RAM)
write_attr:    push de
               ld   a,(hl)
               and  %00000111
IF spectrum_mode
               ld   e,a
               ld   d,zx_colours/256
               ld   a,(de)
ENDIF
               res  7,h
               set  5,h
               ld   e,(hl)
               res  1,h
               ld   h,(hl)
               ld   l,e
attr_style:    jp   attr_normal

attr_normal:   ld   e,a
               ld   a,(bg_col)
               or   e
IF spectrum_mode
               ld   (hl),a
ELSE
               ld   de,32
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
ENDIF
off_scrattr:   pop  de
               jp   (ix)

attr_reverse:  add  a,a
               add  a,a
               add  a,a
               ld   e,a
               ld   a,(fg_col)
               or   e
IF spectrum_mode
               ld   (hl),a
ELSE
               ld   de,32
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
               add  hl,de
               ld   (hl),a
ENDIF
               pop  de
               jp   (ix)

bg_col:        defb 0
fg_col:        defb 0


bad_write:     ld   (&2000),a       ; make $2000 appear unavailable to give 8K RAMTOP
               jp   (ix)

not_scrattr:   sub  &90
               jr   z,write_vic
               dec  a
               jr   nz,bad_write

               ; &91xx write (VIA)
               ld   a,l
               cp   &20
               jr   z,write_9120
               cp   &22
               jr   z,write_9122
               jp   (ix)

write_9120:    push bc
               ld   c,(hl)
               ld   b,8
               ld   a,&ff
               ld   hl,vic_key_rows
vic_key_loop:  rl   c
               jp   c,no_vic_press
               and  (hl)
no_vic_press:  inc  l
               djnz vic_key_loop
               ld   (&9121),a      ; port A
               ld   (&912f),a      ; port A
               pop  bc
               jp   (ix)

write_9122:    bit  7,(hl)
               jr   nz,not_joyb7
IF spectrum_mode
               in   a,(kempston)
               rra
               ld   a,&ff
               jr   nc,no_fire
ELSE
               ld   a,&ff
               in   a,(keyboard)
               bit  4,a             ; Right pressed?
               ld   a,&ff
               jr   nz,no_fire
ENDIF
               ld   a,&7f
no_fire:       ld   l,&20
               ld   (hl),a
not_joyb7:     jp   (ix)


               ; &90xx write (VIC)
write_vic:     ld   a,l
               cp   &05
               jr   z,write_9005
               cp   &0f
               jp   z,write_900f
               cp   &02
               jp   z,write_9002_3
               cp   &03
               jp   z,write_9002_3
               cp   &0a
               jp   z,write_900a
               cp   &0b
               jp   z,write_900b
               cp   &0c
               jp   z,write_900c
               cp   &0d
               jp   z,write_900d
               cp   &0e
               jp   z,write_900e
               jp   (ix)

write_9002_3:  call build_tables
data_redraw:   ld   hl,&1e00        ; screen data
               ld   a,&02           ; 512 bytes
               call refresh_mem     ; redraw
               ld   hl,&900f
               jp   write_900f

write_9005:    ld   a,(hl)
               and  %00000111
               bit  0,a
               jr   nz,chr_block0
               or   %00100000       ; block 4 (&8000)
chr_block0:    ld   (charset_div4+1),a
               ld   hl,&1e00        ; screen data
               ld   a,&02           ; 512 bytes
               call refresh_mem     ; redraw
               jp   (ix)

write_900f:    push bc
               push de
               push hl

               ld   a,(hl)          ; b7-4=bkg colour, b3=non-reverse attrs, b2-0=border
               ld   b,a
               and  %11110000
               rra
IF spectrum_mode
               rra
               rra
               rra
               ld   l,a
               ld   h,zx_colours/256
               ld   a,(hl)          ; map to ZX colour
               ld   d,a
               and  %00000111       ; ink
               ld   e,a
               ld   a,d
               rla
               rla
               rla                  ; bright+paper
ELSE
               ld   d,a
               rra
               rra
               rra
               and  %00000111       ; ink
               ld   e,a
               ld   a,d
ENDIF
               ld   (bg_col),a      ; background for normal attrs
               and  %01000000       ; bright bit
               or   e               ; merge ink
               ld   (fg_col),a      ; foreground for reverse attrs

               ld   a,b
               and  %00000111       ; border colour
IF spectrum_mode
               ld   l,a
               ld   a,(hl)          ; map to ZX colour (never bright)
ENDIF
               out  (border),a

               ld   d,a             ; save ink
               add  a,a
               add  a,a
               add  a,a             ; move to paper position
               or   d               ; ink+paper the same
               ld   d,a

               bit  3,b             ; normal or reverse?
               ld   hl,attr_normal
               jr   nz,non_reverse
               ld   hl,attr_reverse
non_reverse:   ld   (attr_style+1),hl

IF spectrum_mode
               ld   hl,&4000+&1800  ; mode 1 attributes
ELSE
               ld   hl,&4000+&2000  ; mode 2 attributes
ENDIF

               call get_rowscols
               ld   a,d
               inc  b
               dec  b
               jr   z,bord_fill
               inc  c
               dec  c
               jr   z,bord_fill

IF spectrum_mode
ELSE
               sla  c
               sla  c
               sla  c
ENDIF
               ld   l,b
               ld   a,&20
               sub  b
               ld   b,a
               ld   a,d
               ld   d,0
               ld   e,l

bord_row_loop: push bc
bord_col_loop: ld   (hl),a
               inc  hl
               djnz bord_col_loop
               pop  bc
               add  hl,de
               dec  c
               jp   nz,bord_row_loop
               sbc  hl,de

bord_fill:     ld   e,a
IF spectrum_mode
               ld   a,&5b           ; mode 1 MSB after end of attrs
ELSE
               ld   a,&78           ; mode 2 MSB after end of attrs
ENDIF
bord_end_loop: ld   (hl),e
               inc  l
               jp   nz,bord_end_loop
               inc  h
               cp   h
               jp   nz,bord_end_loop

               pop  hl
               pop  de
               pop  bc

               call refresh_attr
               jp   (ix)


write_900A:    push bc              ; bass voice
IF spectrum_mode
               ld   l,(hl)
               ld   bc,ay_reg
               ld   a,&07
               out  (c),a
               res  6,b
               ld   a,(voice_enable)
               or   %00000001       ; disable tone A
               bit  7,l             ; VIC voice disabled?
               jr   z,bass_off
               and  %11111110       ; enable tone A
bass_off:      ld   (voice_enable),a
               out  (c),a
               set  6,b
               ld   h,sound_table/256
               ld   a,(hl)
               res  7,l
               ld   h,(hl)
               ld   l,a
               add  hl,hl
               add  hl,hl           ; bass is 2 octaves below soprano
               ld   a,&00
               out  (c),a
               res  6,b
               out  (c),l
               set  6,b
               inc  a
               out  (c),a
               res  6,b
               out  (c),h
ELSE
               ld   bc,saa_reg
               ld   a,&14
               out  (c),a
               dec  b
               ld   l,(hl)
               ld   a,l
               rlca
               and  %00000001
               ld   h,a
               ld   a,(tone_enable)
               and  %11111110
               or   h
               ld   (tone_enable),a
               out  (c),a
               inc  b

               ld   a,&10
               out  (c),a
               dec  b
               ld   h,sound_table/256
               ld   a,(oct_ab)
               and  %11110000
               or   (hl)            ; table holds bass octave
               ld   (oct_ab),a
               out  (c),a
               inc  b

               ld   a,&08
               out  (c),a
               dec  b
               res  7,l
               ld   a,(hl)
               out  (c),a
ENDIF
               pop  bc
               jp   (ix)

write_900B:    push bc              ; alto voice
IF spectrum_mode
               ld   l,(hl)
               ld   bc,ay_reg
               ld   a,&07
               out  (c),a
               res  6,b
               ld   a,(voice_enable)
               or   %00000010       ; disable tone B
               bit  7,l             ; VIC voice disabled?
               jr   z,alto_off
               and  %11111101       ; enable tone B
alto_off:      ld   (voice_enable),a
               out  (c),a
               set  6,b
               ld   h,sound_table/256
               ld   a,(hl)
               res  7,l
               ld   h,(hl)
               ld   l,a
               add  hl,hl           ; alto is 1 octave below soprano
               ld   a,&02
               out  (c),a
               res  6,b
               out  (c),l
               set  6,b
               inc  a
               out  (c),a
               res  6,b
               out  (c),h
ELSE
               ld   bc,saa_reg
               ld   a,&14
               out  (c),a
               dec  b
               ld   l,(hl)
               ld   a,l
               rlca
               rlca
               and  %00000010
               ld   h,a
               ld   a,(tone_enable)
               and  %11111101
               or   h
               ld   (tone_enable),a
               out  (c),a
               inc  b

               ld   a,&10
               out  (c),a
               dec  b
               ld   h,sound_table/256
               ld   a,(hl)
               inc  a               ; alto is 1 octave above bass
               cp   &08
               jr   c,alto_ok
               ld   a,&07
alto_ok:       add  a,a
               add  a,a
               add  a,a
               add  a,a
               ld   b,a
               ld   a,(oct_ab)
               and  %00001111
               or   b
               ld   (oct_ab),a
               ld   b,0             ; restore SAA data select
               out  (c),a
               inc  b

               ld   a,&09
               out  (c),a
               dec  b
               res  7,l
               ld   a,(hl)
               out  (c),a
ENDIF
               pop  bc
               jp   (ix)

write_900C:    push bc              ; soprano voice
IF spectrum_mode
               ld   l,(hl)
               ld   bc,ay_reg
               ld   a,&07
               out  (c),a
               res  6,b
               ld   a,(voice_enable)
               or   %00000100       ; disable tone C
               bit  7,l             ; VIC voice disabled?
               jr   z,soprano_off
               and  %11111011       ; enable tone C
soprano_off:   ld   (voice_enable),a
               out  (c),a
               set  6,b
               ld   h,sound_table/256
               ld   a,(hl)
               res  7,l
               ld   h,(hl)
               ld   l,a
               ld   a,&04
               out  (c),a
               res  6,b
               out  (c),l
               set  6,b
               inc  a
               out  (c),a
               res  6,b
               out  (c),h
ELSE
               ld   bc,saa_reg
               ld   a,&14
               out  (c),a
               dec  b
               ld   l,(hl)
               ld   a,l
               rlca
               rlca
               rlca
               and  %00000100
               ld   h,a
               ld   a,(tone_enable)
               and  %11111011
               or   h
               ld   (tone_enable),a
               out  (c),a
               inc  b

               ld   a,&11
               out  (c),a
               dec  b
               ld   h,sound_table/256
               ld   a,(hl)
               add  a,2             ; soprano is 2 octaves above bass
               cp   &08
               jr   c,soprano_ok
               ld   a,&07
soprano_ok:    ld   b,a
               ld   a,(oct_cd)
               and  %11110000
               or   b
               ld   (oct_cd),a
               ld   b,0             ; restore SAA data select
               out  (c),a
               inc  b

               ld   a,&0a
               out  (c),a
               dec  b
               res  7,l
               ld   a,(hl)
               out  (c),a
ENDIF
               pop  bc
               jp   (ix)

write_900D:    push bc              ; noise register
IF spectrum_mode
               ld   l,(hl)
               ld   bc,ay_reg
               ld   a,&07
               out  (c),a
               res  6,b
               ld   a,(voice_enable)
               or   %00001000       ; disable noise A
               bit  7,l             ; VIC voice disabled?
               jr   z,noise_off
               and  %11110111       ; enable noise A
noise_off:     ld   (voice_enable),a
               out  (c),a
               set  6,b
               ld   a,l
               cpl
               rra
               rra
               rra
               and  %00001111       ; noise freq only 4 bits :-(
               ld   h,&06
               out  (c),h
               res  6,b
               out  (c),a
ELSE
               ld   bc,saa_reg
               ld   a,&15
               out  (c),a
               dec  b
               ld   l,(hl)
               ld   a,l
               rlca
               rlca
               rlca
               rlca
               and  %00001000
               out  (c),a
               inc  b

               ld   a,&11
               out  (c),a
               ld   h,sound_table/256
               ld   a,(hl)
               add  a,1             ; this should be +3, but sounds better as +1
               cp   &08
               jr   c,noise_ok
               ld   a,&07
noise_ok:      add  a,a
               add  a,a
               add  a,a
               add  a,a
               ld   b,a
               ld   a,(oct_cd)
               and  %00001111
               or   b
               ld   (oct_cd),a
               ld   b,0
               out  (c),a
               inc  b

               ld   a,&0b
               out  (c),a
               dec  b
               res  7,l
               ld   a,(hl)
               out  (c),a
ENDIF
               pop  bc
               jp   (ix)

write_900E:    push bc
               ld   a,(hl)          ; sound volume
               and  %00001111
IF spectrum_mode
               ld   bc,ay_reg
               ld   h,10
vol_loop:      out  (c),h
               res  6,b
               out  (c),a
               set  6,b
               dec  h
               bit  3,h
               jr   nz,vol_loop
ELSE
               ld   l,a
               add  a,a
               add  a,a
               add  a,a
               add  a,a             ; move to high nibble
               or   l               ; left+right channels
               ld   h,0
               ld   bc,saa_reg
vol_loop:      out  (c),h           ; select channel 'h' volume
               dec  b
               out  (c),a           ; volume value from above
               inc  b
               inc  h
               bit  2,h
               jp   z,vol_loop      ; loop for channels 0-3
ENDIF
               pop  bc
               jp   (ix)


init_sound:
IF spectrum_mode
               ld   bc,ay_reg

               ld   a,&07
               out  (c),a
               res  6,b
               ld   a,&3f
               out  (c),a
               ld   (voice_enable),a
               ret
ENDIF
               ld   bc,saa_reg

               ld  a,28
               out (c),a
               ld  a,b
               dec b
               out (c),a
               inc b

               ld   a,20
               out  (c),a
               dec  b
               out  (c),b
               inc  b
               inc  a
               out  (c),a
               dec  b
               out  (c),b
               inc  b

               ld   a,22
               out  (c),a
               dec  b
               ld   a,&30
               out  (c),a
               ret

IF spectrum_mode
voice_enable:  defb &3f     ; noise/tone enable (inverted)
ELSE
tone_enable:   defb 0       ; tone enable bit mask
oct_ab:        defb 0       ; octaves for voices 0+1
oct_cd:        defb 0       ; octaves for voices 2+3
ENDIF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Call the memory write handlers for HL, length B*256
refresh_mem:   push bc
               push de
               push ix
               ld   b,a
               ex   de,hl
               ld   ix,refresh_ret
refresh_loop:  ld   h,d
               ld   l,e
               ld   a,h
               jp   write_trap
refresh_ret:   inc  e
               jp   nz,refresh_loop
               inc  d
               djnz refresh_loop
               pop  ix
               pop  de
               pop  bc
               ret

; Refresh the display attributes from VIC colour RAM
refresh_attr:  push de
               push ix
               ld   de,&9600
               ld   ix,attr_ret
attr_loop:     ld   h,d
               ld   l,e
               jp   write_attr
attr_ret:      inc  e
               jp   nz,attr_loop
               inc  d
               bit  3,d
               jp   z,attr_loop
               pop  ix
               pop  de
               ret

; Rebuild the display look-up tables after row/column change
build_tables:  push bc
               push de
               ld   hl,data_table
               ld   de,&4000
               call get_rowscols
               xor  a
               cp   b
               jr   z,unmap_fill
               cp   c
               jr   z,unmap_fill

row_loop:      push bc
               push de
IF spectrum_mode
               ld   a,d             ; data MSB
               rra
               rra
               rra
               and  %00000011
               or   &58             ; now attr MSB
ELSE
               ld   a,d
               set  5,a             ; move to attr file
ENDIF
col_loop:      ld   (hl),d          ; data MSB
               res  3,h             ; switch to attr MSB table
               ld   (hl),a          ; attr MSB
               set  1,h             ; switch to attr LSB table
               ld   (hl),e          ; data LSB
               set  3,h             ; switch to data LSB table
               ld   (hl),e          ; attr LSB
               res  1,h             ; switch to data MSB table
               inc  e
               inc  hl
               bit  1,h             ; mapping table full?
               jr   nz,table_full
               djnz col_loop
               pop  de
IF spectrum_mode
               ld   a,e
               add  a,32            ; use 64 for double-height
               ld   e,a
               jr   nz,same_third
               ld   a,d
               add  a,8
               ld   d,a
same_third:
ELSE
               inc  d
;              inc  d               ; use extra INC D for double-height
ENDIF
               pop  bc
               dec  c
               jr   nz,row_loop

unmap_fill:    ld   a,&20           ; scratch space at &20xx
               ld   d,h
               ld   e,l
               res  3,d             ; switch to attr MSB table
unmap_loop:    bit  1,h
               jr   nz,done_build
               ld   (hl),a
               inc  hl
               ld   (de),a
               inc  de
               jp   unmap_loop
table_full:    pop  de
               pop  bc
done_build:    pop  de
               pop  bc
               ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

load_state:    ld   hl,&2000
               ld   de,&9000
               ld   bc,&800
               ldir                 ; restore I/O area and colour RAM
               ld   hl,&9000
               ld   a,2
               call refresh_mem     ; activate I/O changes

               ld   a,(reg_a)
               ld   b,a             ; set A
               ld   a,(reg_x)
               ld   iyh,a           ; set X to IYh
               ld   a,(reg_y)
               ld   iyl,a           ; set Y to IYl
               exx
               ld   a,(reg_s)
               ld   l,a             ; set S
               ld   h,&01           ; MSB for stack pointer
               ld   a,(reg_p)
               ld   c,a             ; keep safe
               and  %00001100       ; keep D and I
               or   %00110000       ; force T and B
               ld   d,a             ; set P
               ld   a,c
               and  %01000000       ; keep V
               ld   e,a             ; set V
               ld   a,c
               rra                  ; carry from C
               ex   af,af'          ; set carry
               ld   a,c
               and  %10000010       ; keep N Z
               xor  %00000010       ; zero for Z
               exx
               ld   c,a             ; set N Z
               ld   de,(reg_pc)     ; set PC
               ld   ix,main_loop    ; decode loop
               ret

save_state:    ld   a,b             ; get A
               ld   (reg_a),a
               ld   a,iyh           ; get X from IYh
               ld   (reg_x),a
               ld   a,iyl           ; get Y from IYl
               ld   (reg_y),a
               ex   af,af'          ; carry
               inc  c
               dec  c               ; set N Z
               push af              ; save flags
               ex   af,af'          ; protect carry
               exx
               pop  bc
               ld   a,c
               and  %10000001       ; keep Z80 N and C
               bit  6,c             ; check Z80 Z
               jr   z,save_nz
               or   %00000010       ; set Z
save_nz:       or   e               ; merge V
               or   d               ; merge T B D I
               ld   (reg_p),a
               ld   a,l             ; get S
               ld   (reg_s),a
               exx
               ld   (reg_pc),de
               ld   hl,&9000
               ld   de,&2000
               ld   bc,&800
               ldir
               ret

get_rowscols:  ld   a,(&9002)       ; b6-0 = screen columns
               and  %01111111
               cp   28
               jr   c,no_col_clip
               ld   a,27
no_col_clip:   ld   b,a
               ld   a,(&9003)       ; b6-1 = screen rows
               rra
               and  %00111111
               cp   &24
               jr   c,no_row_clip
               ld   a,23
no_row_clip:   ld   c,a
               ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

               defs -$\256

msb_table:     defb op_00>>8, op_01>>8, op_02>>8, op_03>>8, op_04>>8, op_05>>8, op_06>>8, op_07>>8
               defb op_08>>8, op_09>>8, op_0a>>8, op_0b>>8, op_0c>>8, op_0d>>8, op_0e>>8, op_0f>>8
               defb op_10>>8, op_11>>8, op_12>>8, op_13>>8, op_14>>8, op_15>>8, op_16>>8, op_17>>8
               defb op_18>>8, op_19>>8, op_1a>>8, op_1b>>8, op_1c>>8, op_1d>>8, op_1e>>8, op_1f>>8
               defb op_20>>8, op_21>>8, op_22>>8, op_23>>8, op_24>>8, op_25>>8, op_26>>8, op_27>>8
               defb op_28>>8, op_29>>8, op_2a>>8, op_2b>>8, op_2c>>8, op_2d>>8, op_2e>>8, op_2f>>8
               defb op_30>>8, op_31>>8, op_32>>8, op_33>>8, op_34>>8, op_35>>8, op_36>>8, op_37>>8
               defb op_38>>8, op_39>>8, op_3a>>8, op_3b>>8, op_3c>>8, op_3d>>8, op_3e>>8, op_3f>>8
               defb op_40>>8, op_41>>8, op_42>>8, op_43>>8, op_44>>8, op_45>>8, op_46>>8, op_47>>8
               defb op_48>>8, op_49>>8, op_4a>>8, op_4b>>8, op_4c>>8, op_4d>>8, op_4e>>8, op_4f>>8
               defb op_50>>8, op_51>>8, op_52>>8, op_53>>8, op_54>>8, op_55>>8, op_56>>8, op_57>>8
               defb op_58>>8, op_59>>8, op_5a>>8, op_5b>>8, op_5c>>8, op_5d>>8, op_5e>>8, op_5f>>8
               defb op_60>>8, op_61>>8, op_62>>8, op_63>>8, op_64>>8, op_65>>8, op_66>>8, op_67>>8
               defb op_68>>8, op_69>>8, op_6a>>8, op_6b>>8, op_6c>>8, op_6d>>8, op_6e>>8, op_6f>>8
               defb op_70>>8, op_71>>8, op_72>>8, op_73>>8, op_74>>8, op_75>>8, op_76>>8, op_77>>8
               defb op_78>>8, op_79>>8, op_7a>>8, op_7b>>8, op_7c>>8, op_7d>>8, op_7e>>8, op_7f>>8
               defb op_80>>8, op_81>>8, op_82>>8, op_83>>8, op_84>>8, op_85>>8, op_86>>8, op_87>>8
               defb op_88>>8, op_89>>8, op_8a>>8, op_8b>>8, op_8c>>8, op_8d>>8, op_8e>>8, op_8f>>8
               defb op_90>>8, op_91>>8, op_92>>8, op_93>>8, op_94>>8, op_95>>8, op_96>>8, op_97>>8
               defb op_98>>8, op_99>>8, op_9a>>8, op_9b>>8, op_9c>>8, op_9d>>8, op_9e>>8, op_9f>>8
               defb op_a0>>8, op_a1>>8, op_a2>>8, op_a3>>8, op_a4>>8, op_a5>>8, op_a6>>8, op_a7>>8
               defb op_a8>>8, op_a9>>8, op_aa>>8, op_ab>>8, op_ac>>8, op_ad>>8, op_ae>>8, op_af>>8
               defb op_b0>>8, op_b1>>8, op_b2>>8, op_b3>>8, op_b4>>8, op_b5>>8, op_b6>>8, op_b7>>8
               defb op_b8>>8, op_b9>>8, op_ba>>8, op_bb>>8, op_bc>>8, op_bd>>8, op_be>>8, op_bf>>8
               defb op_c0>>8, op_c1>>8, op_c2>>8, op_c3>>8, op_c4>>8, op_c5>>8, op_c6>>8, op_c7>>8
               defb op_c8>>8, op_c9>>8, op_ca>>8, op_cb>>8, op_cc>>8, op_cd>>8, op_ce>>8, op_cf>>8
               defb op_d0>>8, op_d1>>8, op_d2>>8, op_d3>>8, op_d4>>8, op_d5>>8, op_d6>>8, op_d7>>8
               defb op_d8>>8, op_d9>>8, op_da>>8, op_db>>8, op_dc>>8, op_dd>>8, op_de>>8, op_df>>8
               defb op_e0>>8, op_e1>>8, op_e2>>8, op_e3>>8, op_e4>>8, op_e5>>8, op_e6>>8, op_e7>>8
               defb op_e8>>8, op_e9>>8, op_ea>>8, op_eb>>8, op_ec>>8, op_ed>>8, op_ee>>8, op_ef>>8
               defb op_f0>>8, op_f1>>8, op_f2>>8, op_f3>>8, op_f4>>8, op_f5>>8, op_f6>>8, op_f7>>8
               defb op_f8>>8, op_f9>>8, op_fa>>8, op_fb>>8, op_fc>>8, op_fd>>8, op_fe>>8, op_ff>>8


IF spectrum_mode
sound_table:   MDAT "ay38912.dat"
ELSE
sound_table:   MDAT "saa1099.dat"
ENDIF

               defs -$\256

IF spectrum_mode
               ; VIC to Spectrum colours, with bright in b3
zx_colours:    defb &0,&7,&2,&5,&3,&4,&1,&6,&6,&E,&A,&D,&B,&C,&9,&E
ENDIF

end:           equ  $
end_len:       equ  end-start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Instruction implementations
INC "opimpl.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Snapshot registers follow 8K RAM and 2K I/O + colour RAM
               org &2800
               dump low_page,$

reg_a:         defb &00
reg_p:         defb &04     ; interrupts disabled
reg_x:         defb &00
reg_y:         defb &00
reg_s:         defb &00
reg_pc:        defw &fd22   ; reset routine (from &fffc in kernal ROM)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; VIC-20 ROMs

               dump &8000
               MDAT "chargen"

               dump &c000
               MDAT "basic2"

               dump &e000
               MDAT "kernal"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Test snapshots

               dump low_page,&0000
;              MDAT "gridrunner.v"
;              MDAT "blitzkreig.v"
;              MDAT "deflex.v"
