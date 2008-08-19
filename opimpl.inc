; 65C02 instruction implementations

INC "opdefs.inc"

               org  op_00
               dump $
i_brk:         inc  de              ; return to BRK+2
               ld   a,d             ; PCH
               exx
               ld   (hl),a          ; push return PCH
               dec  l               ; S--
               exx
               ld   a,e             ; PCL
               exx
               ld   (hl),a          ; push return PCL
               dec  l               ; S--
               ex   af,af'          ; carry
               inc  c
               dec  c               ; set N Z
               push af              ; save flags
               ex   af,af'          ; save carry
               pop  bc
               ld   a,c
               and  %10000001       ; keep N and C
               bit  6,c             ; check Z
               jr   z,brk_nz
               or   %00000010       ; set Z
brk_nz:        or   e               ; merge V
               or   d               ; merge T D I
               or   %00010000       ; set B
               ld   (hl),a          ; push P
               dec  l               ; S--
               exx
               ld   de,(m6502_int)  ; fetch interrupt handler
               jp   (ix) ; main_loop
op_00_len:     equ  $-op_00



               org  op_01
               dump $
i_ora_ix:      ld   a,(de)          ; ORA ($xx,X)
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   a,b             ; A
               or   (hl)            ; A|x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_01_len:     equ  $-op_01


               org  op_02
               dump $
               jp   i_undoc_1
op_02_len:     equ  $-op_02


               org  op_03
               dump $
               jp   i_undoc_2
op_03_len:     equ  $-op_03


               org  op_04
               dump $
i_tsb_z:       ld   a,(de)          ; TSB $nn [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   c,(hl)          ; x
               ld   a,c
               or   b               ; x|A
               ld   (hl),a          ; set mem
               ld   a,c
               and  b               ; x&A
               ld   c,a             ; set Z
               jp   (ix) ; zwrite_loop
op_04_len:     equ  $-op_04



               org  op_05
               dump $
i_ora_z:       ld   a,(de)          ; ORA $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   a,b             ; A
               or   (hl)            ; A|x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_05_len:     equ  $-op_05



               org  op_06
               dump $
i_asl_z:       ld   a,(de)          ; ASL $nn
               inc  de
               ld   l,a
               ld   h,0
               ex   af,af'
               sla  (hl)            ; x << 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_06_len:     equ  $-op_06



               org  op_07
               dump $
i_rmb_0:       ld   a,(de)          ; RMB0 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               res  0,(hl)
               jp   (ix) ; zwrite_loop
op_07_len:     equ  $-op_07



               org  op_08
               dump $
i_php:         ex   af,af'          ; carry
               inc  c
               dec  c               ; set N Z
               push af              ; save flags
               ex   af,af'          ; protect carry
               exx
               pop  bc
               ld   a,c
               and  %10000001       ; keep Z80 N and C
               bit  6,c             ; check Z80 Z
               jr   z,php_nz
               or   %00000010       ; set Z
php_nz:        or   e               ; merge V
               or   d               ; merge T D I
               ld   (hl),a
               dec  l               ; S--
               exx
               jp   (ix) ; main_loop
op_08_len:     equ  $-op_08



               org  op_09
               dump $
i_ora_i:       ld   h,d
               ld   l,e
               inc  de
               ld   a,b             ; A
               or   (hl)            ; A|x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_09_len:     equ  $-op_09



               org  op_0a
               dump $
i_asl_acc:     ex   af,af'
               sla  b               ; A << 1
               ld   c,b             ; set N Z
               ex   af,af'          ; set carry
               jp   (ix) ; main_loop
op_0a_len:     equ  $-op_0a



               org  op_0b
               dump $
               jp   i_undoc_2
op_0b_len:     equ  $-op_0b



               org  op_0c
               dump $
i_tsb_a:       ex   de,hl           ; TSB $nnnn [65C02]
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   c,(hl)          ; x
               ld   a,c
               or   b               ; x|A
               ld   (hl),a          ; set mem
               ld   a,c
               and  b               ; x&A
               ld   c,a             ; set Z
               jp   write_loop
op_0c_len:     equ  $-op_0c



               org  op_0d
               dump $
i_ora_a:       ex   de,hl           ; ORA $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,b             ; A
               or   (hl)            ; A|x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_0d_len:     equ  $-op_0d



               org  op_0e
               dump $
i_asl_a:       ex   de,hl           ; ASL $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ex   af,af'
               sla  (hl)            ; x << 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_0e_len:     equ  $-op_0e



               org  op_0f
               dump $
i_bbr_0:       ld   a,(de)          ; BBR0 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  0,(hl)
               jp   z,i_bra         ; ToDo: read after branch
               inc  de
               jp   (ix) ; zread_loop
op_0f_len:     equ  $-op_0f



               org  op_10
               dump $
i_bpl:         inc  c
               dec  c
               jp   p,i_bra         ; branch if plus
               inc  de
               jp   (ix) ; main_loop
op_10_len:     equ  $-op_10



               org  op_11
               dump $
i_ora_iy:      ld   a,(de)          ; ORA ($nn),Y
               inc  de
               ld   l,a
               ld   h,0
               ld   a,iyl           ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ld   a,b             ; A
               or   (hl)            ; A|x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_11_len:     equ  $-op_11



               org  op_12
               dump $
i_ora_iz:      ld   a,(de)          ; indirect zero-page [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   a,b             ; A
               or   (hl)            ; A|x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_12_len:     equ  $-op_12



               org  op_13
               dump $
               jp   i_undoc_2
op_13_len:     equ  $-op_13



               org  op_14
               dump $
i_trb_z:       ld   a,(de)          ; TRB $nn [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   c,(hl)          ; x
               ld   a,b             ; A
               cpl                  ; ~A
               and  b               ; x&~A
               ld   (hl),a          ; set mem
               ld   a,c
               and  b               ; x&A
               ld   c,a             ; set Z (N lost)
               jp   (ix) ; zwrite_loop
op_14_len:     equ  $-op_14



               org  op_15
               dump $
i_ora_zx:      ld   a,(de)          ; ORA $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,b             ; A
               or   (hl)            ; A|x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_15_len:     equ  $-op_15



               org  op_16
               dump $
i_asl_zx:      ld   a,(de)          ; ASL $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ex   af,af'
               sla  (hl)            ; x << 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_16_len:     equ  $-op_16



               org  op_17
               dump $
i_rmb_1:       ld   a,(de)          ; RMB1 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               res  1,(hl)
               jp   (ix) ; zwrite_loop
op_17_len:     equ  $-op_17



               org  op_18
               dump $
i_clc:         and  a               ; clear C
               ex   af,af'          ; save carry
               jp   (ix) ; main_loop
op_18_len:     equ  $-op_18



               org  op_19
               dump $
i_ora_ay:      ld   a,(de)          ; ORA $nnnn,Y
               inc  de
               add  a,iyl           ; add Y
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   a,b             ; A
               or   (hl)            ; A|x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_19_len:     equ  $-op_19



               org  op_1a
               dump $
i_inc_acc:     inc  b               ; A++ [65C02]
               ld   c,b             ; set N Z
               jp   (ix) ; main_loop
op_1a_len:     equ  $-op_1a



               org  op_1b
               dump $
               jp   i_undoc_3
op_1b_len:     equ  $-op_1b



               org  op_1c
               dump $
i_trb_a:       ex   de,hl           ; TRB $nnnn [65C02]
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   c,(hl)          ; x
               ld   a,b             ; A
               cpl                  ; ~A
               and  b               ; x&~A
               ld   (hl),a          ; set mem
               ld   a,c
               and  b               ; x&A
               ld   c,a             ; set Z (N lost)
               jp   write_loop
op_1c_len:     equ  $-op_1c



               org  op_1d
               dump $
i_ora_ax:      ld   a,(de)          ; ORA $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   a,b             ; A
               or   (hl)            ; A|x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_1d_len:     equ  $-op_1d



               org  op_1e
               dump $
i_asl_ax:      ld   a,(de)          ; ASL $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ex   af,af'
               sla  (hl)            ; x << 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_1e_len:     equ  $-op_1e



               org  op_1f
               dump $
i_bbr_1:       ld   a,(de)          ; BBR1 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  1,(hl)
               jp   z,i_bra
               inc  de
               jp   (ix) ; main_loop
op_1f_len:     equ  $-op_1f



               org  op_20
               dump $
i_jsr:         ex   de,hl           ; JSR nn
               ld   e,(hl)          ; subroutine low
               inc  hl              ; only 1 inc - we push ret-1
               ld   d,(hl)          ; subroutine high
               ld   a,h             ; PCh
               exx
               ld   (hl),a          ; push ret-1 high byte
               dec  l               ; S--
               exx
               ld   a,l             ; PCl
               exx
               ld   (hl),a          ; push ret-1 low byte
               dec  l               ; S--
               exx
               jp   (ix) ; main_loop
op_20_len:     equ  $-op_20



               org  op_21
               dump $
i_and_ix:      ld   a,(de)          ; AND ($xx,X)
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   a,b             ; A
               and  (hl)            ; A&x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_21_len:     equ  $-op_21



               org  op_22
               dump $
               jp   i_undoc_1
op_22_len:     equ  $-op_22


               org  op_23
               dump $
               jp   i_undoc_2
op_23_len:     equ  $-op_23



               org  op_24
               dump $
i_bit_z:       ld   a,(de)          ; BIT $nn
               inc  de
               ld   l,a
               ld   h,0
               jp   i_bit
op_24_len:     equ  $-op_24



               org  op_25
               dump $
i_and_z:       ld   a,(de)          ; AND $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   a,b             ; A
               and  (hl)            ; A&x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_25_len:     equ  $-op_25



               org  op_26
               dump $
i_rol_z:       ld   a,(de)          ; ROL $nn
               inc  de
               ld   l,a
               ld   h,0
               ex   af,af'
               rl   (hl)            ; x << 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_26_len:     equ  $-op_26



               org  op_27
               dump $
i_rmb_2:       ld   a,(de)          ; RMB2 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               res  2,(hl)
               jp   (ix) ; zwrite_loop
op_27_len:     equ  $-op_27



               org  op_28
               dump $
i_plp:         exx
               inc  l               ; S++
               ld   a,(hl)          ; pop P
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
               jp   (ix) ; main_loop
op_28_len:     equ  $-op_28



               org  op_29
               dump $
i_and_i:       ld   h,d
               ld   l,e
               inc  de
               ld   a,b             ; A
               and  (hl)            ; A&x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_29_len:     equ  $-op_29



               org  op_2a
               dump $
i_rol_acc:     ex   af,af'
               rl   b
               ld   c,b             ; set N Z
               ex   af,af'          ; set carry
               jp   (ix) ; main_loop
op_2a_len:     equ  $-op_2a


               org  op_2b
               dump $
               jp   i_undoc_2
op_2b_len:     equ  $-op_2b



               org  op_2c
               dump $
i_bit_a:       ex   de,hl           ; BIT $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               jp   i_bit
op_2c_len:     equ  $-op_2c



               org  op_2d
               dump $
i_and_a:       ex   de,hl           ; AND $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,b             ; A
               and  (hl)            ; A&x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_2d_len:     equ  $-op_2d



               org  op_2e
               dump $
i_rol_a:       ex   de,hl           ; ROL $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ex   af,af'
               rl   (hl)            ; x << 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_2e_len:     equ  $-op_2e



               org  op_2f
               dump $
i_bbr_2:       ld   a,(de)          ; BBR2 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  2,(hl)
               jp   z,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_2f_len:     equ  $-op_2f



               org  op_30
               dump $
i_bmi:         inc  c
               dec  c
               jp   m,i_bra         ; branch if minus
               inc  de
               jp   (ix) ; main_loop
op_30_len:     equ  $-op_30



               org  op_31
               dump $
i_and_iy:      ld   a,(de)          ; AND ($nn),Y
               inc  de
               ld   l,a
               ld   h,0
               ld   a,iyl           ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ld   a,b             ; A
               and  (hl)            ; A&x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_31_len:     equ  $-op_31



               org  op_32
               dump $
i_and_iz:      ld   a,(de)          ; indirect zero-page [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   a,b             ; A
               and  (hl)            ; A&x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_32_len:     equ  $-op_32



               org  op_33
               dump $
               jp   i_undoc_2
op_33_len:     equ  $-op_33



               org  op_34
               dump $
i_bit_zx:      ld   a,(de)          ; BIT $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               jp   i_bit
op_34_len:     equ  $-op_34



               org  op_35
               dump $
i_and_zx:      ld   a,(de)          ; AND $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,b             ; A
               and  (hl)            ; A&x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_35_len:     equ  $-op_35



               org  op_36
               dump $
i_rol_zx:      ld   a,(de)          ; ROL $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ex   af,af'
               rl   (hl)            ; x << 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_36_len:     equ  $-op_36



               org  op_37
               dump $
i_rmb_3:       ld   a,(de)          ; RMB3 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               res  3,(hl)
               jp   (ix) ; zwrite_loop
op_37_len:     equ  $-op_37



               org  op_38
               dump $
i_sec:         scf                  ; set C
               ex   af,af'          ; save carry
               jp   (ix) ; main_loop
op_38_len:     equ  $-op_38



               org  op_39
               dump $
i_and_ay:      ld   a,(de)          ; AND $nnnn,Y
               inc  de
               add  a,iyl           ; add Y
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   a,b             ; A
               and  (hl)            ; A&x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_39_len:     equ  $-op_39



               org  op_3a
               dump $
i_dec_acc:     dec  b               ; A-- [65C02]
               ld   c,b             ; set N Z
               jp   (ix) ; main_loop
op_3a_len:     equ  $-op_3a



               org  op_3b
               dump $
               jp   i_undoc_3
op_3b_len:     equ  $-op_3b



               org  op_3c
               dump $
i_bit_ax:      ld   a,(de)          ; BIT $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               jp   i_bit
op_3c_len:     equ  $-op_3c



               org  op_3d
               dump $
i_and_ax:      ld   a,(de)          ; AND $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   a,b             ; A
               and  (hl)            ; A&x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_3d_len:     equ  $-op_3d



               org  op_3e
               dump $
i_rol_ax:      ld   a,(de)          ; ROL $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ex   af,af'
               rl   (hl)            ; x << 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_3e_len:     equ  $-op_3e



               org  op_3f
               dump $
i_bbr_3:       ld   a,(de)          ; BBR3 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  3,(hl)
               jp   z,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_3f_len:     equ  $-op_3f



               org  op_40
               dump $
i_rti:         exx
               inc  l               ; S++
               ld   a,(hl)          ; pop P
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
               exx
               inc  l               ; S++
               ld   a,(hl)          ; pop return LSB
               exx
               ld   e,a             ; PCL
               exx
               inc  l               ; S++
               ld   a,(hl)          ; pop return MSB
               exx
               ld   d,a             ; PCH
               jp   (ix) ; main_loop
op_40_len:     equ  $-op_40



               org  op_41
               dump $
i_eor_ix:      ld   a,(de)          ; EOR ($xx,X)
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   a,b             ; A
               xor  (hl)            ; A^x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_41_len:     equ  $-op_41



               org  op_42
               dump $
               jp   i_undoc_1
op_42_len:     equ  $-op_42



               org  op_43
               dump $
               jp   i_undoc_2
op_43_len:     equ  $-op_43



               org  op_44
               dump $
               jp   i_undoc_2
op_44_len:     equ  $-op_44



               org  op_45
               dump $
i_eor_z:       ld   a,(de)          ; EOR $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   a,b             ; A
               xor  (hl)            ; A^x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_45_len:     equ  $-op_45



               org  op_46
               dump $
i_lsr_z:       ld   a,(de)          ; LSR $nn
               inc  de
               ld   l,a
               ld   h,0
               ex   af,af'
               srl  (hl)            ; x >> 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_46_len:     equ  $-op_46



               org  op_47
               dump $
i_rmb_4:       ld   a,(de)          ; RMB4 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               res  4,(hl)
               jp   (ix) ; zwrite_loop
op_47_len:     equ  $-op_47



               org  op_48
               dump $
i_pha:         ld   a,b             ; A
               exx
               ld   (hl),a          ; push A
               dec  l               ; S--
               exx
               jp   (ix) ; main_loop
op_48_len:     equ  $-op_48



               org  op_49
               dump $
i_eor_i:       ld   h,d
               ld   l,e
               inc  de
               ld   a,b             ; A
               xor  (hl)            ; A^x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_49_len:     equ  $-op_49



               org  op_4a
               dump $
i_lsr_acc:     ex   af,af'
               srl  b               ; A >> 1
               ld   c,b             ; set N Z
               ex   af,af'          ; set carry
               jp   (ix) ; main_loop
op_4a_len:     equ  $-op_4a



               org  op_4b
               dump $
               jp   i_undoc_2
op_4b_len:     equ  $-op_4b



               org  op_4c
               dump $
i_jmp_a:       ex   de,hl           ; JMP nn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               jp   (ix) ; main_loop
op_4c_len:     equ  $-op_4c



               org  op_4d
               dump $
i_eor_a:       ex   de,hl           ; EOR $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,b             ; A
               xor  (hl)            ; A^x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_4d_len:     equ  $-op_4d



               org  op_4e
               dump $
i_lsr_a:       ex   de,hl           ; LSR $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ex   af,af'
               srl  (hl)            ; x >> 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_4e_len:     equ  $-op_4e



               org  op_4f
               dump $
i_bbr_4:       ld   a,(de)          ; BBR4 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  4,(hl)
               jp   z,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_4f_len:     equ  $-op_4f



               org  op_50
               dump $
i_bvc:         exx
               bit  6,e
               exx
               jp   z,i_bra         ; branch if V clear
               inc  de
               jp   (ix) ; main_loop
op_50_len:     equ  $-op_50



               org  op_51
               dump $
i_eor_iy:      ld   a,(de)          ; EOR ($nn),Y
               inc  de
               ld   l,a
               ld   h,0
               ld   a,iyl           ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ld   a,b             ; A
               xor  (hl)            ; A^x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_51_len:     equ  $-op_51



               org  op_52
               dump $
i_eor_iz:      ld   a,(de)          ; indirect zero-page [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   a,b             ; A
               xor  (hl)            ; A^x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_52_len:     equ  $-op_52



               org  op_53
               dump $
               jp   i_undoc_2
op_53_len:     equ  $-op_53



               org  op_54
               dump $
               jp   i_undoc_2
op_54_len:     equ  $-op_54



               org  op_55
               dump $
i_eor_zx:      ld   a,(de)          ; EOR $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,b             ; A
               xor  (hl)            ; A^x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_55_len:     equ  $-op_55



               org  op_56
               dump $
i_lsr_zx:      ld   a,(de)          ; LSR $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ex   af,af'
               srl  (hl)            ; x >> 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_56_len:     equ  $-op_56



               org  op_57
               dump $
i_rmb_5:       ld   a,(de)          ; RMB5 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               res  5,(hl)
               jp   (ix) ; zwrite_loop
op_57_len:     equ  $-op_57



               org  op_58
               dump $
i_cli:         exx
               res  2,d             ; clear I (enable interrupts)
               exx
               jp   (ix) ; main_loop
op_58_len:     equ  $-op_58



               org  op_59
               dump $
i_eor_ay:      ld   a,(de)          ; EOR $nnnn,Y
               inc  de
               add  a,iyl           ; add Y
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   a,b             ; A
               xor  (hl)            ; A^x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_59_len:     equ  $-op_59



               org  op_5a
               dump $
i_phy:         ld   a,iyl           ; PHY [65C02]
               exx
               ld   (hl),a          ; push Y
               dec  l               ; S--
               exx
               jp   (ix) ; main_loop
op_5a_len:     equ  $-op_5a



               org  op_5b
               dump $
               jp   i_undoc_3
op_5b_len:     equ  $-op_5b



               org  op_5c
               dump $
               jp   i_undoc_3
op_5c_len:     equ  $-op_5c



               org  op_5d
               dump $
i_eor_ax:      ld   a,(de)          ; EOR $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   a,b             ; A
               xor  (hl)            ; A^x
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; read_loop
op_5d_len:     equ  $-op_5d



               org  op_5e
               dump $
i_lsr_ax:      ld   a,(de)          ; LSR $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ex   af,af'
               srl  (hl)            ; x >> 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_5e_len:     equ  $-op_5e



               org  op_5f
               dump $
i_bbr_5:       ld   a,(de)          ; BBR5 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  5,(hl)
               jp   z,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_5f_len:     equ  $-op_5f



               org  op_60
               dump $
i_rts:         exx                  ; RTS
               inc  l               ; S++
               ld   a,(hl)          ; PC LSB
               exx
               ld   e,a
               exx
               inc  l               ; S++
               ld   a,(hl)          ; PC MSB
               exx
               ld   d,a
               inc  de              ; PC++ (strange but true)
               jp   (ix) ; main_loop
op_60_len:     equ  $-op_60



               org  op_61
               dump $
i_adc_ix:      ld   a,(de)          ; ADX ($xx,X)
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               jp   i_adc
op_61_len:     equ  $-op_61



               org  op_62
               dump $
               jp   i_undoc_1
op_62_len:     equ  $-op_62



               org  op_63
               dump $
               jp   i_undoc_2
op_63_len:     equ  $-op_63



               org  op_64
               dump $
i_stz_z:       ld   a,(de)          ; STZ $nn [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   (hl),h
               jp   (ix) ; zwrite_loop
op_64_len:     equ  $-op_64



               org  op_65
               dump $
i_adc_z:       ld   a,(de)          ; ADC $nn
               inc  de
               ld   l,a
               ld   h,0
               jp   i_adc
op_65_len:     equ  $-op_65



               org  op_66
               dump $
i_ror_z:       ld   a,(de)          ; ROR $nn
               inc  de
               ld   l,a
               ld   h,0
               ex   af,af'
               rr   (hl)            ; x >> 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_66_len:     equ  $-op_66



               org  op_67
               dump $
i_rmb_6:       ld   a,(de)          ; RMB6 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               res  6,(hl)
               jp   (ix) ; zwrite_loop
op_67_len:     equ  $-op_67



               org  op_68
               dump $
i_pla:         exx
               inc  l               ; S++
               ld   a,(hl)          ; pop A
               exx
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               jp   (ix) ; main_loop
op_68_len:     equ  $-op_68



               org  op_69
               dump $
i_adc_i:       ld   h,d
               ld   l,e
               inc  de
i_adc:         ex   af,af'          ; carry
               ld   a,b             ; A
               adc  a,(hl)          ; A+M+C
adc_daa:       nop
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               exx
               jp   pe,adcsbc_v
               ld   e,%00000000
               exx
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
adcsbc_v:      ld   e,%01000000
               exx
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_69_len:     equ  $-op_69



               org  op_6a
               dump $
i_ror_acc:     ex   af,af'
               rr   b
               ld   c,b             ; set N Z
               ex   af,af'          ; set carry
               jp   (ix) ; main_loop
op_6a_len:     equ  $-op_6a



               org  op_6b
               dump $
               jp   i_undoc_2
op_6b_len:     equ  $-op_6b



               org  op_6c
               dump $
i_jmp_i:       ex   de,hl           ; JMP (nn)
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               ex   de,hl
               ld   e,(hl)
               inc  l               ; 6502 bug wraps within page, *OR*
;              inc  hl              ; 65C02 spans pages correctly
               ld   d,(hl)
               jp   (ix) ; main_loop
op_6c_len:     equ  $-op_6c



               org  op_6d
               dump $
i_adc_a:       ex   de,hl           ; ADC $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               jp   i_adc
op_6d_len:     equ  $-op_6d



               org  op_6e
               dump $
i_ror_a:       ex   de,hl           ; ROR $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ex   af,af'
               rr   (hl)            ; x >> 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_6e_len:     equ  $-op_6e



               org  op_6f
               dump $
i_bbr_6:       ld   a,(de)          ; BBR6 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  6,(hl)
               jp   z,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_6f_len:     equ  $-op_6f



               org  op_70
               dump $
i_bvs:         exx
               bit  6,e
               exx
               jp   nz,i_bra        ; branch if V set
               inc  de
               jp   (ix) ; main_loop
op_70_len:     equ  $-op_70



               org  op_71
               dump $
i_adc_iy:      ld   a,(de)          ; ADC ($nn),Y
               inc  de
               ld   l,a
               ld   h,0
               ld   a,iyl           ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               jp   i_adc
op_71_len:     equ  $-op_71



               org  op_72
               dump $
i_adc_iz:      ld   a,(de)          ; indirect zero-page [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               jp   i_adc
op_72_len:     equ  $-op_72



               org  op_73
               dump $
               jp   i_undoc_2
op_73_len:     equ  $-op_73



               org  op_74
               dump $
i_stz_zx:      ld   a,(de)          ; STZ $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   (hl),h
               jp   (ix) ; zwrite_loop
op_74_len:     equ  $-op_74



               org  op_75
               dump $
i_adc_zx:      ld   a,(de)          ; ADC $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               jp   i_adc
op_75_len:     equ  $-op_75



               org  op_76
               dump $
i_ror_zx:      ld   a,(de)          ; ROR $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ex   af,af'
               rr   (hl)            ; x >> 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_76_len:     equ  $-op_76



               org  op_77
               dump $
i_rmb_7:       ld   a,(de)          ; RMB7 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               res  7,(hl)
               jp   (ix) ; zwrite_loop
op_77_len:     equ  $-op_77



               org  op_78
               dump $
i_sei:         exx
               set  2,d             ; set I (disable interrupts)
               exx
               jp   (ix) ; main_loop
op_78_len:     equ  $-op_78



               org  op_79
               dump $
i_adc_ay:      ld   a,(de)          ; ADC $nnnn,Y
               inc  de
               add  a,iyl           ; add Y
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               jp   i_adc
op_79_len:     equ  $-op_79



               org  op_7a
               dump $
i_ply:         exx                  ; PLY [65C02]
               inc  l               ; S++
               ld   a,(hl)          ; pop
               exx
               ld   iyl,a           ; set Y
               ld   c,a             ; set N Z
               jp   (ix) ; main_loop
op_7a_len:     equ  $-op_7a



               org  op_7b
               dump $
               jp   i_undoc_3
op_7b_len:     equ  $-op_7b



               org  op_7c
               dump $
i_jmp_ax:      ld   a,(de)          ; JMP (nn,X) [65C02]
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0             ; carry spans page
               ld   h,a
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               jp   (ix) ; main_loop
op_7c_len:     equ  $-op_7c



               org  op_7d
               dump $
i_adc_ax:      ld   a,(de)          ; ADC $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               jp   i_adc
op_7d_len:     equ  $-op_7d



               org  op_7e
               dump $
i_ror_ax:      ld   a,(de)          ; ROR $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ex   af,af'
               rr   (hl)            ; x >> 1
               ld   c,(hl)          ; set N Z
               ex   af,af'          ; set carry
               jp   write_loop
op_7e_len:     equ  $-op_7e



               org  op_7f
               dump $
i_bbr_7:       ld   a,(de)          ; BBR7 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  7,(hl)
               jp   z,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_7f_len:     equ  $-op_7f



               org  op_80-1
               dump $
i_bra_ex:      ex   af,af'          ; preseve C before branch
;              org  op_80
;              dump $
i_bra:         ld   a,(de)          ; BRA [65C02]
               inc  de
               ld   l,a             ; offset low
               rla                  ; set carry with sign
               sbc  a,a             ; form high byte for offset
               ld   h,a
               add  hl,de           ; PC=PC+e
               ex   de,hl
               jp   (ix) ; main_loop
op_80_len:     equ  $-op_80



               org  op_81
               dump $
i_sta_ix:      ld   a,(de)          ; STA ($xx,X)
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   (hl),b          ; store A
               jp   (ix) ; zwrite_loop
op_81_len:     equ  $-op_81



               org  op_82
               dump $
               jp   i_undoc_2
op_82_len:     equ  $-op_82



               org  op_83
               dump $
               jp   i_undoc_2
op_83_len:     equ  $-op_83



               org  op_84
               dump $
i_sty_z:       ld   a,(de)          ; STY $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   a,iyl           ; Y
               ld   (hl),a
               jp   (ix) ; zwrite_loop
op_84_len:     equ  $-op_84



               org  op_85
               dump $
i_sta_z:       ld   a,(de)          ; STA $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   (hl),b          ; store A
               jp   (ix) ; zwrite_loop
op_85_len:     equ  $-op_85



               org  op_86
               dump $
i_stx_z:       ld   a,(de)          ; STX $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   a,iyh           ; X
               ld   (hl),a
               jp   (ix) ; zwrite_loop
op_86_len:     equ  $-op_86



               org  op_87
               dump $
i_smb_0:       ld   a,(de)          ; SMB0 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               set  0,(hl)
               jp   (ix) ; zwrite_loop
op_87_len:     equ  $-op_87



               org  op_88
               dump $
i_dey:         dec  iyl             ; Y--
               ld   c,iyl           ; set N Z
               jp   (ix) ; main_loop
op_88_len:     equ  $-op_88



               org  op_89
               dump $
i_bit_i:       ld   h,d             ; BIT #$nn
               ld   l,e
               inc  de
i_bit:         ld   c,(hl)          ; set N
               ld   a,c
               and  %01000000       ; V flag from bit 6 of x
               exx
               ld   e,a             ; set V
               exx
               ld   a,(de)          ; next instruction
               and  %11011111
               cp   &d0             ; BNE or BEQ next?
               jr   z,bit_z
               jp   (ix) ; main_loop ; use N flag
bit_z:         ld   a,b             ; A
               and  c               ; perform BIT test
               ld   c,a             ; set Z
               jp   (ix) ; read_loop
op_89_len:     equ  $-op_89



               org  op_8a
               dump $
i_txa:         ld   b,iyh           ; A=X
               ld   c,b             ; set N Z
               jp   (ix) ; main_loop
op_8a_len:     equ  $-op_8a



               org  op_8b
               dump $
               jp   i_undoc_2
op_8b_len:     equ  $-op_8b



               org  op_8c
               dump $
i_sty_a:       ex   de,hl           ; STY $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,iyl           ; Y
               ld   (hl),a
               jp   write_loop
op_8c_len:     equ  $-op_8c



               org  op_8d
               dump $
i_sta_a:       ex   de,hl           ; STA $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   (hl),b          ; store A
               jp   write_loop
op_8d_len:     equ  $-op_8d



               org  op_8e
               dump $
i_stx_a:       ex   de,hl           ; STX $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   a,iyh           ; X
               ld   (hl),a
               jp   write_loop
op_8e_len:     equ  $-op_8e



               org  op_8f
               dump $
i_bbs_0:       ld   a,(de)          ; BBS0 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  0,(hl)
               jp   nz,i_bra            ; ToDo: read after branch
               inc  de
               jp   (ix) ; zread_loop
op_8f_len:     equ  $-op_8f



               org  op_90
               dump $
i_bcc:         ex   af,af'
               jp   nc,i_bra_ex     ; branch if C clear
               ex   af,af'
               inc  de
               jp   (ix) ; main_loop
op_90_len:     equ  $-op_90



               org  op_91
               dump $
i_sta_iy:      ld   a,(de)          ; STA ($nn),Y
               inc  de
               ld   l,a
               ld   h,0
               ld   a,iyl           ; Y
               add  a,(hl)
               inc  l
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ld   (hl),b          ; store A
               jp   write_loop
op_91_len:     equ  $-op_91



               org  op_92
               dump $
i_sta_iz:      ld   a,(de)          ; STA ($nn) [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   (hl),b          ; store A
               jp   write_loop
op_92_len:     equ  $-op_92



               org  op_93
               dump $
               jp   i_undoc_2
op_93_len:     equ  $-op_93



               org  op_94
               dump $
i_sty_zx:      ld   a,(de)          ; STY $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,iyl           ; Y
               ld   (hl),a
               jp   (ix) ; zwrite_loop
op_94_len:     equ  $-op_94



               org  op_95
               dump $
i_sta_zx:      ld   a,(de)          ; STA $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   (hl),b          ; store A
               jp   (ix) ; zwrite_loop
op_95_len:     equ  $-op_95



               org  op_96
               dump $
i_stx_zy:      ld   a,(de)          ; STX $nn,Y
               inc  de
               add  a,iyl           ; add Y (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,iyh           ; X
               ld   (hl),a
               jp   (ix) ; zwrite_loop
op_96_len:     equ  $-op_96



               org  op_97
               dump $
i_smb_1:       ld   a,(de)          ; SMB1 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               set  1,(hl)
               jp   (ix) ; zwrite_loop
op_97_len:     equ  $-op_97



               org  op_98
               dump $
i_tya:         ld   b,iyl           ; A=Y
               ld   c,b             ; set N Z
               jp   (ix) ; main_loop
op_98_len:     equ  $-op_98



               org  op_99
               dump $
i_sta_ay:      ld   a,(de)          ; STA $nnnn,Y
               inc  de
               add  a,iyl           ; add Y
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   (hl),b          ; store A
               jp   write_loop
op_99_len:     equ  $-op_99



               org  op_9a
               dump $
i_txs:         ld   a,iyh           ; X
               exx
               ld   l,a             ; set S (no flags set)
               exx
               jp   (ix) ; main_loop
op_9a_len:     equ  $-op_9a



               org  op_9b
               dump $
               jp   i_undoc_2
op_9b_len:     equ  $-op_9b



               org  op_9c
               dump $
i_stz_a:       ex   de,hl           ; STZ $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   (hl),0
               jp   write_loop
op_9c_len:     equ  $-op_9c



               org  op_9d
               dump $
i_sta_ax:      ld   a,(de)          ; STA $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   (hl),b          ; store A
               jp   write_loop
op_9d_len:     equ  $-op_9d



               org  op_9e
               dump $
i_stz_ax:      ld   a,(de)          ; STZ $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   (hl),0
               jp   write_loop
op_9e_len:     equ  $-op_9e



               org  op_9f
               dump $
i_bbs_1:       ld   a,(de)          ; BBS1 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  1,(hl)
               jp   nz,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_9f_len:     equ  $-op_9f



               org  op_a0
               dump $
i_ldy_i:       ld   a,(de)          ; LDY #$nn
               inc  de
               ld   c,a             ; set N Z
               ld   iyl,c           ; set Y
               jp   (ix) ; main_loop
op_a0_len:     equ  $-op_a0



               org  op_a1
               dump $
i_lda_ix:      ld   a,(de)          ; LDA ($nn,X)
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   b,(hl)          ; set A
               ld   c,b             ; set N Z
               jp   (ix) ; zread_loop
op_a1_len:     equ  $-op_a1



               org  op_a2
               dump $
i_ldx_i:       ld   a,(de)          ; LDX #$nn
               inc  de
               ld   iyh,a           ; set X
               ld   c,a             ; set N Z
               jp   (ix) ; main_loop
op_a2_len:     equ  $-op_a2



               org  op_a3
               dump $
               jp   i_undoc_2
op_a3_len:     equ  $-op_a3



               org  op_a4
               dump $
i_ldy_z:       ld   a,(de)          ; LDY $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   c,(hl)          ; set N Z
               ld   iyl,c           ; set Y
               jp   (ix) ; zread_loop
op_a4_len:     equ  $-op_a4



               org  op_a5
               dump $
i_lda_z:       ld   a,(de)          ; LDA $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   b,(hl)          ; set A
               ld   c,b             ; set N Z
               jp   (ix) ; zread_loop
op_a5_len:     equ  $-op_a5



               org  op_a6
               dump $
i_ldx_z:       ld   a,(de)          ; LDX $nn
               inc  de
               ld   l,a
               ld   h,0
               ld   c,(hl)          ; set N Z
               ld   iyh,c           ; set X
               jp   (ix) ; zread_loop
op_a6_len:     equ  $-op_a6



               org  op_a7
               dump $
i_smb_2:       ld   a,(de)          ; SMB2 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               set  2,(hl)
               jp   (ix) ; zwrite_loop
op_a7_len:     equ  $-op_a7



               org  op_a8
               dump $
i_tay:         ld   iyl,b           ; Y=A
               ld   c,b             ; set N Z
               jp   (ix) ; main_loop
op_a8_len:     equ  $-op_a8



               org  op_a9
               dump $
i_lda_i:       ld   a,(de)          ; LDA #$nn
               inc  de
               ld   b,a             ; set A
               ld   c,b             ; set N Z
               jp   (ix) ; main_loop
op_a9_len:     equ  $-op_a9



               org  op_aa
               dump $
i_tax:         ld   iyh,b           ; X=A
               ld   c,b             ; set N Z
               jp   (ix) ; main_loop
op_aa_len:     equ  $-op_aa



               org  op_ab
               dump $
               jp   i_undoc_2
op_ab_len:     equ  $-op_ab



               org  op_ac
               dump $
i_ldy_a:       ex   de,hl           ; LDY $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   c,(hl)          ; set N Z
               ld   iyl,c           ; set Y
               jp   (ix) ; read_loop
op_ac_len:     equ  $-op_ac



               org  op_ad
               dump $
i_lda_a:       ex   de,hl           ; LDA $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   b,(hl)          ; set A
               ld   c,b             ; set N Z
               jp   (ix) ; read_loop
op_ad_len:     equ  $-op_ad



               org  op_ae
               dump $
i_ldx_a:       ex   de,hl           ; LDX $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ld   c,(hl)          ; set N Z
               ld   iyh,c           ; set X
               jp   (ix) ; read_loop
op_ae_len:     equ  $-op_ae



               org  op_af
               dump $
i_bbs_2:       ld   a,(de)          ; BBS2 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  2,(hl)
               jp   nz,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_af_len:     equ  $-op_af



               org  op_b0
               dump $
i_bcs:         ex   af,af'
               jp   c,i_bra_ex      ; branch if C set
               ex   af,af'
               inc  de
               jp   (ix) ; main_loop
op_b0_len:     equ  $-op_b0



               org  op_b1
               dump $
i_lda_iy:      ld   a,(de)          ; LDA ($nn),Y
               inc  de
               ld   l,a
               ld   h,0
               ld   a,iyl           ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ld   b,(hl)          ; set A
               ld   c,b             ; set N Z
               jp   (ix) ; read_loop
op_b1_len:     equ  $-op_b1



               org  op_b2
               dump $
i_lda_iz:      ld   a,(de)          ; LDA ($nn) [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ld   b,(hl)          ; set A
               ld   c,b             ; set N Z
               jp   (ix) ; read_loop
op_b2_len:     equ  $-op_b2



               org  op_b3
               dump $
               jp   i_undoc_2
op_b3_len:     equ  $-op_b3



               org  op_b4
               dump $
i_ldy_zx:      ld   a,(de)          ; LDY $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   c,(hl)          ; set N Z
               ld   iyl,c           ; set Y
               jp   (ix) ; zread_loop
op_b4_len:     equ  $-op_b4



               org  op_b5
               dump $
i_lda_zx:      ld   a,(de)          ; LDA $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   b,(hl)          ; set A
               ld   c,b             ; set N Z
               jp   (ix) ; zread_loop
op_b5_len:     equ  $-op_b5



               org  op_b6
               dump $
i_ldx_zy:      ld   a,(de)          ; LDX $nn,Y
               inc  de
               add  a,iyl           ; add Y (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   c,(hl)          ; set N Z
               ld   iyh,c           ; set X
               jp   (ix) ; zread_loop
op_b6_len:     equ  $-op_b6



               org  op_b7
               dump $
i_smb_3:       ld   a,(de)          ; SMB3 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               set  3,(hl)
               jp   (ix) ; zwrite_loop
op_b7_len:     equ  $-op_b7



               org  op_b8
               dump $
i_clv:         exx                  ; clear overflow
               ld   e,0
               exx
               jp   (ix) ; main_loop
op_b8_len:     equ  $-op_b8



               org  op_b9
               dump $
i_lda_ay:      ld   a,(de)          ; LDA $nnnn,Y
               inc  de
               add  a,iyl           ; add Y
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   b,(hl)          ; set A
               ld   c,b             ; set N Z
               jp   (ix) ; read_loop
op_b9_len:     equ  $-op_b9



               org  op_ba
               dump $
i_tsx:         exx
               ld   a,l             ; S
               exx
               ld   iyh,a           ; X=S
               ld   c,a             ; set N Z
               jp   (ix) ; main_loop
op_ba_len:     equ  $-op_ba



               org  op_bb
               dump $
               jp   i_undoc_3
op_bb_len:     equ  $-op_bb



               org  op_bc
               dump $
i_ldy_ax:      ld   a,(de)          ; LDY $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   c,(hl)          ; set N Z
               ld   iyl,c           ; set Y
               jp   (ix) ; read_loop
op_bc_len:     equ  $-op_bc



               org  op_bd
               dump $
i_lda_ax:      ld   a,(de)          ; LDA $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   b,(hl)          ; set A
               ld   c,b             ; set N Z
               jp   (ix) ; read_loop
op_bd_len:     equ  $-op_bd



               org  op_be
               dump $
i_ldx_ay:      ld   a,(de)          ; LDX $nnnn,Y
               inc  de
               add  a,iyl           ; add Y
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ld   c,(hl)          ; set N Z
               ld   iyh,c           ; set X
               jp   (ix) ; read_loop
op_be_len:     equ  $-op_be



               org  op_bf
               dump $
i_bbs_3:       ld   a,(de)          ; BBS3 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  3,(hl)
               jp   nz,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_bf_len:     equ  $-op_bf



               org  op_c0
               dump $
i_cpy_i:       ld   h,d
               ld   l,e
               inc  de
               ex   af,af'          ; carry
               ld   a,iyl           ; Y
               sub  (hl)            ; Y-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_c0_len:     equ  $-op_c0



               org  op_c1
               dump $
i_cmp_ix:      ld   a,(de)          ; CMP ($xx,X)
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ex   af,af'          ; carry
               ld   a,b             ; A
               sub  (hl)            ; A-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_c1_len:     equ  $-op_c1



               org  op_c2
               dump $
               jp   i_undoc_2
op_c2_len:     equ  $-op_c2



               org  op_c3
               dump $
               jp   i_undoc_2
op_c3_len:     equ  $-op_c3



               org  op_c4
               dump $
i_cpy_z:       ld   a,(de)          ; CPY $nn
               inc  de
               ld   l,a
               ld   h,0
               ex   af,af'          ; carry
               ld   a,iyl           ; Y
               sub  (hl)            ; Y-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_c4_len:     equ  $-op_c4



               org  op_c5
               dump $
i_cmp_z:       ld   a,(de)          ; CMP $nn
               inc  de
               ld   l,a
               ld   h,0
               ex   af,af'          ; carry
               ld   a,b             ; A
               sub  (hl)            ; A-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_c5_len:     equ  $-op_c5



               org  op_c6
               dump $
i_dec_z:       ld   a,(de)          ; DEC $nn
               inc  de
               ld   l,a
               ld   h,0
               dec  (hl)            ; zero-page--
               ld   c,(hl)          ; set N Z
               jp   (ix) ; zread_write_loop
op_c6_len:     equ  $-op_c6



               org  op_c7
               dump $
i_smb_4:       ld   a,(de)          ; SMB4 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               set  4,(hl)
               jp   (ix) ; zwrite_loop
op_c7_len:     equ  $-op_c7



               org  op_c8
               dump $
i_iny:         inc  iyl             ; Y++
               ld   c,iyl           ; set N Z
               jp   (ix) ; main_loop
op_c8_len:     equ  $-op_c8



               org  op_c9
               dump $
i_cmp_i:       ld   h,d
               ld   l,e
               inc  de
               ex   af,af'          ; carry
               ld   a,b             ; A
               sub  (hl)            ; A-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_c9_len:     equ  $-op_c9



               org  op_ca
               dump $
i_dex:         dec  iyh             ; X--
               ld   c,iyh           ; set N Z
               jp   (ix) ; main_loop
op_ca_len:     equ  $-op_ca



               org  op_cb
               dump $
i_wai:         dec  de              ; WAI [65C02]
               jp   (ix) ; main_loop
op_cb_len:     equ  $-op_cb



               org  op_cc
               dump $
i_cpy_a:       ex   de,hl           ; CPY $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ex   af,af'          ; carry
               ld   a,iyl           ; Y
               sub  (hl)            ; Y-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_cc_len:     equ  $-op_cc



               org  op_cd
               dump $
i_cmp_a:       ex   de,hl           ; CMP $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ex   af,af'          ; carry
               ld   a,b             ; A
               sub  (hl)            ; A-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_cd_len:     equ  $-op_cd



               org  op_ce
               dump $
i_dec_a:       ex   de,hl           ; DEC $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               dec  (hl)            ; mem--
               ld   c,(hl)          ; set N Z
               jp   read_write_loop
op_ce_len:     equ  $-op_ce



               org  op_cf
               dump $
i_bbs_4:       ld   a,(de)          ; BBS4 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  4,(hl)
               jp   nz,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_cf_len:     equ  $-op_cf



               org  op_d0
               dump $
i_bne:         inc  c
               dec  c
               jp   nz,i_bra        ; branch if not zero
               inc  de
               jp   (ix) ; main_loop
op_d0_len:     equ  $-op_d0



               org  op_d1
               dump $
i_cmp_iy:      ld   a,(de)          ; CMP ($nn),Y
               inc  de
               ld   l,a
               ld   h,0
               ld   a,iyl           ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               ex   af,af'          ; carry
               ld   a,b             ; A
               sub  (hl)            ; A-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_d1_len:     equ  $-op_d1



               org  op_d2
               dump $
i_cmp_iz:      ld   a,(de)          ; indirect zero-page [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               ex   af,af'          ; carry
               ld   a,b             ; A
               sub  (hl)            ; A-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_d2_len:     equ  $-op_d2



               org  op_d3
               dump $
               jp   i_undoc_2
op_d3_len:     equ  $-op_d3



               org  op_d4
               dump $
               jp   i_undoc_2
op_d4_len:     equ  $-op_d4



               org  op_d5
               dump $
i_cmp_zx:      ld   a,(de)          ; CMP $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ex   af,af'          ; carry
               ld   a,b             ; A
               sub  (hl)            ; A-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_d5_len:     equ  $-op_d5



               org  op_d6
               dump $
i_dec_zx:      ld   a,(de)          ; DEC $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               dec  (hl)            ; zero-page--
               ld   c,(hl)          ; set N Z
               jp   (ix) ; zread_write_loop
op_d6_len:     equ  $-op_d6



               org  op_d7
               dump $
i_smb_5:       ld   a,(de)          ; SMB5 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               set  5,(hl)
               jp   (ix) ; zwrite_loop
op_d7_len:     equ  $-op_d7



               org  op_d8
               dump $
i_cld:         exx
               res  3,d             ; clear D
               exx
               xor  a               ; NOP
               ld   (adc_daa),a     ; use binary mode for adc
               ld   (sbc_daa),a     ; use binary mode for sbc
               jp   (ix) ; main_loop
op_d8_len:     equ  $-op_d8



               org  op_d9
               dump $
i_cmp_ay:      ld   a,(de)          ; CMP $nnnn,Y
               inc  de
               add  a,iyl           ; add Y
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ex   af,af'          ; carry
               ld   a,b             ; A
               sub  (hl)            ; A-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_d9_len:     equ  $-op_d9



               org  op_da
               dump $
i_phx:         ld   a,iyh           ; PHX [65C02]
               exx
               ld   (hl),a          ; push X
               dec  l               ; S--
               exx
               jp   (ix) ; main_loop
op_da_len:     equ  $-op_da



               org  op_db
               dump $
i_stp:         dec  de              ; STP [65C02]
               jp   i_sei           ; disable interrupts
op_db_len:     equ  $-op_db



               org  op_dc
               dump $
               jp   i_undoc_3
op_dc_len:     equ  $-op_dc



               org  op_dd
               dump $
i_cmp_ax:      ld   a,(de)          ; CMP $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               ex   af,af'          ; carry
               ld   a,b             ; A
               sub  (hl)            ; A-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_dd_len:     equ  $-op_dd



               org  op_de
               dump $
i_dec_ax:      ld   a,(de)          ; DEC $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               dec  (hl)            ; mem--
               ld   c,(hl)          ; set N Z
               jp   read_write_loop
op_de_len:     equ  $-op_de



               org  op_df
               dump $
i_bbs_5:       ld   a,(de)          ; BBS5 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  5,(hl)
               jp   nz,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_df_len:     equ  $-op_df



               org  op_e0
               dump $
i_cpx_i:       ld   h,d
               ld   l,e
               inc  de
               ex   af,af'          ; carry
               ld   a,iyh           ; X
               sub  (hl)            ; X-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_e0_len:     equ  $-op_e0



               org  op_e1
               dump $
i_sbc_ix:      ld   a,(de)          ; SBC ($xx,X)
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               jp   i_sbc
op_e1_len:     equ  $-op_e1



               org  op_e2
               dump $
               jp   i_undoc_2
op_e2_len:     equ  $-op_e2



               org  op_e3
               dump $
               jp   i_undoc_2
op_e3_len:     equ  $-op_e3



               org  op_e4
               dump $
i_cpx_z:       ld   a,(de)          ; CPX $nn
               inc  de
               ld   l,a
               ld   h,0
               ex   af,af'          ; carry
               ld   a,iyh           ; X
               sub  (hl)            ; X-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_e4_len:     equ  $-op_e4



               org  op_e5
               dump $
i_sbc_z:       ld   a,(de)          ; SBC $nn
               inc  de
               ld   l,a
               ld   h,0
               jp   i_sbc
op_e5_len:     equ  $-op_e5



               org  op_e6
               dump $
i_inc_z:       ld   a,(de)          ; INC $nn
               inc  de
               ld   l,a
               ld   h,0
               inc  (hl)            ; zero-page++
               ld   c,(hl)          ; set N Z
               jp   (ix) ; zread_write_loop
op_e6_len:     equ  $-op_e6



               org  op_e7
               dump $
i_smb_6:       ld   a,(de)          ; SMB6 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               set  6,(hl)
               jp   (ix) ; zwrite_loop
op_e7_len:     equ  $-op_e7



               org  op_e8
               dump $
i_inx:         inc  iyh             ; X++
               ld   c,iyh           ; set N Z
               jp   (ix) ; main_loop
op_e8_len:     equ  $-op_e8



               org  op_e9
               dump $
i_sbc_i:       ld   h,d
               ld   l,e
               inc  de
i_sbc:         ex   af,af'          ; carry
               ccf                  ; uses inverted carry
               ld   a,b
               sbc  a,(hl)          ; A-M-(1-C)
sbc_daa:       nop
               ld   b,a             ; set A
               ld   c,a             ; set N Z
               ccf                  ; no carry for overflow
               exx
               jp   pe,adcsbc_v
               ld   e,%00000000
               exx
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_e9_len:     equ  $-op_e9



               org  op_ea
               dump $
i_nop:         jp   (ix) ; main_loop
op_ea_len:     equ  $-op_ea



               org  op_eb
               dump $
               jp   i_undoc_2
op_eb_len:     equ  $-op_eb



               org  op_ec
               dump $
i_cpx_a:       ex   de,hl           ; CPX $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               ex   af,af'          ; carry
               ld   a,iyh           ; X
               sub  (hl)            ; X-x (result discarded)
               ld   c,a             ; set N Z
               ccf
               ex   af,af'          ; set carry
               jp   (ix) ; read_loop
op_ec_len:     equ  $-op_ec



               org  op_ed
               dump $
i_sbc_a:       ex   de,hl           ; SBC $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               jp   i_sbc
op_ed_len:     equ  $-op_ed



               org  op_ee
               dump $
i_inc_a:       ex   de,hl           ; INC $nnnn
               ld   e,(hl)
               inc  hl
               ld   d,(hl)
               inc  hl
               ex   de,hl
               inc  (hl)            ; mem++
               ld   c,(hl)          ; set N Z
               jp   read_write_loop
op_ee_len:     equ  $-op_ee



               org  op_ef
               dump $
i_bbs_6:       ld   a,(de)          ; BBS6 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  6,(hl)
               jp   nz,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_ef_len:     equ  $-op_ef



               org  op_f0
               dump $
i_beq:         inc  c
               dec  c
               jp   z,i_bra         ; branch if zero
               inc  de
               jp   (ix) ; main_loop
op_f0_len:     equ  $-op_f0



               org  op_f1
               dump $
i_sbc_iy:      ld   a,(de)          ; SBC ($nn),Y
               inc  de
               ld   l,a
               ld   h,0
               ld   a,iyl           ; Y
               add  a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,0
               adc  a,h
               ld   h,a
               jp   i_sbc
op_f1_len:     equ  $-op_f1



               org  op_f2
               dump $
i_sbc_iz:      ld   a,(de)          ; indirect zero-page [65C02]
               inc  de
               ld   l,a
               ld   h,0
               ld   a,(hl)
               inc  l               ; (may wrap in zero page)
               ld   h,(hl)
               ld   l,a
               ld   a,(hl)
               inc  hl
               ld   h,(hl)
               ld   l,a
               jp   i_sbc
op_f2_len:     equ  $-op_f2



               org  op_f3
               dump $
               jp   i_undoc_2
op_f3_len:     equ  $-op_f3



               org  op_f4
               dump $
               jp   i_undoc_2
op_f4_len:     equ  $-op_f4



               org  op_f5
               dump $
i_sbc_zx:      ld   a,(de)          ; SBC $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               jp   i_sbc
op_f5_len:     equ  $-op_f5



               org  op_f6
               dump $
i_inc_zx:      ld   a,(de)          ; INC $nn,X
               inc  de
               add  a,iyh           ; add X (may wrap in zero page)
               ld   l,a
               ld   h,0
               inc  (hl)            ; zero-page++
               ld   c,(hl)          ; set N Z
               jp   (ix) ; zread_write_loop
op_f6_len:     equ  $-op_f6



               org  op_f7
               dump $
i_smb_7:       ld   a,(de)          ; SMB7 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               set  7,(hl)
               jp   (ix) ; zwrite_loop
op_f7_len:     equ  $-op_f7



               org  op_f8
               dump $
i_sed:         exx
               set  3,d             ; set D
               exx
               ld   a,&27           ; DAA
               ld   (adc_daa),a     ; use decimal mode for adc
               ld   (sbc_daa),a     ; use decimal mode for sbc
               jp   (ix) ; main_loop
op_f8_len:     equ  $-op_f8



               org  op_f9
               dump $
i_sbc_ay:      ld   a,(de)          ; SBC $nnnn,Y
               inc  de
               add  a,iyl           ; add Y
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               jp   i_sbc
op_f9_len:     equ  $-op_f9



               org  op_fa
               dump $
i_plx:         exx                  ; PLX [65C02]
               inc  l               ; S++
               ld   a,(hl)          ; pop
               exx
               ld   iyh,a           ; set X
               ld   c,a             ; set N Z
               jp   (ix) ; main_loop
op_fa_len:     equ  $-op_fa



               org  op_fb
               dump $
               jp   i_undoc_3
op_fb_len:     equ  $-op_fb



               org  op_fc
               dump $
               jp   i_undoc_3
op_fc_len:     equ  $-op_fc



               org  op_fd
               dump $
i_sbc_ax:      ld   a,(de)          ; SBC $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               jp   i_sbc
op_fd_len:     equ  $-op_fd



               org  op_fe
               dump $
i_inc_ax:      ld   a,(de)          ; INC $nnnn,X
               inc  de
               add  a,iyh           ; add X
               ld   l,a
               ld   a,(de)
               inc  de
               adc  a,0
               ld   h,a
               inc  (hl)            ; mem++
               ld   c,(hl)          ; set N Z
               jp   read_write_loop
op_fe_len:     equ  $-op_fe



               org  op_ff
               dump $
i_bbs_7:       ld   a,(de)          ; BBS7 [65C02]
               inc  de
               ld   l,a
               ld   h,0
               bit  7,(hl)
               jp   nz,i_bra
               inc  de
               jp   (ix) ; zread_loop
op_ff_len:     equ  $-op_ff
