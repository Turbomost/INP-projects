; Autor reseni: Václav Valenta xvalen29

; Projekt 2 - INP 2022
; Vernamova sifra na architekture MIPS64

; DATA SEGMENT
                .data
login:          .asciiz "xvalen29"  ; sem doplnte vas login
cipher:         .space  17  ; misto pro zapis sifrovaneho loginu

params_sys5:    .space  8   ; misto pro ulozeni adresy pocatku
                            ; retezce pro vypis pomoci syscall 5
                            ; (viz nize "funkce" print_string)

; CODE SEGMENT
                .text
main:

    ; r6 = key
    ; r7 = login index
    ; r10 = ascii value of character
    ; r21 = temporary register for slti instructions
            
            main_loop:
                ; load char
                lb      r10, login(r7)  ; load character from login
                
                slti    r21, r10, 97    ; if ascii value of character is lower than 97 (is not a letter), jump to end
                bne     r0, r21, end    
                nop
                
                ; encrypt char
                sub     r10, r10, r6    ; subtract 0 / 1 (based on key) from character

                bne     r6, r0, check   ; if key is 0, jump to check
                nop

                addi    r10, r10, 22    ; if key is 1, add 22 to ascii value of character

            ; shift char if needed
            check:
                slti    r21, r10, 97    ; if ascii value of character is lower than 97, add 26
                bne     r0, r21, _add
                nop

                slti    r21, r10, 123   ; if ascii value of character is higher than 122, subtract 26
                beq     r0, r21, _sub
                nop

                b       key             ; else, jump directly to key
                nop

            ; add 26 to character
            _add:
                addi    r10, r10, 26
                b       key
                nop
            
            ; subtract 26 from character
            _sub:
                addi    r21, r0, 26
                sub     r10, r10, r21

            ; change key
            key:
                xori    r6, r6, 1       ; change key to !key
                sb      r10, cipher(r7) ; save character to cipher
                addi    r7, r7, 1       ; increment login index
                b       main_loop       ; jump to main_loop
                nop

            ; end of main_loop
            end:
                addi    r4, r0, cipher  ; put address of cipher to r4
                jal     print_string    ; call print_string
                syscall 0               ; halt


print_string:   ; adresa retezce se ocekava v r4
                sw      r4, params_sys5(r0)
                addi    r14, r0, params_sys5    ; adr pro syscall 5 musi do r14
                syscall 5   ; systemova procedura - vypis retezce na terminal
                jr      r31 ; return - r31 je urcen na return address
