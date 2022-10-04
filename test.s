global main

extern print
extern printi
extern flush
extern getchar
extern ord
extern chr
extern size
extern substring
extern concat
extern not
extern exit
extern stringEqual
extern malloc
extern initArray

section .data
    align 2

section .text
    main:
    push rbp
    mov rbp, rsp
    sub rsp, 32
            
    l23:
    mov rcx, rbp
    mov [rbp + -32], rcx
    mov [rbp + -8], rdi
    mov rdi, 1
    mov rax, 0
    cmp rdi, rax
    jle l17
    l16:
    mov rax, 1
    jmp l18
    l17:
    mov rax, 0
    l18:
    mov rcx, 1
    cmp rax, rcx
    jne l20
    l19:
    call printi
    jmp l21
    l20:
    mov rdi, 0
    call printi
    l21:
    mov rax, 0
    mov rcx, [rbp + -32]
    mov rbp, rcx
    jmp l22
    l22:
    
    
    leave
    ret
            
