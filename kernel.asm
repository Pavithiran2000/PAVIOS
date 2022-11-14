;*****************start of the kernel code***************
[org 0x000]
[bits 16]

[SEGMENT .text]

;START #####################################################

	
    mov ax, 0x0100			;location where kernel is loaded
    mov ds, ax
    mov es, ax
    
    cli
    mov ss, ax				;stack segment
    mov sp, 0xFFFF			;stack pointer at 64k limit
    sti

    push dx
    push es
    xor ax, ax
    mov es, ax
    cli
    mov word [es:0x21*4], _int0x21	; setup interrupt service
    mov [es:0x21*4+2], cs
    sti
    pop es
    pop dx

	call _display_endl
	mov si, wel0
	mov al, 0x01
	int 0x21
	call _display_endl


	call _display_endl
	mov si, wel01
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, wel01
	mov al, 0x01
	int 0x21

	call _display_endl
	mov si, wel02
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, wel02
	mov al, 0x01
	int 0x21

	call _display_endl
	mov si, wel03
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, wel03
	mov al, 0x01
	int 0x21

	call _display_endl
	mov si, wel04
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, wel04
	mov al, 0x01
	int 0x21

	call _display_endl
	mov si, wel05
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, wel05
	mov al, 0x01
	int 0x21

	call _display_endl
	call _display_endl
	mov si, wel0
	mov al, 0x01
	int 0x21
	call _display_endl
	call _display_endl

    mov si, strWelcomeMsg   ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21


	call _display_endl
	mov si, wel1
	mov al, 0x01
	int 0x21

	call _display_endl
	mov si, wel2
	mov al, 0x01
	int 0x21

	call _display_endl
	mov si, wel3
	mov al, 0x01
	int 0x21
	call _display_endl
	


    call _display_endl
    mov si, strHelpMsg   ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21
    call _display_endl

	call _shell				; call the shell
    
    int 0x19                ; reboot
;END #######################################################

_int0x21:
    _int0x21_ser0x01:       ;service 0x01
    cmp al, 0x01            ;see if service 0x01 wanted
    jne _int0x21_end        ;goto next check (now it is end)
    
	_int0x21_ser0x01_start:
    lodsb                   ; load next character
    or  al, al              ; test for NUL character
    jz  _int0x21_ser0x01_end
    mov ah, 0x0E            ; BIOS teletype
    mov bh, 0x00            ; display page 0
    mov bl, 0x07            ; text attribute
    int 0x10                ; invoke BIOS
    jmp _int0x21_ser0x01_start
    _int0x21_ser0x01_end:
    jmp _int0x21_end

    _int0x21_end:
    iret
;---------------------------------------------------------------------
_shell:
	_shell_begin:
	;move to next line
	call _display_endl

	;display prompt
	call _display_prompt

	;get user command
	call _get_command
	
	;split command into components
	call _split_cmd

	;check command & perform action

	; empty command--------------------------
	_cmd_none:		
	mov si, strCmd0
	cmp BYTE [si], 0x00
	jne	_cmd_ver		;next command
	jmp _cmd_done
	
	; display version----------------------------
	_cmd_ver:		
	mov si, strCmd0
	mov di, cmdVer
	mov cx, 4
	repe	cmpsb
	jne	_cmd_displayHelpMenu		;next command
	
	call _display_endl
	mov si, strOsName		;display version
	mov al, 0x01
    	int 0x21
	call _display_space
	mov si, txtVersion		;display version
	mov al, 0x01
    	int 0x21
    	call _display_space
	
	mov si, strMajorVer		
	mov al, 0x01
    	int 0x21
	mov si, strMinorVer
	mov al, 0x01
    	int 0x21
    	jmp _cmd_done
    	
    	;display Help Menu-----------------------------------
	_cmd_displayHelpMenu:
	call _display_endl		
	mov si, strCmd0
	mov di, cmdHelp
	mov cx, 4
	repe	cmpsb
	jne	_cmd_displayHardwareInfo
	
	
	call _display_endl
	mov si, strHelpMsg1
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, strHelpMsg2
	mov al, 0x01
	int 0x21
	call _display_endl
	mov si, strHelpMsg3
	mov al, 0x01
	int 0x21
	call _display_endl
	jmp _cmd_done
    	
    	
    	
	;display hardware information----------------------------
    	_cmd_displayHardwareInfo:
    	mov si, strCmd0
	mov di, cmdHardInfo
	mov cx, 8
	repe	cmpsb
	jne 	_cmd_exit
	
	call _cmd_Memory
	call _cmd_cpuVendorID
	call _cmd_ProcessorType
	call _cmd_hard_drives
	call _cmd_ports
	
	jmp _cmd_done
	
	; exit shell-------------------------------------------
	_cmd_exit:		
	mov si, strCmd0
	mov di, cmdExit
	mov cx, 5
	repe	cmpsb
	jne	_cmd_unknown		;next command

	je _shell_end			;exit from shell
	jmp _cmd_done
	
_cmd_unknown:
	call _display_endl
	mov si, msgUnknownCmd		;unknown command
	mov al, 0x01
    	int 0x21
    	
;-------------------------------------------------------------

_cmd_done:

	;call _display_endl
	jmp _shell_begin
	
;--------------------------------------------------------------
	
_shell_end:
	ret
	
;---------------------------------------------------------------
	
_cmd_Memory:
    	cli
	mov ss, ax			;stack segment
	mov sp, 0xFFFF			;stack pointer at 64k limit
	sti

	push dx
	push es
	xor ax, ax
	mov es, ax
	cli
	mov word [es:0x21*4], _int0x21	; setup interrupt service
	mov [es:0x21*4+2], cs
	sti
	pop es
	pop dx
	

	; Reading Base Memory ----------------------------------------------
	
	push ax
	push dx
	
	int 0x12		; call interrupt 12 to get base mem size
	mov dx,ax 
	mov [basemem] , ax
	
	pop dx
	pop ax

	; Reading extended Memory ----------------------------------------------

	xor cx, cx		; Clear CX
	xor dx, dx		; clear DX
	mov ax, 0xE801
	int 0x15		; call interrupt 15h
	mov dx, ax		; save memory value in DX as the procedure argument
	mov [extmem1], ax

	xor cx, cx		; clear CX
        xor dx, dx		; clear DX
        mov ax, 0xE801
        int 0x15		; call interrupt 15h
	mov ax, dx		; save memory value in AX for division
	xor dx, dx
	mov si , 16
	div si			; divide AX value to get the number of MB
	mov dx, ax
	mov [extmem2], ax
	push dx			; save dx value

	; Total memory = basemem + extmem1 + extmem2 ------------------------------------
	call _display_endl
	mov si, strtotalmemory
	mov al, 0x01
	int 0x21
	
	mov ax, [basemem]	
	add ax, [extmem1]	; ax = ax + extmem1
	shr ax, 10
	add ax, [extmem2]	; ax = ax + extmem2
	mov dx, ax
	call _print_dec
	mov al, 0x4D            
	mov ah, 0x0E            ; BIOS teletype acts on 'M'
	mov bh, 0x00
	mov bl, 0x07
	int 0x10
	
;------------------------------------------------------------------------------------------

_cmd_cpuVendorID:
	call _display_endl
	mov si, strCPUVendorID
	mov al, 0x01
	int 0x21
	
	mov eax,0
	cpuid; call cpuid command
	mov [strVendorID],ebx		; load last string
	mov [strVendorID+4],edx;	 load middle string
	mov [strVendorID+8],ecx		; load first string

	mov si, strVendorID	 ;print string
	mov al, 0x01
	int 0x21
	
;--------------------------------------------------------------------------------------------

_cmd_ProcessorType:
	call _display_endl
	mov si, strCPUdescription
	mov al, 0x01
	int 0x21

	mov eax, 0x80000002		; get first part of the brand
	cpuid
	mov  [strBrand], eax
	mov  [strBrand+4], ebx
	mov  [strBrand+8], ecx
	mov  [strBrand+12], edx

	mov eax,0x80000003
	cpuid; call cpuid command
	mov [strBrand+16],eax
	mov [strBrand+20],ebx
	mov [strBrand+24],ecx
	mov [strBrand+28],edx

	mov eax,0x80000004
	cpuid     ; call cpuid command
	mov [strBrand+32],eax
	mov [strBrand+36],ebx
	mov [strBrand+40],ecx
	mov [strBrand+44],edx

	call _display_space	

	mov si, strBrand          ;print processor type
	mov al, 0x01
	int 0x21

;----------------------------------------------------------------------------------------------

	_cmd_hard_drives:
	call _display_endl
	mov si, strhdnumber
        mov al, 0x01
        int 0x21

	mov ax,0040h             ; look at 0040:0075 for a number
	mov es,ax                ;
	mov dl,[es:0075h]        ; move the number into DL register
	add dl,30h		; add 48 to get ASCII value            
	mov al, dl
        mov ah, 0x0E            ; BIOS teletype acts on character 
        mov bh, 0x00
        mov bl, 0x07
        int 0x10
	
;--------------------------------------------------------------------------------------------	

_cmd_ports:
	call _display_endl
	mov si, strserialportnumber
	mov al, 0x01
	int 0x21

	mov ax, [es:0x10]
	shr ax, 9
	and ax, 0x0007
	add al, 30h
	mov ah, 0x0E            ; BIOS teletype acts on character
	mov bh, 0x00
	mov bl, 0x07
	int 0x10
	call _display_endl
	jmp _cmd_done
	
;----------------------------------------------------------------------------------------
	
os_exit:
	mov ax, 5307h
	mov cx, 3
 	mov bx, 1
	int 15h
	
;---------------------------------------------------------------------------------------

_get_command:
	;initiate count
	mov BYTE [cmdChrCnt], 0x00
	mov di, strUserCmd

	_get_cmd_start:
	mov ah, 0x10		;get character
	int 0x16

	cmp al, 0x00		;check if extended key
	je _extended_key
	cmp al, 0xE0		;check if new extended key
	je _extended_key

	cmp al, 0x08		;check if backspace pressed
	je _backspace_key

	cmp al, 0x0D		;check if Enter pressed
	je _enter_key

	mov bh, [cmdMaxLen]		;check if maxlen reached
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start

	;add char to buffer, display it and start again
	mov [di], al			;add char to buffer
	inc di					;increment buffer pointer
	inc BYTE [cmdChrCnt]	;inc count

	mov ah, 0x0E			;display character
	mov bl, 0x07
	int 0x10
	jmp	_get_cmd_start

	_extended_key:			;extended key - do nothing now
	jmp _get_cmd_start

	_backspace_key:
	mov bh, 0x00			;check if count = 0
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start		;yes, do nothing
	
	dec BYTE [cmdChrCnt]	;dec count
	dec di

	;check if beginning of line
	mov	ah, 0x03		;read cursor position
	mov bh, 0x00
	int 0x10

	cmp dl, 0x00
	jne	_move_back
	dec dh
	mov dl, 79
	mov ah, 0x02
	int 0x10

	mov ah, 0x09		; display without moving cursor
	mov al, ' '
    	mov bh, 0x00
    	mov bl, 0x07
	mov cx, 1			; times to display
    	int 0x10
	jmp _get_cmd_start

	_move_back:
	mov ah, 0x0E		; BIOS teletype acts on backspace!
    	mov bh, 0x00
    	mov bl, 0x07
    	int 0x10
	mov ah, 0x09		; display without moving cursor
	mov al, ' '
    	mov bh, 0x00
    	mov bl, 0x07
	mov cx, 1			; times to display
    	int 0x10
	jmp _get_cmd_start

	_enter_key:
	mov BYTE [di], 0x00
	ret

;------------------------------------------------------------------------------------

_split_cmd:
	;adjust si/di
	mov si, strUserCmd
	;mov di, strCmd0

	;move blanks
	_split_mb0_start:
	cmp BYTE [si], 0x20
	je _split_mb0_nb
	jmp _split_mb0_end

	_split_mb0_nb:
	inc si
	jmp _split_mb0_start

	_split_mb0_end:
	mov di, strCmd0

	_split_1_start:			;get first string
	cmp BYTE [si], 0x20
	je _split_1_end
	cmp BYTE [si], 0x00
	je _split_1_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_1_start

	_split_1_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb1_start:
	cmp BYTE [si], 0x20
	je _split_mb1_nb
	jmp _split_mb1_end

	_split_mb1_nb:
	inc si
	jmp _split_mb1_start

	_split_mb1_end:
	mov di, strCmd1

	_split_2_start:			;get second string
	cmp BYTE [si], 0x20
	je _split_2_end
	cmp BYTE [si], 0x00
	je _split_2_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_2_start

	_split_2_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb2_start:
	cmp BYTE [si], 0x20
	je _split_mb2_nb
	jmp _split_mb2_end

	_split_mb2_nb:
	inc si
	jmp _split_mb2_start

	_split_mb2_end:
	mov di, strCmd2

	_split_3_start:			;get third string
	cmp BYTE [si], 0x20
	je _split_3_end
	cmp BYTE [si], 0x00
	je _split_3_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_3_start

	_split_3_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb3_start:
	cmp BYTE [si], 0x20
	je _split_mb3_nb
	jmp _split_mb3_end

	_split_mb3_nb:
	inc si
	jmp _split_mb3_start

	_split_mb3_end:
	mov di, strCmd3

	_split_4_start:			;get fourth string
	cmp BYTE [si], 0x20
	je _split_4_end
	cmp BYTE [si], 0x00
	je _split_4_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_4_start

	_split_4_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb4_start:
	cmp BYTE [si], 0x20
	je _split_mb4_nb
	jmp _split_mb4_end

	_split_mb4_nb:
	inc si
	jmp _split_mb4_start

	_split_mb4_end:
	mov di, strCmd4

	_split_5_start:			;get last string
	cmp BYTE [si], 0x20
	je _split_5_end
	cmp BYTE [si], 0x00
	je _split_5_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_5_start

	_split_5_end:
	mov BYTE [di], 0x00

	ret
	
;---------------------------------------------------------------------------------------

_display_space:
	mov ah, 0x0E                            ; BIOS teletype
	mov al, 0x20
    	mov bh, 0x00                            ; display page 0
    	mov bl, 0x07                            ; text attribute
    	int 0x10                                ; invoke BIOS
	ret
	
;---------------------------------------------------------------------------

_display_endl:
	mov ah, 0x0E		; BIOS teletype acts on newline!
    	mov al, 0x0D
	mov bh, 0x00
    	mov bl, 0x07
    	int 0x10
	mov ah, 0x0E		; BIOS teletype acts on linefeed!
    	mov al, 0x0A
	mov bh, 0x00
    	mov bl, 0x07
    	int 0x10
	ret

;------------------------------------------------------------------------------

_display_prompt:
	mov si, strPrompt
	mov al, 0x01
	int 0x21
	ret
	
;---------------------------------------------------------------------------
	
_print_dec:
	push ax		; save AX
	push cx		; save CX
	push si		; save SI
	mov ax,dx		; copy number to AX
	mov si,10		; SI is used as the divisor
	xor cx,cx		; clear CX
	
;-----------------------------------------------------------------------------
	
_non_zero:

	xor dx,dx		; clear DX
	div si			; divide by 10
	push dx			; push number onto the stack
	inc cx			; increment CX to do it more times
	or ax,ax		; clear AX
	jne _non_zero		; if not go to _non_zero
	
;-----------------------------------------------------------------------------
	
_prepare_digits:

	pop dx			; get the digit from DX
	add dl,0x30		; add 30 to get the ASCII value
	call _print_char	; print char
	loop _prepare_digits	; loop till cx == 0

	pop si			; restore SI
	pop cx			; restore CX
	pop ax			; restore AX
	ret    
	
;--------------------------------------------------------------------------                  

_print_char:
	push ax			; save AX 
	mov al, dl
        mov ah, 0x0E		; BIOS teletype acts on printing char
        mov bh, 0x00
        mov bl, 0x07
        int 0x10

	pop ax			; restore AX
	ret
	
;-----------------------------------------------------------------------

_save_string:
	mov dword [si], eax
	mov dword [si+4], ebx
	mov dword [si+8], ecx
	mov dword [si+12], edx
	ret
	
;##########################################################################

[SEGMENT .data]
    	strWelcomeMsg   	db  	" Welcome to PAVIOS version 0.06", 0x00
	wel0			db	" ----------------------------------------------------------------", 0x00
	wel01			db	"         ******  ******  **  **  ******    ******  ******", 0x00
	wel02			db	"         **  **  **  **  **  **    **      **  **  **    ", 0x00
	wel03			db	"         ******  ******  **  **    **      **  **  ******", 0x00
	wel04			db	"         **      **  **  **  **    **      **  **      **", 0x00
	wel05			db	"         **      **  **  ******  ******    ******  ******", 0x00
	wel1 			db 	" Based on Josh", 0x00
	wel2 			db 	" By Dr.Mohan Raj Dhanagopal", 0x00
	wel3 			db 	" PAVIOS From SE/2019/015_Pavithiran", 0x00


    	strHelpMsg		db	" Type help for display help menu", 0x00
	strPrompt		db	" PAVIOS>>", 0x00
	cmdMaxLen		db	255			;maximum length of commands

	strOsName		db	"PAVIOS", 0x00	;OS details
	strMajorVer		db	"0", 0x00
	strMinorVer		db	".06", 0x00

	cmdVer			db	"ver", 0x00		; internal commands
	cmdExit		db	"exit", 0x00
	cmdHardInfo		db 	"hardinfo",0x00
	cmdHelp		db	"help",0x00
	
	strHelpMsg1		db  	"Type ver to display version",0x00
	strHelpMsg2		db  	"Type hardinfo for displaying Hardware information",0x00
	strHelpMsg3		db  	"Type exit for rebooting the system",0x00

	txtVersion		db	"version", 0x00	;messages and other strings
	msgUnknownCmd		db	"Unknown command or bad file name!", 0x00
	
	strtotalmemory		db	"Total Memory: ", 0x00
	strCPUVendorID		db	"CPU Vendor ID: ", 0x00
	strCPUdescription	db	"CPU description: ", 0x00
	strNotSupported	db	"Not supported.", 0x00
	strhdnumber		db	"Number of hard drives: ",0x00
	strserialportnumber	db	"Number of serial ports: ", 0x00

[SEGMENT .bss]
	strUserCmd	resb	256		;buffer for user commands
	cmdChrCnt	resb	1		;count of characters
	strCmd0		resb	256		;buffers for the command components
	strCmd1		resb	256
	strCmd2		resb	256
	strCmd3		resb	256
	strCmd4		resb	256
		
	strVendorID	resb	16
	strBrand	resb	48
	basemem		resb	2
	extmem1		resb	2
	extmem2		resb	2

;********************end of the kernel code********************
