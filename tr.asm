;tinyrogue
;12/8/16
;
;TODO:
;       WORK ON PURSUIT, SNEAKING & BACKSTABBING CODE
;       ADD TERMINATOR MOB (CAN DESTROY WALLS)
;       FIX DYNAMIC LIGHTING DELAY ON MOBS
;       FIX WALKTHRU WALL BUG, SWITCH TO BINARY COLLISION CK ROUTINE IN ROOM CREATION FUNCTION
;       FIX STUCK MOVEMENT WALKING ALONG JAGGED WALLS
;       ADD FLOORING TO ROOMS
;       AUTO DOORWAY ALIGNMENT FOR MOBS
;       ADD MICROSTEPPING
;       FIX INVISIBLE MOBS NEXT TO CHAR
;       ADD SOUND - added gunshot sound to LMOUSE
;       ADD WEAPON RECOIL
;           machinegun shotgun chainsaw machete 
;       USE COMBAT COLORS IN STATUS BAR
;       
;USE RGB MACRO TO MAKE PREDEFINED COLOR EQUs
;Font: Consolas 9pt
;Line Spacing 1.0, uncheck Add 10pt space between paragraphs
;Wordpad: landscape, print both sides, flip on short edge
;
;ml /c /coff labXX.asm
;link /SUBSYSTEM:WINDOWS labXX
;
;Line 278 mystery
;
;pWorldTable    coords from (0,0) to (4 Billion,4 Billion)
;     DWORD,DWORD == # entries,ID# of active area (used to calculate which WX,WY to -> to)
; +8  DWORD,DWORD == WorldX,WorldY   our first area starts at (2 Billion, 2 Billion)
;     DWORD,DWORD == pLocalTable,RESERVED
; +24 DWORD,DWORD == WorldX,WorldY   
;     DWORD,DWORD == pLocalTable,RESERVED
;               etc.
;
;   pLocalTable    coords start from (0,0) to (width of window,height of window)
;        DWORD == <RESERVED>
;    +4  DWORD == MobsTable
;    +8  DWORD == StructsTable
;
;      pMobsTable
;        DWORD,DWORD == # mobs,<RESERVED>
;    +8  DWORD,DWORD == X|Y,pChar pointer to char/mob/item detail sheet
;    +16 DWORD,DWORD == X|Y,pChar pointer to char/mob/item detail sheet
;               etc.
;
;      pStructsTable
;        DWORD  ==  RESERVED
;        DWORD  ==  pStructure, varying sizes determined by ((RoomW*RoomH)*8)+16)
;        DWORD  ==  pStructure, varying sizes determined by ((RoomW*RoomH)*8)+16)
;        ...
;        ...
;        0     
;
;         pStructure
;           DWORD,DWORD == RESERVED, RESERVED
;           DWORD,DWORD == RoomX|RoomY,RoomW|RoomH
;       +16 DWORD,DWORD == WallX|WallY,pChar pointer to wall description
;       +24 DWORD,DWORD == WallX|WallY,pChar pointer to wall description
;                  etc.
;
;pChar (default values can be derived from MOBSTRUCT)
;      primarily used as a character sheet for self and mobs, but can be used
;      also as description/misc fields for objects, structures, etc. so field
;      meanings can vary in that case.
;   
;      DWORD == ptr Name/race id
;      DWORD == symbol
;       ...     color either default/something else
;       ...     MOBID == area # mob belongs to (byte)|id # from counter (byte)
;               ptr to personal memories (contains results from CollionDet)
;                   DWORD == ptr to char sheet of current combatant
;                   DWORD == # steps to an obstacle/encounter/item/etc.
;                   DWORD == ptr to char sheet of encounter/item/etc.
;                   DWORD == ptr to dmg msg (next field)
;                   BYTES == series of bytes that is the actual dmg msg -> by ^
;               computed stats:  STR|INT|DEX|CON  1 byte each
;               HP|CurrHP
;               SP|CurrSP
;               ALLEGIANCE
;               WEAPON
;               STATUS:
;                       0 dead
;                       1 normal
;                       2 in combat
;                       3 fatality
;MobID
;   DWORD == area # mob resides|id# generated from a counter with a min val of 2
;invoke CollisionDet (ptr LocalTable we're using),(ptr X,Y of mob to move),(MobN|E|W|S)
;   returns value to Char/Mob's personal memories in their char sheet:
;                               # steps
;                               ptr to char/detail sheet of the encounter/item
;   also returns # of steps in eax

include tr-incprotomac.asm

WinMain PROTO :DWORD,:DWORD,:DWORD,:DWORD
CreateNewArea PROTO :HWND,:DWORD,:DWORD,:DWORD,:DWORD
CollisionDet PROTO :DWORD,:DWORD,:BYTE
DiceRoll4 PROTO :DWORD
Combat PROTO :DWORD,:DWORD
VisibilityCk PROTO :DWORD,:DWORD
PlayMp3File PROTO :DWORD,:DWORD
ScrBinCk PROTO :DWORD,:DWORD,:DWORD,:DWORD,:DWORD,:DWORD
CramEAX PROTO :DWORD,:DWORD

.data

include tr-data.asm

.data?

include tr-dataq.asm

.const

include tr-const.asm

.code

include tr-winmain.asm

WndProc proc hWnd:HWND,uMsg:UINT,wParam:WPARAM,lParam:LPARAM
    LOCAL hdc:HDC
    LOCAL ps:PAINTSTRUCT
    LOCAL hFont:HFONT
    LOCAL hdcCOPY:HDC
    LOCAL hFile:HANDLE
    LOCAL wfbytes:DWORD

    .if uMsg==WM_CREATE         ;used to create edit controls, buttons, etc.
        invoke CreateFont,FONT1H,FONT1W,0,0,400,0,0,0,OEM_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,DEFAULT_PITCH,ADDR TYPEFACE   
        mov mobFont,eax
        
        invoke CreateFont,LGFONT24x36H,LGFONT24x36W,0,0,400,0,0,0,OEM_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,DEFAULT_PITCH,ADDR TYPEFACE   
        mov lgFont24x36,eax

        invoke CreateFont,16,12,0,0,400,0,0,0,OEM_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,DEFAULT_PITCH,NULL   
        mov dmgmsgFont,eax
        
        invoke CreateFont,12,9,0,0,400,0,0,0,OEM_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,DEFAULT_PITCH,NULL   
        mov statsFont,eax
        
        invoke CreateFont,TINYFONT15x12H,TINYFONT15x12W,0,0,400,0,0,0,OEM_CHARSET,OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,DEFAULT_PITCH,NULL   
        mov tinyFont15x12,eax

;Initialize fields for background bitmap

        mov BitmapFile.bfType,4d42h
;        mov BitmapFile.bfSize,0
        mov BitmapFile.bfReserved1,0
        mov BitmapFile.bfReserved2,0
;        mov BitmapFile.bfOffBits,0

        mov BitmapInfo.biSize,sizeof BITMAPINFOHEADER
        mov BitmapInfo.biWidth,Scr_Width
        mov BitmapInfo.biHeight,Scr_Height
        mov BitmapInfo.biPlanes,1
        mov BitmapInfo.biBitCount,24
        mov BitmapInfo.biCompression,BI_RGB
;        mov BitmapInfo.biSizeImage,0
;        mov BitmapInfo.biXPelsPerMeter,0
;        mov BitmapInfo.biYPelsPerMeter,0
        mov BitmapInfo.biClrUsed,0
        mov BitmapInfo.biClrImportant,0

        movzx edi,BitmapInfo.biBitCount
        mov eax,BitmapInfo.biWidth
        mul edi
        add eax,31
        and eax,-32
        shr eax,3
        mov ecx,BitmapInfo.biHeight
        mul ecx
        mov BitmapInfo.biSizeImage,eax

        invoke GetDC,NULL
        mov hdc,eax

        invoke GetDeviceCaps,hdc,LOGPIXELSX
        invoke MulDiv,eax,39370,1000
        mov BitmapInfo.biXPelsPerMeter,eax

        invoke GetDeviceCaps,hdc,LOGPIXELSY
        invoke MulDiv,eax,39370,1000
        mov BitmapInfo.biYPelsPerMeter,eax

        invoke ReleaseDC,NULL,hdc

        mov eax,sizeof BITMAPFILEHEADER
        add eax,BitmapInfo.biSize
        mov BitmapFile.bfOffBits,eax
        add eax,BitmapInfo.biSizeImage
        mov BitmapFile.bfSize,eax

        invoke CreateDIBSection,hdc,ADDR BitmapInfo,DIB_RGB_COLORS,ADDR pBitmapBits,0,0
        mov BackgroundBMP,eax

        invoke PostMessage,hWnd,WM_CREATEWORLD,NULL,NULL

    .elseif uMsg==WM_CREATEWORLD
        invoke GetTickCount
        invoke nseed,eax
    
        invoke CreateEvent,NULL,FALSE,FALSE,NULL
        mov hEventMob,eax
        mov eax,OFFSET ThreadProc
        invoke CreateThread,NULL,NULL,eax,NULL,NORMAL_PRIORITY_CLASS,ADDR ThreadID
        invoke CloseHandle,eax
    
        invoke HeapCreate,NULL,4096,NULL
        mov hTable,eax
    
        invoke HeapAlloc,hTable,NULL,512            ;Create WorldTable
        mov pWorldTable,eax
        mov esi,eax
        mov DWORD ptr [esi],1
        mov DWORD ptr [esi+4],1
        mov DWORD ptr [esi+8],2000000000
        mov DWORD ptr [esi+12],2000000000
    
        invoke HeapAlloc,hTable,NULL,512
        mov [esi+16],eax                            ;First LocalTable entry into WorldTable
        mov pLocalTabCurr,eax
        mov esi,eax

        invoke HeapAlloc,hTable,NULL,96             ;Setup first Character Sheet
        mov pChar,eax
        mov [esi+12],eax
        mov esi,[esi+12]                            ;load Char sheet
        mov ebx,1
        shl ebx,16
        mov bx,1
        mov [esi+OFFSET_MOB_OBJ_ID],ebx             ;MOB/OBJ ID==area 1|id# 1
        mov eax,[human1.raceidptr]
        mov [esi+OFFSET_NAME],eax                   ;->"human"
        mov eax,[human1.symb]
        mov [esi+OFFSET_SYMB],eax                   ;'@'
        mov eax,[human1.color]
        mov [esi+OFFSET_COLOR],eax                  ;SNOW 3
        mov DWORD ptr [esi+OFFSET_VISIBLE],eax
        mov DWORD ptr [esi+OFFSET_ALTCOLOR],eax
        mov eax,[human1.weaponptr]
        mov [esi+OFFSET_WEAPON],eax
        mov DWORD ptr [esi+OFFSET_ALLEGIANCE],1
        mov DWORD ptr [esi+OFFSET_STATUS],1
        invoke CramEAX,FONT1W,FONT1H
        mov [esi+OFFSET_FONTWH],eax
        push DWORD ptr mobFont
        pop [esi+OFFSET_FONT]
        lea eax,[esi+OFFSET_STATUSMSG+4]
        mov [esi+OFFSET_STATUSMSG],eax
        invoke HeapAlloc,hTable,HEAP_ZERO_MEMORY,96 ;create space for personal memories
        mov [esi+OFFSET_MOBMEM],eax
        lea ebx,[eax+OFFSET_MOBMEM_DMGMSG+4]
        mov [eax+OFFSET_MOBMEM_DMGMSG],ebx    
        invoke DiceRoll4,human1.attribs
        mov [esi+OFFSET_STATS],eax                  ;STR|INT|DEX|CON
;        movzx eax,al                                ;isolate CON stat, Make HP==CON*LVL
        invoke nrandom,30
        add eax,70                                  ;HP==70+(1 to 30 more HP)
        push ax
        shl eax,16
        pop ax                                      
        mov [esi+OFFSET_HPCURRHP],eax               ;HP|Current HP
        mov eax,CharX
        shl eax,16
        mov ebx,CharY
        push bx
        pop ax
        mov [esi+OFFSET_BgXY],eax        
    
        invoke HeapAlloc,hTable,HEAP_ZERO_MEMORY,80
        mov TS_MSG1,eax
        invoke HeapAlloc,hTable,HEAP_ZERO_MEMORY,80
        mov TS_MSG2,eax
        invoke HeapAlloc,hTable,HEAP_ZERO_MEMORY,80
        mov TS_MSG3,eax
          
        mov esi,pLocalTabCurr
        invoke CreateNewArea,hWnd,NULL,NULL,NULL,NULL

        invoke PostMessage,hWnd,WM_CHAR,NULL,NULL   ;display our very first area
    
    .elseif uMsg==WM_SIZE 
        mov eax,lParam              ;high word of lParam contains the height and the low word of lParam the width of the client area
        mov edx,eax 
        shr edx,16 
        and eax,0ffffh 
        mov hWin,edx
        mov wWin,eax

    .elseif uMsg==WM_PAINT
        invoke BeginPaint,hWnd,ADDR ps
        mov hdc,eax
    
        invoke SelectObject,hdc,mobFont
        mov hFont,eax
    
        RGB 255,255,255
        invoke SetTextColor,hdc,eax
        invoke SetBkMode,hdc,TRANSPARENT
        invoke SetBkColor,hdc,COLOR_BLACK

        invoke CreateCompatibleDC,hdc
        mov hdcCOPY,eax

        invoke SelectObject,hdcCOPY,BackgroundBMP

        .if (PAINT_CHAR>0)                  ;PAINT_CHAR==idx# to mob/obj's X,Y in MobsTable
            mov esi,pMobsCurr
            mov DWORD ptr edi,PAINT_CHAR

            mov ebx,[esi+(edi*8)+4]         ;->Char/Mob Sheet
            mov eax,[ebx+OFFSET_BgXY]
            shr eax,16
            mov BgX,eax
            mov eax,[ebx+OFFSET_BgXY]
            and eax,0ffffh
            mov BgY,eax
            mov eax,[ebx+OFFSET_FONTWH]
            shr eax,16
            mov ebx,[ebx+OFFSET_FONTWH]
            and ebx,0ffffh
            invoke BitBlt,hdc,BgX,BgY,eax,ebx,hdcCOPY,BgX,BgY,SRCCOPY  ;restore background of char/mob
            
            mov ebx,[esi+(edi*8)+4]         ;->Char/Mob Sheet
            invoke SelectObject,hdc,[ebx+OFFSET_FONT]
            invoke SetTextColor,hdc,[ebx+OFFSET_VISIBLE]
            mov DWORD ptr edi,PAINT_CHAR
            mov eax,[esi+(edi*8)]
            mov [ebx+OFFSET_BgXY],eax       ;Update for next movement
            shr eax,16                      ;X
            mov ecx,[esi+(edi*8)]
            and ecx,0ffffh                  ;Y
            lea ebx,[ebx+OFFSET_SYMB]
            invoke TextOut,hdc,eax,ecx,ebx,1
            mov PAINT_CHAR,0
        .endif

        .if (PAINT_STRUCTURE>0)             ;PAINT_STRUCTURE==# of structs to display
            mov edi,1
            .REPEAT
                mov esi,pStructsCurr        ;->list of structures in current LocalTable
                push edi
                mov esi,[esi+(edi*4)]       ;->to pStruct to display
                mov edi,1                   
                mov eax,[esi+(edi*8)+4]
                and eax,0ffffh              ;RoomH
                mov ebx,[esi+(edi*8)+4]
                shr ebx,16                  ;RoomW
                shl ebx,1                   ;*2
                sub eax,2
                shl eax,1                   ;*2
                add ebx,eax
                inc edi
                .REPEAT
                    mov edx,[esi+(edi*8)+4] ;->char sheet
                    invoke SetTextColor,hdc,[edx+OFFSET_VISIBLE]
                    mov eax,[esi+(edi*8)]
                    shr eax,16              ;X
                    mov ecx,[esi+(edi*8)]
                    and ecx,0ffffh          ;Y
;                    invoke TextOut,hdc,eax,ecx,[edx+OFFSET_SYMB],1
;Above line does not function correctly on newer i7/Win8.1 machine ?
;replaced with line below:
invoke TextOut,hdc,eax,ecx,OFFSET Wall_char,1
                    inc edi
                    dec ebx
                .UNTIL (ebx==0)
                pop edi
                inc edi
            .UNTIL (edi>PAINT_STRUCTURE)
            mov PAINT_STRUCTURE,0
        .endif

        .if (PAINT_DMGMSG>0)                ;PAINT_DMGMSG->to DMGMSG to display    
            invoke SelectObject,hdc,dmgmsgFont
            invoke SetTextColor,hdc,COLOR_SNOWWHITE
            mov esi,TS_MSG2
            mov edi,TS_MSG1
            mov ecx,len(TS_MSG2)
            rep movsb
            mov BYTE ptr [edi],0
            mov esi,TS_MSG3
            mov edi,TS_MSG2
            mov ecx,len(TS_MSG3)
            rep movsb
            mov BYTE ptr [edi],0
            mov esi,PAINT_DMGMSG
            mov edi,TS_MSG3
            mov ecx,len(esi)
            rep movsb
            mov BYTE ptr [edi],0
            invoke TextOut,hdc,0,450,TS_MSG1,len(TS_MSG1)
            invoke TextOut,hdc,0,468,TS_MSG2,len(TS_MSG2)
            invoke TextOut,hdc,0,486,TS_MSG3,len(TS_MSG3)
            mov PAINT_DMGMSG,0
        .endif
    
        .if (MouseClick>0)
            invoke SelectObject,hdc,statsFont
            invoke SetBkColor,hdc,COLOR_TEST
            invoke SetTextColor,hdc,COLOR_SNOWWHITE
    
            .if (MouseClick==1)
                invoke dwtoa,MouseX,ADDR buffer
                invoke TextOut,hdc,720,535,ADDR buffer,len(ADDR buffer)
                invoke dwtoa,MouseY,ADDR buffer
                invoke TextOut,hdc,760,535,ADDR buffer,len(ADDR buffer)
                jmp @F
            .elseif (MouseClick>CHESTSHOT)
                mov Targeted,OFFSET HeadShotTxt
            .elseif (MouseClick>LEGSHOT)
                mov Targeted,OFFSET ChestShotTxt
            .else
                mov Targeted,OFFSET LegShotTxt
            .endif
        
            invoke TextOut,hdc,825,535,Targeted,len(Targeted)
        
            invoke dwtoa,CharX,ADDR buffer
            invoke TextOut,hdc,0,520,ADDR buffer,len(ADDR buffer)
            invoke dwtoa,CharY,ADDR buffer
            invoke TextOut,hdc,40,520,ADDR buffer,len(ADDR buffer)
        
         @@:
            mov MouseClick,0    
        .endif
    
;display stats of our char
        
        mov esi,pChar       
    
    @ShowStats:
        invoke SelectObject,hdc,statsFont
    
        invoke SetBkColor,hdc,COLOR_TEST
        invoke SetTextColor,hdc,COLOR_SNOWWHITE

        mov ebx,[esi+OFFSET_STATS]          ;load stats
        mov esi,OFFSET StatBar
        mov edi,OFFSET StatMsg
        cld
        .REPEAT                             ;value of ebx is persistent through entire REPEAT..UNTIL loop
            mov al,[esi]
            .if (al=='%')
                inc esi
                push esi
                .if (BYTE ptr [esi]=='h')
                    mov esi,pChar
                    mov ebx,[esi+OFFSET_HPCURRHP]
                    rol ebx,16
                    jmp @F
                .elseif (BYTE ptr [esi]=='H')
                    ror ebx,16
                    jmp @F
                .elseif (BYTE ptr [esi]=='a')
                    mov esi,pWorldTable
                    mov ebx,[esi]
                    jmp @F
                .elseif (BYTE ptr [esi]=='A')
                    mov esi,pWorldTable
                    mov ebx,[esi+4]
                    jmp @F
                .elseif (BYTE ptr [esi]=='m')
                    mov esi,pWorldTable
                    lea esi,[esi+8]         ;move ptr to first WorldX,WorldY entry
                    dec ebx
                    shl ebx,4
                    mov esi,[esi+ebx+8]     ;->LocalTable
                    mov esi,[esi+4]         ;->MobsTable
                    mov ebx,[esi]
                    jmp @F
                .elseif (BYTE ptr [esi]=='x')
                    mov esi,pChar
                    mov ebx,[esi+OFFSET_MOBMEM]     
                    .if (DWORD ptr [ebx+OFFSET_MOBMEM_COMBATANT]>0)
                        mov esi,[ebx+OFFSET_MOBMEM_COMBATANT] 
                        mov ebx,[esi+OFFSET_HPCURRHP]
                        rol ebx,16
                    .else
                        xor ebx,ebx
                    .endif
                    jmp @F
                .elseif (BYTE ptr [esi]=='X')
                    ror ebx,16
                    jmp @F
                .endif
                rol ebx,8        
             @@:
                movzx eax,bl
                pop esi
                .if (BYTE ptr [esi]!=' ')
                    inc esi
                .endif
                invoke dwtoa,eax,edi
                inc edi
                .if (bl>9)
                    inc edi
                .endif
                .if (bl>99)
                    inc edi
                .endif
            .else
                movsb
            .endif
        .UNTIL (BYTE ptr [esi]==0)
        mov BYTE ptr [edi],0                ;NULL terminate our new string

        invoke BitBlt,hdc,0,535,700,10,hdcCOPY,0,535,SRCCOPY     ;restore background
        invoke TextOut,hdc,0,535,ADDR StatMsg,len(ADDR StatMsg)

        invoke DeleteDC,hdcCOPY        
        invoke SelectObject,hdc,hFont
        invoke EndPaint,hWnd,ADDR ps

    .elseif uMsg==WM_LBUTTONDOWN
        invoke mciSendCommand,Mp3DeviceID,MCI_CLOSE,0,0
        invoke PlayMp3File,hWnd,ADDR gunshot1
        mov eax,lParam
        and eax,0FFFFh
        mov MouseX,eax
        mov eax,lParam
        shr eax,16
        mov MouseY,eax
    
        ;Scan current MobTable for any mob that matches MX,MY coords
        ;if match found, calculate hit/miss depending on range of shot and type of weapon used
    
        mov esi,pMobsCurr
        mov ecx,[esi]                       ;# mobs
        lea esi,[esi+8]                     ;-> first X,Y entry
        mov edi,esi
        .REPEAT
            mov eax,[edi]                   ;X|Y
            shr eax,16                      ;X
            .if (MouseX>eax)
                add eax,FONT1W
                .if (MouseX<eax)
                    mov eax,[edi]
                    and eax,0ffffh          ;Y
                    .if (MouseY>eax)
                        add eax,FONT1H
                        .if (MouseY<eax)
                        ;We have a target
                        ;calculate the shot if's a headshot, chest, or legs
                        ;determine dmg done
                            sub eax,MouseY
                            mov MouseClick,eax
                            invoke Combat,esi,edi  ;esi->first (self) X|Y entry, edi->target's X|Y, MouseClick==head/chest/leg shot
                            jmp @Targeted
                        .endif
                    .endif
                .endif
            .endif
            lea edi,[edi+8]                 ;-> next X|Y entry
            dec ecx
        .UNTIL (ecx==0)
        ;No target found
        mov MouseClick,1
      @Targeted:
        mov XYrc.left,720
        mov XYrc.right,850
        mov XYrc.top,535
        mov XYrc.bottom,560
        invoke InvalidateRect,hwnd,ADDR XYrc,FALSE
        
    .elseif uMsg==WM_CHAR           ;used to handle keyboard input
        push wParam 
        pop  char
        
        mov edi,pMobsCurr
        mov esi,pLocalTabCurr
        lea edi,[edi+8]             ;->first X|Y entry (self)
        mov ebx,[edi+4]             ;char sheet of self
    
        .if (char==MobN)||(char==MobS)||(char==MobE)||(char==MobW)
            mov eax,[edi]
            and eax,0ffffh
            mov CharY,eax
            mov eax,[edi]
            shr eax,16
            mov CharX,eax
    
            push CharX
            pop XYrc.left
            
            push CharY
            pop XYrc.top
            
            push CharX
            pop XYrc.right
            add XYrc.right,FONT1W
            
            push CharY
            pop XYrc.bottom
            add XYrc.bottom,FONT1H

            invoke CollisionDet,esi,edi,BYTE ptr char   ;returns eax with value of # steps

            .if (eax==0)                                
            ;if 0 steps, ck what obstacle was in the way

                mov ebx,[ebx+OFFSET_MOBMEM]             ;Check char/mob's personal memories
                mov ebx,[ebx+OFFSET_MOBMEM_ENCCS]                 

                .if (ebx>0)                             ;Check if a Char Sheet entry is present, and if so:

                    .if (DWORD ptr [ebx+OFFSET_DOORWAY]>0)
                    ;Ck if char sheet is for a Doorway and perform any necessary alignment for char/mob
                        mov PAINT_CHAR,1                ;idx# of self in LocalTable
                        invoke InvalidateRect,hWnd,ADDR XYrc,FALSE
    
                        mov eax,[ebx+OFFSET_DOORWAY]    ;load original coords of Doorway
                        mov ebx,eax
                        shr eax,16
                        mov CharX,eax
                        and ebx,0ffffh
                        mov CharY,ebx
                        invoke CramEAX,CharX,CharY
                        mov esi,pMobsCurr
                        mov [esi+8],eax                 ;Update Char's new X,Y
    
                        push CharX
                        pop XYrc.left
                        push CharY
                        pop XYrc.top
                        add CharX,FONT1W
                        push CharX
                        pop XYrc.right
                        add CharY,FONT1H
                        push CharY
                        pop XYrc.bottom

                    .elseif (SDWORD ptr [ebx+OFFSET_ALLEGIANCE]<0)&&(DWORD ptr [ebx+OFFSET_STATUS]!=STAT_DEAD)
                    ;else ck if char/Mob was engaged in combat, if so assume obstacle is a combatant
                        mov eax,[ebx+OFFSET_MOB_OBJ_ID]
                        and eax,0ffffh
                        dec eax
                        shl eax,3           ;*8
                        lea eax,[edi+eax]   ;->target's X|Y                 
                        invoke Combat,edi,eax

                    .endif
                .else
                ;if obstacle was not a Doorway or enemy combatant then assume it was a Wall for now
                    jmp @WallSoNoGo

                .endif
                jmp @F

            ;otherwise steps were available so begin calculations for InvalidateRect for char/mob movement 

            .elseif (char==MobW)
                .if(eax>CharX)
                    mov CharX,0
                .else
                    sub CharX,eax                        
                .endif
                push CharX
                pop XYrc.left
            .elseif (char==MobE)
                add CharX,eax
                mov ecx,CharX
                add ecx,FONT1W
                mov XYrc.right,ecx                
            .elseif (char==MobN)
                .if(eax>CharY)
                    mov CharY,0
                .else
                    sub CharY,eax
                .endif
                push CharY
                pop XYrc.top
            .elseif (char==MobS)
                add CharY,eax
                mov ecx,CharY
                add ecx,FONT1H
                mov XYrc.bottom,ecx
            .endif
                  
            mov ecx,wWin
            mov edx,hWin

;Ck if self attempts to walk off screen        
            .if (CharX==0)||(CharX>ecx)||(CharY==0)||(CharY>edx)      
                mov edi,pWorldTable
                mov esi,[edi+4]         ;get ID# of current active area
                lea edi,[edi+8]         ;set to first WorldX,WorldY
                dec esi
                shl esi,4               
                lea edi,[edi+esi]       ;now point to current WorldX,WorldY area
                mov eax,[edi]           ;get WX
                mov ebx,[edi+4]         ;and WY
            
                mov WorldX,eax          ;calculate (WX,WY) of surrounding areas
                mov WorldY,ebx
                mov WorldXW,eax
                mov WorldXE,eax
                mov WorldYN,ebx
                mov WorldYS,ebx
                sub WorldXW,ecx
                add WorldXE,ecx
                sub WorldYN,edx
                add WorldYS,edx
    
                mov edi,pWorldTable
                mov esi,[edi]           ;load # areas that exist
        
                .REPEAT
                    mov edi,pWorldTable
                    lea edi,[edi+8]         ;set to first WorldX,WorldY
                    dec esi
                    push esi
                    shl esi,4               ;*16      
                    lea edi,[edi+esi]       ;goto each WorldX,WorldY coord
                    mov eax,[edi]           ;load the WX
                    mov ebx,[edi+4]         ;and WY we need to check
                    pop esi
                   
                    .if (CharX==0 && eax==WorldXW && ebx==WorldY)||(CharX>ecx && eax==WorldXE && ebx==WorldY)||(CharY==0 && eax==WorldX && ebx==WorldYN)||(CharY>edx && eax==WorldX && ebx==WorldYS)
                        inc esi                 
                        mov edi,pWorldTable     ;we found an existing area
                        mov [edi+4],esi         ;update active area #

                        push esi
                        pop eax            
                        shl eax,16          
                        mov ax,1
                        mov ebx,pChar
                        mov [ebx+OFFSET_MOB_OBJ_ID],eax  ;update with current area self is in
    
                        lea edi,[edi+8]
                        dec esi
                        shl esi,4
                        lea edi,[edi+esi]       ;goto appropriate WX,WY
                        mov esi,[edi+8]         ;load it's LocalTable
                        mov pLocalTabCurr,esi
             
                        .if (CharX==0)
                            mov ecx,wWin
                            sub ecx,FONT1W
                            mov CharX,ecx
                        .elseif (CharX>ecx)
                            mov CharX,0
                        .elseif (CharY==0)
                            mov edx,hWin
                            sub edx,FONT1H
                            mov CharY,edx
                        .elseif (CharY>edx)
                            mov CharY,0
                        .endif
    
                        jmp @Update_Area        
                                
                    .elseif (esi==0)        ;we have to create a new entry
                        mov edi,pWorldTable
                        mov eax,[edi]       ;get # areas
                        inc eax
                        mov [edi],eax       ;update # areas
                        mov [edi+4],eax     ;active area==new area we're about to add

                        push eax            
                        shl eax,16          
                        mov ax,1
                        mov ebx,pChar
                        mov [ebx+OFFSET_MOB_OBJ_ID],eax  ;update current area self is in
                        pop eax
    
                        lea edi,[edi+8]     ;point to first WorldX,WorldY
                        dec eax
                        shl eax,4
                        lea edi,[edi+eax]   ;set pointer to empty spot to add new WX,WY
                        
                        .if (CharX==0)
                            mov eax,WorldXW
                            mov ebx,WorldY
                            sub ecx,FONT1W
                            mov CharX,ecx
                        .elseif (CharX>ecx)
                            mov eax,WorldXE
                            mov ebx,WorldY
                            mov CharX,0
                        .elseif (CharY==0)
                            mov eax,WorldX
                            mov ebx,WorldYN
                            mov edx,hWin
                            sub edx,FONT1H
                            mov CharY,edx
                        .elseif (CharY>edx)
                            mov eax,WorldX
                            mov ebx,WorldYS
                            mov CharY,0
                        .endif
    
                        mov [edi],eax                       ;add new WX
                        mov [edi+4],ebx                     ;and WY coords
                        invoke HeapAlloc,hTable,NULL,512    ;create a new LocalTable
                        mov [edi+8],eax                     ;save to WorldTable
                        mov pLocalTabCurr,eax
                        mov esi,eax                         ;->LocalTable
    
                        invoke CreateNewArea,hWnd,NULL,NULL,NULL,NULL

            @Update_Area:
                        push DWORD ptr [esi+4]
                        pop pMobsCurr
                        push DWORD ptr [esi+8]
                        pop pStructsCurr

                        invoke CramEAX,CharX,CharY
                        mov esi,pMobsCurr
                        mov [esi+8],eax                     ;update self's (X,Y) in MobsTable
    
                        invoke InvalidateRect,hWnd,NULL,TRUE   ;Clear entire screen of new area
                        xor esi,esi
                    .endif
                .UNTIL (esi==0)
            .else
                invoke CramEAX,CharX,CharY
                mov esi,pMobsCurr
                mov [esi+8],eax
            .endif   
       
        @@: 
            mov PAINT_CHAR,1        ;idx# of self in LocalTable
            invoke InvalidateRect,hWnd,ADDR XYrc,FALSE

            invoke VisibilityCk,1,2 ;Begin dynamic lighting routine
        .endif

     @WallSoNoGo:

        mov eax,pChar
        xor edi,edi
        mov esi,OFFSET STAT_DESC

     @Stat_Ck:
        .if (edi==DWORD ptr [eax+OFFSET_STATUS])
            mov DWORD ptr edi,[eax+OFFSET_STATUSMSG]
            mov ecx,len(esi)
            inc ecx
            rep movsb
        .else
            .REPEAT
                inc esi
            .UNTIL (BYTE ptr [esi]==0)
            inc esi
            inc edi
            jmp @Stat_Ck
        .endif

        mov XYrc.left,0
        mov XYrc.top,500
        mov XYrc.right,700
        mov XYrc.bottom,534      
    
        mov XYrc.left,0
        mov XYrc.top,535
        mov XYrc.right,675
        mov XYrc.bottom,590      
    
        mov PAINT_STATS1,1
        invoke InvalidateRect,hWnd,ADDR XYrc,FALSE
    
        invoke UpdateWindow,hWnd
    
    .elseif uMsg==WM_TIMER
        mov eax,wParam
        mov MobID,eax
        invoke SetEvent,hEventMob
                 
    .elseif uMsg==WM_DESTROY
        invoke DeleteObject,hFont
        invoke DeleteObject,mobFont
        invoke DeleteObject,dmgmsgFont
        invoke DeleteObject,statsFont
        invoke DeleteObject,tinyFont15x12
        invoke DeleteObject,BackgroundBMP
        
        invoke HeapDestroy,hTable
        invoke PostQuitMessage,NULL
    
    .else
        invoke DefWindowProc,hWnd,uMsg,wParam,lParam
        ret

    .endif

    xor eax,eax
    ret
WndProc endp

VisibilityCk PROC USES ebx ecx edx esi edi EyesMobID:DWORD,Radius:DWORD
;Radius==# steps around EyesMobID that is visible
;Calculate radius of EyesMobID
;   X1==EyesMobIDX-(Radius*FONT1W) X2==EyesMobIDX+(Radius*FONT1W)
;   Y1==EyesMobIDY-(Radius*FONT1H) Y2==EyesMobIDY+(Radius*FONT1H)
;Ck X,Y of every object
;   if it falls inbetween calculated radius, 
;       change OFFSET_VISIBLE to value of OFFSET_COLOR 
;   else
;       change OFFSET_VISIBLE to 0
    LOCAL NumStructs:DWORD
    LOCAL NumObjs:DWORD
    LOCAL VisiXYrc:RECT
    LOCAL WallXYrc:RECT
    LOCAL FONTW:DWORD
    LOCAL FONTH:DWORD

    push Radius
    pop SightRadius                     ;update global var for use by ThreadProc
    mov esi,pMobsCurr
    mov edi,esi
    lea edi,[edi+8]                     ;->first X|Y entry (self)
    mov eax,EyesMobID
    dec eax
    shl eax,3                           ;*8
    lea edi,[edi+eax]                   ;->EyeMobID's X|Y

    mov eax,[edi+4]                     ;->Char Sheet
    mov edx,[eax+OFFSET_FONTWH]
    shr edx,16
    mov FONTW,edx
    mov edx,[eax+OFFSET_FONTWH]
    and edx,0ffffh
    mov FONTH,edx

    mov eax,[edi]
    shr eax,16                          ;X
    push eax
    mov VisiXYrc.left,eax
    mov eax,Radius
    mov ebx,FONTW
    mul ebx
    sub VisiXYrc.left,eax               ;==CharX-(FONT1W*Radius)
    .if (SDWORD ptr VisiXYrc.left<0)
        mov VisiXYrc.left,0
    .endif
    pop VisiXYrc.right
    add VisiXYrc.right,eax
    mov eax,FONTW
    add VisiXYrc.right,eax              ;==CharX+(FONT1W*Radius)+FONT1W
    mov eax,wWin
    .if (DWORD ptr VisiXYrc.right>eax)
         mov VisiXYrc.right,eax
    .endif
    mov eax,[edi]
    and eax,0ffffh                      ;Y
    push eax
    mov VisiXYrc.top,eax
    mov eax,Radius
    mov ebx,FONTH
    mul ebx
    sub VisiXYrc.top,eax                ;==CharY-(FONT1H*Radius)
    .if (SDWORD ptr VisiXYrc.top<0)
        mov VisiXYrc.top,0
    .endif
    pop VisiXYrc.bottom
    add VisiXYrc.bottom,eax
    mov eax,FONTH
    add VisiXYrc.bottom,eax             ;==CharY+(FONT1H*Radius)+FONT1H
    mov eax,hWin
    .if (DWORD ptr VisiXYrc.bottom>eax)
        mov VisiXYrc.bottom,eax
    .endif
    
;Adjust [OFFSET_VISIBLE] of mobs/objects as necessary

    .if (DWORD ptr [esi]==1)            ;if only 1 mob (self) in area scan for structures
		mov esi,pStructsCurr
        mov NumStructs,1
        mov edi,NumStructs
        .if (DWORD ptr [esi+4]==0)
            jmp @NoScan
        .else
@StructScan:
            mov esi,[esi+(edi*4)]       ;->a pStruct entry in pStructTable
            mov edi,1
            mov eax,[esi+(edi*8)+4]
            and eax,0ffffh
            mov ebx,[esi+(edi*8)+4]
            shr ebx,16
            shl ebx,1                   ;*2
            sub eax,2
            shl eax,1                   ;*2
            add ebx,eax
            mov NumObjs,ebx
            inc NumObjs
            inc edi                     ;first entry for scan starts at esi+24 in StrucTable

            jmp @F
        .endif
    .else
        mov NumStructs,0
        push [esi]
        pop NumObjs
        mov edi,1                       ;first entry for scan starts at esi+8 in MobsTable
     @@:.REPEAT
            mov eax,[esi+(edi*8)]
            shr eax,16                  ;X
            mov edx,eax
            add edx,FONTW
            mov ebx,[esi+(edi*8)]
            and ebx,0ffffh              ;Y
            mov ecx,ebx
            add ecx,FONTH

            mov WallXYrc.left,eax
            mov WallXYrc.top,ebx
            mov WallXYrc.right,edx
            mov WallXYrc.bottom,ecx

            .if (edx>VisiXYrc.left)&&(eax<VisiXYrc.right)&&(ecx>VisiXYrc.top)&&(ebx<VisiXYrc.bottom)                    
            ;Mob/Obj is in sight!
                mov ebx,[esi+(edi*8)+4]     ;->char sheet
                push [ebx+OFFSET_COLOR]
                pop [ebx+OFFSET_VISIBLE]
            .else
            ;Mob/Obj is not in sight
                mov ebx,[esi+(edi*8)+4]     ;->char sheet
                .if (DWORD ptr [ebx+OFFSET_VISIBLE])>0
                    push DWORD ptr [ebx+OFFSET_ALTCOLOR]
                    pop DWORD ptr [ebx+OFFSET_VISIBLE]
                .else
                    mov DWORD ptr [ebx+OFFSET_VISIBLE],0
;                    mov DWORD ptr [ebx+OFFSET_VISIBLE],COLOR_DARKGRAY
                .endif        
            .endif
            
            .if(NumStructs>0)
                push NumStructs
                pop PAINT_STRUCTURE
                invoke InvalidateRect,hwnd,ADDR WallXYrc,FALSE
            .endif

            inc edi
        .UNTIL (edi>NumObjs)              

        inc NumStructs
        mov edi,NumStructs
        mov esi,pStructsCurr
        .if (DWORD ptr [esi+(edi*4)]>0)
            jmp @StructScan
        .endif                   
    .endif

;    invoke InvalidateRect,hwnd,ADDR VisiXYrc,TRUE

@NoScan:
    ret
VisibilityCk ENDP        

CollisionDet PROC USES ebx ecx edx esi edi ptrLocalTab:DWORD,ptrMobXY:DWORD,Mobdir:BYTE
    LOCAL TmpXY:DWORD
    LOCAL NumStructs:DWORD
    LOCAL pMobMem:DWORD
    LOCAL NumObjs:DWORD
    LOCAL CollX:DWORD
    LOCAL CollY:DWORD
    LOCAL FONTW:DWORD
    LOCAL FONTH:DWORD
    
    mov edi,ptrMobXY
    push [edi]
    push edi
    mov eax,[edi]  
    mov TmpXY,eax                       ;save Mob's original X,Y
    mov eax,[edi+4]                     ;->char sheet

    mov ebx,[eax+OFFSET_FONTWH]
    shr ebx,16
    mov FONTW,ebx
    mov ebx,[eax+OFFSET_FONTWH]
    and ebx,0ffffh
    mov FONTH,ebx

    mov eax,[eax+OFFSET_MOBMEM]         ;->mob's personal memories
    mov pMobMem,eax
    mov eax,[edi]
    and eax,0ffffh
    mov CollY,eax
    mov ebx,[edi]
    shr ebx,16
    mov CollX,ebx
    invoke CramEAX,wWin,hWin            ;and set out of the way while scanning for obstacles
    mov [edi],eax    

    mov esi,ptrLocalTab
    mov esi,[esi+4]                     ;->MobsTable
    .if (DWORD ptr [esi]==1)            ;if only 1 mob (self) in area then goto StructsTable
        mov esi,ptrLocalTab
        mov esi,[esi+8]                 ;->StructsTable
        mov NumStructs,1
        mov edi,NumStructs
        .if (DWORD ptr [esi+4]==0)
            jmp @NoScan
        .else
@StructScan:
            mov esi,[esi+(edi*4)]       ;->pStruct entry in pStructTable
            mov edi,1
            mov eax,[esi+(edi*8)+4]
            and eax,0ffffh
            mov ebx,[esi+(edi*8)+4]
            shr ebx,16
            shl ebx,1                   ;*2
            sub eax,2
            shl eax,1                   ;*2
            add ebx,eax
            mov NumObjs,ebx
            inc NumObjs
            inc edi                     ;first entry for scan starts at esi+24 in StrucTable
            mov eax,TmpXY
            and eax,0ffffh
            mov CollY,eax
            mov ebx,TmpXY
            shr ebx,16
            mov CollX,ebx

            jmp @F
        .endif
    .else
        mov NumStructs,0
        push [esi]
        pop NumObjs
        mov edi,1                       ;first entry for scan starts at esi+8 in MobsTable

     @@:.if (Mobdir==MobW)
            push CollX
            mov ecx,CollX
            mov eax,FONTW
            shl eax,1                   ;*2
            inc eax
            sub ecx,eax
            mov ebx,CollY
            sub ebx,FONTH
            push ebx
            mov edx,CollY
            add edx,FONTH
        .elseif (Mobdir==MobE)
            mov ecx,CollX
            add ecx,FONTW
            dec ecx
            mov eax,CollX
            mov ebx,FONTW
            shl ebx,1                   ;*2
            dec ebx
            add eax,ebx
            push eax
            mov ebx,CollY
            sub ebx,FONTH
            push ebx
            mov edx,CollY
            add edx,FONTH
        .elseif (Mobdir==MobN)
            mov ecx,CollX
            sub ecx,FONTW
            inc ecx
            mov eax,CollX
            add eax,FONTW
            dec eax
            push eax
            mov ebx,CollY
            mov eax,FONTH
            shl eax,1                   ;*2
            sub ebx,eax
            push ebx
            mov edx,CollY
            sub edx,FONTH
            inc edx
        .elseif (Mobdir==MobS)
            mov ecx,CollX
            sub ecx,FONTW
            inc ecx
            mov eax,CollX
            add eax,FONTW
            dec eax
            push eax
            mov ebx,CollY
            add ebx,FONTH
            dec ebx
            push ebx
            mov edx,CollY
            mov eax,FONTH
            shl eax,1                   ;*2
            add edx,eax
        .endif
        .if(SDWORD ptr ecx<0)
            xor ecx,ecx
        .endif            
        .if(SDWORD ptr edx<0)
            xor edx,edx
        .endif            
        pop ebx
        pop eax
        .if(SDWORD ptr ebx<0)
            xor ebx,ebx
        .endif            
        .if(SDWORD ptr eax<0)
            xor eax,eax
        .endif            
        push eax        
        push ebx

     @@:.REPEAT
            mov ebx,wWin
            shl ebx,16
            mov eax,hWin
            mov bx,ax
            mov eax,[esi+(edi*8)]
            .if(eax==ebx)                       ;found Doorway Coords
                mov eax,[esi+(edi*8)+4]         ;->CSheet
                mov eax,[eax+OFFSET_DOORWAY]    ;load original X|Y coords of wall
            .endif
            mov ebx,eax
            and eax,0ffffh
            mov CollY,eax
            shr ebx,16
            mov CollX,ebx

            pop ebx
            pop eax

            .if ((CollX>ecx)&&(CollX<eax))&&((CollY>ebx)&&(CollY<edx))
                .if (Mobdir==MobW)
                    mov ecx,CollX
                    mov ebx,[TmpXY]
                    shr ebx,16
                    sub ebx,ecx
                    sub ebx,FONTW
                    mov eax,ebx
                .elseif (Mobdir==MobE)
                    mov ecx,CollX
                    mov ebx,[TmpXY]
                    shr ebx,16
                    sub ecx,ebx
                    sub ecx,FONTW
                    mov eax,ecx
                .elseif (Mobdir==MobN)
                    mov ecx,CollY
                    mov ebx,[TmpXY]
                    and ebx,0ffffh
                    sub ebx,ecx
                    sub ebx,FONTH
                    mov eax,ebx
                .elseif (Mobdir==MobS)
                    mov ecx,CollY
                    mov ebx,[TmpXY]
                    and ebx,0ffffh
                    sub ecx,ebx
                    sub ecx,FONTH
                    mov eax,ecx
                .endif
                .if(SDWORD ptr eax<0)
                    xor eax,eax
                .endif
                push  [esi+(edi*8)+4]           ;->char/descrip sheet of encounter
                mov edi,pMobMem
                pop [edi+OFFSET_MOBMEM_ENCCS]   ;save it to personal memory
                jmp @F
            .endif
            
            push eax
            push ebx
            inc edi
        .UNTIL (edi>NumObjs)

        pop eax
        pop eax

        inc NumStructs
        mov edi,NumStructs
;        mov esi,pStructsCurr
        mov esi,ptrLocalTab
        mov esi,[esi+8]                 ;->StructsTable
        .if (DWORD ptr [esi+(edi*4)]>0)
            jmp @StructScan
        .endif                   
    .endif
@NoScan:
    .if (Mobdir==MobN)||(Mobdir==MobS)
        mov eax,FONTH
    .elseif (Mobdir==MobW)||(Mobdir==MobE)
        mov eax,FONTW
    .endif
    mov edi,pMobMem
    mov DWORD ptr [edi+OFFSET_MOBMEM_ENCCS],0  ;no encounter         
 @@:mov [edi+OFFSET_MOBMEM_NUMSTEPS],eax       ;# steps
    pop edi
    pop [edi]

    ret
CollisionDet endp    

CreateNewArea PROC USES eax ebx ecx edx esi edi hWnd:HWND,RoomW:DWORD,RoomH:DWORD,RoomX:DWORD,RoomY:DWORD
;New LocalTable should already be created in WorldTable and pLocalTabCurr updated
;Create new StructTable and structures and new MobTable and population

    LOCAL NumRooms:DWORD
    
    invoke HeapAlloc,hTable,NULL,64 ;create StructTable
    mov [esi+8],eax                 ;save -> in newly created LocalTable
    mov pStructsCurr,eax

    ;convert screen size to bytes
    mov eax,wWin
    shr eax,3                       ;/8 to convert to # bytes needed
    mov ebx,hWin
    mul ebx                         ;total bytes needed
 
    invoke HeapAlloc,hTable,HEAP_ZERO_MEMORY,eax
    mov pScrBin,eax

    invoke nrandom,6
    inc eax
    mov NumRooms,eax
    
    mov edi,1                       ;skip first entry in StructTable, leave undefined

    ;Create new values for Room X,Y,width and height
    ;randW==random # of pixels, range min FONT1W*4 to wWin/2
    ;RoomX==FONT1W to (wWin-FONT1W-randW)
    ;RoomW==randW/FONT1W
    ;
    ;randH==random # of pixels, range min FONT1H*4 to hWin/2
    ;RoomY==FONT1H to (hWin-FONT1H-randH)
    ;RoomH==randH/FONT1H

    .REPEAT
        push edi
   
    @WallNoGo:
        mov eax,wWin
        shr eax,1                   ;/2
        invoke nrandom,eax
        mov ebx,FONT1W
        shl ebx,2                   ;*4
        .if(eax<ebx)
            mov RoomW,ebx           ;min 4 width
        .else
            mov RoomW,eax
        .endif
        mov eax,wWin
        sub eax,FONT1W
        sub eax,RoomW
        invoke nrandom,eax
        .if(eax<FONT1W)
            mov RoomX,FONT1W
            inc RoomX               ;min X coord is FONT1W+1
        .else
            mov RoomX,eax
        .endif                      ;RoomX
        xor edx,edx
        mov eax,RoomW
        mov ebx,FONT1W
        div ebx
        mov RoomW,eax               ;RoomW

        mov eax,hWin
        shr eax,1       ;/2
        invoke nrandom,eax
        mov ebx,FONT1H
        shl ebx,2       ;*4
        .if(eax<ebx)
            mov RoomH,ebx           ;min 4 height
        .else
            mov RoomH,eax
        .endif
        mov eax,hWin
        sub eax,FONT1H
        sub eax,RoomH
        invoke nrandom,eax
        .if(eax<FONT1H)
            mov RoomY,FONT1H
            inc RoomY               ;min Y coord is FONT1H+1
        .else
            mov RoomY,eax
        .endif                      ;RoomY
        xor edx,edx
        mov eax,RoomH
        mov ebx,FONT1H
        div ebx
        mov RoomH,eax               ;RoomH

    ;Begin check for overlapping of any existing structure(s)
        
        sub RoomX,FONT1W            ;adjust dimensions of room to include a perimeter ck automatically
        sub RoomY,FONT1H
        add RoomW,2
        add RoomH,2

        invoke ScrBinCk,RoomX,RoomY,RoomW,RoomH,FONT1W,FONT1H

        .if(eax==1)
            add RoomX,FONT1W        ;restore original room dimensions
            add RoomY,FONT1H
            sub RoomW,2
            sub RoomH,2
        .else
            jmp @WallNoGo
        .endif
        
        pop edi
        push edi

        mov eax,RoomH
        mul RoomW                   ;get RoomH*RoomW
        shl eax,3                   ;*8
        add eax,16
        mov ebx,eax
       
        mov esi,pStructsCurr
        lea esi,[esi+(edi*4)]               ;->next available slot for another pStruct
        invoke HeapAlloc,hTable,NULL,ebx    ;each X,Y slot == 2 DWORDS == 8 bytes
        mov [esi],eax                       ;save->pStruct
        mov DWORD ptr [esi+4],0             ;0 terminate next field in case this is the last entry
        mov esi,eax                         ;->pStruct
        mov edi,1                       
    
        invoke CramEAX,RoomX,RoomY
        mov [esi+(edi*8)],eax
        invoke CramEAX,RoomW,RoomH
        mov [esi+(edi*8)+4],eax
        
        ;Walls

        inc edi                     ;set idx to beginning X|Y of structure
        
        xor ebx,ebx

        .REPEAT                     ;Build N & E walls first, then S & W walls
            push RoomW
            push RoomH
        
            .REPEAT                 ;N wall first time thru loop, S wall second time thru
                dec RoomW
                .if(ebx==1)
                    sub RoomX,FONT1W
                .else
                    add RoomX,FONT1W
                .endif
;                invoke nrandom,2    ;jagged walls
;                .if(eax==0)
;                    dec RoomY
;                .else
;                    inc RoomY                
;                .endif
                invoke CramEAX,RoomX,RoomY
                mov [esi+(edi*8)],eax           ;save X|Y
                invoke HeapAlloc,hTable,NULL,96 
                mov [esi+(edi*8)+4],eax         ;save pCharSheet
                push esi                        
                mov esi,[esi+(edi*8)+4]         ;->Char/Mob Sheet
                mov [esi+OFFSET_NAME],OFFSET Wall_desc
                mov [esi+OFFSET_SYMB],OFFSET Wall_char
                mov DWORD ptr [esi+OFFSET_DOORWAY],0
                mov DWORD ptr [esi+OFFSET_COLOR],COLOR_BRICK
                mov DWORD ptr [esi+OFFSET_VISIBLE],COLOR_MAROON4
                mov DWORD ptr [esi+OFFSET_ALTCOLOR],COLOR_MAROON4
                pop esi
                inc edi
            .UNTIL(RoomW==1)
        
            .REPEAT                 ;E wall first time thru loop, W wall second time thru
                dec RoomH
                .if(ebx==1)
                    sub RoomY,FONT1H
                .else
                    add RoomY,FONT1H
                .endif
;                invoke nrandom,2    ;jagged walls
;                .if(eax==0)
;                    dec RoomX
;                .else
;                    inc RoomX                
;                .endif
                invoke CramEAX,RoomX,RoomY
                mov [esi+(edi*8)],eax           ;save X|Y
                invoke HeapAlloc,hTable,NULL,96 
                mov [esi+(edi*8)+4],eax         ;save pCharSheet
                push esi                        
                mov esi,[esi+(edi*8)+4]         ;->Char/Mob Sheet
                mov [esi+OFFSET_NAME],OFFSET Wall_desc
                mov [esi+OFFSET_SYMB],OFFSET Wall_char
                mov DWORD ptr [esi+OFFSET_DOORWAY],0
                mov DWORD ptr [esi+OFFSET_COLOR],COLOR_BRICK
                mov DWORD ptr [esi+OFFSET_VISIBLE],COLOR_MAROON4
                mov DWORD ptr [esi+OFFSET_ALTCOLOR],COLOR_MAROON4
                pop esi
                inc edi
            .UNTIL(RoomH==1)
        
            pop RoomH
            pop RoomW
            inc ebx
        .UNTIL(ebx==2)

; Doorway Code
; create a doorway on N or S walls and E or W walls
    
        mov edi,1               ;set idx to start of N wall
        invoke nrandom,2        
        .if(eax==1)             ;S wall randomly chosen for a doorway
            add edi,RoomW
            add edi,RoomH
            sub edi,2
        .endif
        mov ebx,RoomW
        sub ebx,2
        invoke nrandom,ebx
        add edi,ebx
        push DWORD ptr [esi+(edi*8)]        ;X|Y
        mov eax,[esi+(edi*8)+4]             ;->CSheet
        pop DWORD ptr [eax+OFFSET_DOORWAY]  ;save original X|Y
        invoke CramEAX,wWin,hWin
        mov [esi+(edi*8)],eax   ;set piece of wall out of the way and offscreen, essentially creating a doorway    

        mov edi,RoomW
        dec edi                 ;set idx to start of E wall
        invoke nrandom,2        
        .if(eax==1)             ;W wall randomly chosen for a doorway
            add edi,RoomW
            add edi,RoomH
            sub edi,2
        .endif
        mov ebx,RoomH
        sub ebx,2
        invoke nrandom,ebx
        add edi,ebx
        push DWORD ptr [esi+(edi*8)]        ;X|Y
        mov eax,[esi+(edi*8)+4]             ;->CSheet
        pop DWORD ptr [eax+OFFSET_DOORWAY]  ;save original X|Y
        invoke CramEAX,wWin,hWin
        mov [esi+(edi*8)],eax   ;set piece of wall out of the way and offscreen, essentially creating a doorway    

        pop edi
        inc edi
    .UNTIL (edi>NumRooms)

; Population code
    
    invoke HeapAlloc,hTable,NULL,256            ;Create new MobsTable
    mov pMobsCurr,eax
    mov esi,pLocalTabCurr
    mov [esi+4],eax                             ;Save to LocalTable (first field Reserved)
    mov esi,eax                                 ;->MobsTable

    invoke nrandom,5
    inc eax
    mov [esi],eax       ;total mobs including self
    push eax
    invoke CramEAX,CharX,CharY
    mov [esi+8],eax
    mov eax,pChar
    mov [esi+12],eax    
    
    mov edi,2
    pop eax
    .if (eax>1)         ;Only populate if more than 1 mob exists
        .REPEAT
         @@:
            mov eax,hWin
            sub eax,FONT1H
            invoke nrandom,eax
            push eax
            mov eax,wWin
            sub eax,FONT1W
            invoke nrandom,eax
            pop edx
            push edx
            push eax
            invoke ScrBinCk,eax,edx,1,1,FONT1W,FONT1H   ;Ck for overlap
            pop ecx
            pop edx
            .if(eax==0)
                jmp @B      ;get new X,Y for mob if overlap occurs on other structures/mobs
            .endif            
            mov ebx,YY  ;get calculations
            rol ebx,16
         @@:
            invoke CramEAX,ecx,edx
            mov [esi+(edi*8)],eax
            invoke HeapAlloc,hTable,NULL,96 ;create ->char sheet for new mob
            mov [esi+(edi*8)+4],eax
            push esi                        ;save (->MobsTable)
            mov esi,[esi+(edi*8)+4]         ;now (->Char/Mob Sheet)
            mov eax,[zombie1.raceidptr]
            mov [esi+OFFSET_NAME],eax       ;have first entry of char sheet contains -> "zombie1" name
            mov eax,[zombie1.symb]
            mov [esi+OFFSET_SYMB],eax   
            mov eax,[zombie1.color]
            rol eax,16
            mov [esi+OFFSET_COLOR],eax  
            mov DWORD ptr [esi+OFFSET_VISIBLE],COLOR_LIGHTGRAY          
            mov DWORD ptr [esi+OFFSET_ALTCOLOR],COLOR_LIGHTGRAY
            mov eax,[zombie1.weaponptr]
            mov [esi+OFFSET_WEAPON],eax
            invoke CramEAX,LGFONT24x36W,LGFONT24x36H
            mov [esi+OFFSET_FONTWH],eax
            push DWORD ptr lgFont24x36
            pop [esi+OFFSET_FONT]
            invoke nrandom,1000
            add eax,1000
            push esi                        ;save (->Char/Mob Sheet)
            mov esi,pWorldTable
            mov ecx,[esi+4]                 ;get current area #
            shl ecx,16
            mov edx,edi
            mov cx,dx
            pop esi                         ;Restore (->Char/Mob Sheet)
            mov [esi+OFFSET_MOB_OBJ_ID],ecx ;MobID
            mov [esi+OFFSET_MOBSPEED],eax
            invoke SetTimer,hWnd,ecx,eax,0
            invoke HeapAlloc,hTable,HEAP_ZERO_MEMORY,128 ;for personal memories
            mov [esi+OFFSET_MOBMEM],eax
            mov DWORD ptr [eax+OFFSET_MOBMEM_ORIGPATH],5
            mov DWORD ptr [eax+OFFSET_MOBMEM_ALTPATH1],5
            mov DWORD ptr [eax+OFFSET_MOBMEM_ALTPATH2],5                
            lea ebx,[eax+OFFSET_MOBMEM_DMGMSG+4]
            mov [eax+OFFSET_MOBMEM_DMGMSG],ebx    
            invoke DiceRoll4,zombie1.attribs
            mov [esi+OFFSET_STATS],eax  ;STR|INT|DEX|CON
            movzx eax,al                    ;isolate CON stat
            push ax
            shl eax,16
            pop ax                          ;Normally HP==CON*LVL
            mov [esi+OFFSET_HPCURRHP],eax   ;HP|Current HP
            mov SDWORD ptr [esi+OFFSET_ALLEGIANCE],-1
            mov DWORD ptr [esi+OFFSET_STATUS],1
            lea ebx,[esi+OFFSET_STATUSMSG+4]
            mov [esi+OFFSET_STATUSMSG],ebx
            pop esi                         ;Restore (->MobsTable)
            inc edi
        .UNTIL edi>[esi]

        invoke HeapFree,hTable,NULL,pScrBin
    .endif
        
    ret
CreateNewArea ENDP

Combat PROC USES ebx ecx edx edi esi pXY:DWORD,pTargetXY:DWORD
    LOCAL TMPHIT:DWORD
    LOCAL CombatXYrc:RECT
        
    mov edi,pXY
    mov edi,[edi+4]     ;->self's char sheet
    mov ebx,pTargetXY
    mov ebx,[ebx+4]     ;->target's char sheet
    mov DWORD ptr [ebx+OFFSET_STATUS],STAT_COMBAT
    mov DWORD ptr [edi+OFFSET_STATUS],STAT_COMBAT

    mov eax,[edi+OFFSET_MOBMEM]
    mov [eax+OFFSET_MOBMEM_COMBATANT],ebx
    mov eax,[ebx+OFFSET_MOBMEM]
    mov [eax+OFFSET_MOBMEM_COMBATANT],edi

    mov eax,[edi+OFFSET_MOB_OBJ_ID]
    and eax,0ffffh
    .if (eax==1)
        mov WordOff_a,OFFSET Word_You
        push [ebx+OFFSET_NAME]
        pop WordOff_b
        mov WordOff_c,OFFSET Word_your
        mov eax,[edi+OFFSET_WEAPON]
        push [eax+16]
        pop WordOff_d

        mov eax,[ebx+OFFSET_MOBSPEED]                       ;Adjust Target mob's speed
        shr eax,3                                           ;/8
        invoke SetTimer,hwnd,[ebx+OFFSET_MOB_OBJ_ID],eax,0  ;and go from casual stroll to RUN FOR YOUR LIFE
    .else
        push [edi+OFFSET_NAME]
        pop WordOff_a
        mov WordOff_b,OFFSET Word_you
        mov WordOff_c,OFFSET Word_his
        mov eax,[edi+OFFSET_WEAPON]
        push [eax+16]
        pop WordOff_d
    .endif

    invoke nrandom,6        ;use a default 1d6 hit roll
    mov edx,[edi+OFFSET_STATS]
    shl edx,16
    shr edx,24              ;isolate DEX STAT to
    sub edx,15              ;calculate +HIT bonus modifier if any
    .if (SDWORD ptr edx>0)
        add eax,edx     
    .endif
    mov TMPHIT,eax
    .if (eax<3)             ;ck if die roll was a miss
        xor ecx,ecx
        mov [DmgMsgs],ecx
        jmp @F
    .endif        

    mov eax,[ebx+OFFSET_HPCURRHP]
    shr eax,16                  ;isolate maxHP
    shr eax,2                   ;/4
    mov [DmgMsgs+16],eax        ;ANNIHALTE
    shr eax,1                   ;/8
    mov [DmgMsgs+32],eax        ;massacre
    shl eax,1                   ;*2
    mov [DmgMsgs+24],eax        ;OBLITERATE
    mov eax,[ebx+OFFSET_HPCURRHP]
    shr eax,16                  ;isolate maxHP
    shr eax,4                   ;/16
    mov [DmgMsgs+40],eax        ;hard

    .if (MouseClick>0)              ;ranged damage
        .if (MouseClick>CHESTSHOT)          ;headshot
            ;make dmg==mob's current hp for insta-kill (headshot)
            mov eax,[ebx+OFFSET_HPCURRHP]
        .else
            mov edx,[edi+OFFSET_WEAPON]
            invoke DiceRoll4,[edx+12]       ;chest shot
            .if (MouseClick<LEGSHOT)        ;leg shot
                ;use reduced dmg roll and slow the mob's movement
                shr eax,1   ;/2
            .endif                
        .endif
        mov MouseClick,0   
    .else                           ;melee damage
        mov edx,[edi+OFFSET_WEAPON] ;->WEAPON TYPE
        invoke DiceRoll4,[edx+12]   ;and compute dmg roll using ->#d#
        mov edx,[edi+OFFSET_STATS]
        shr edx,24                  ;isolate STR STAT
        sub edx,15                  ;to calculate +DAM bonus modifier if any
        .if (SDWORD ptr edx>0)
            add eax,edx        
        .endif
    .endif
    
    mov edx,[ebx+OFFSET_HPCURRHP]
    push edx
    movzx edx,dx
    sub edx,eax
    .if (SDWORD ptr edx<1)      ;target has been slain
        mov DWORD ptr [ebx+OFFSET_STATUS],STAT_FATALITY
        mov DWORD ptr [ebx+OFFSET_COLOR],COLOR_RED   
    .endif
    mov ecx,16
 @@:.if (eax>=[DmgMsgs+ecx])     ;determine which DMG msg to show
        push edi
        push ecx
        mov esi,[DmgMsgs+ecx+4]
        mov edi,[edi+OFFSET_MOBMEM]
        mov edi,[edi+OFFSET_MOBMEM_DMGMSG]
        .REPEAT
            mov al,[esi]
            .if (al=='%')
                cld
                inc esi
                mov al,[esi]
                push esi
                .if (al=='a')
                    mov esi,WordOff_a
                    mov ecx,len(WordOff_a)
                    rep movsb
                .elseif (al=='b')
                    mov esi,WordOff_b
                    mov ecx,len(WordOff_b)
                    rep movsb
                .elseif (al=='c')
                    mov esi,WordOff_c
                    mov ecx,len(WordOff_c)
                    rep movsb
                .elseif (al=='d')
                    mov esi,WordOff_d
                    mov ecx,len(WordOff_d)
                    rep movsb
                .elseif (al=='s')||(al=='S')||(al=='e')
                    mov eax,[edi+OFFSET_MOB_OBJ_ID]
                    and eax,0ffffh
                    .if (eax>1)
                        movsb
                    .endif
                .endif
                pop esi
                inc esi
            .else
                 movsb
            .endif
        .UNTIL (BYTE ptr [esi]==0)              
        mov BYTE ptr [edi],0
        pop ecx
        pop edi
        .if (DWORD ptr [ebx+OFFSET_STATUS]==STAT_FATALITY)
            push edi
            mov edi,[edi+OFFSET_MOBMEM]
    
            mov CombatXYrc.left,0
            mov CombatXYrc.top,450
            push wWin
            pop CombatXYrc.right
            mov CombatXYrc.bottom,500       ;+font height
            push [edi+OFFSET_MOBMEM_DMGMSG]
            pop PAINT_DMGMSG
            invoke InvalidateRect,hwnd,ADDR CombatXYrc,FALSE   ;Need to output last DMGMSG
     
            mov DWORD ptr [edi+OFFSET_MOBMEM_COMBATANT],0
            mov edi,[ebx+OFFSET_MOB_OBJ_ID]
            and edi,0ffffh
            mov PAINT_CHAR,edi

            mov edi,pTargetXY
            mov ecx,[edi]
            and ecx,0ffffh      ;Y
            mov eax,[edi]
            shr eax,16          ;X

            mov CombatXYrc.left,eax
            mov CombatXYrc.top,ecx
            add eax,FONT1W
            mov CombatXYrc.right,eax
            add ecx,FONT1H
            mov CombatXYrc.bottom,ecx       
            
            invoke CramEAX,wWin,hWin        
            mov [edi],eax                               ;move departed XY off screen

            invoke InvalidateRect,hwnd,ADDR CombatXYrc,FALSE
            invoke UpdateWindow,hwnd                    ;clear departed from screen

            pop edi
            mov DWORD ptr [ebx+OFFSET_STATUS],STAT_DEAD
            mov DWORD ptr [edi+OFFSET_STATUS],STAT_FATALITY
            mov ecx,8
            mov [DmgMsgs+8],ecx                         ;Process 'slain' DMGMSG 
            mov eax,ecx
            xor edx,edx
            jmp @B
        .endif
        jmp @F
    .endif
    add ecx,8
    jmp @B
 @@:.if (ecx==0)        ;a miss
        jmp @F
    .endif
    pop eax
    mov ax,dx
    mov [ebx+OFFSET_HPCURRHP],eax
 @@:
    mov ebx,pChar
    mov ebx,[ebx+OFFSET_MOBMEM]
    .if (DWORD ptr [ebx+OFFSET_MOBMEM_DMGMSG]>0)
        mov ebx,[ebx+OFFSET_MOBMEM_DMGMSG]
        mov CombatXYrc.left,0
        mov CombatXYrc.top,450
        push wWin
        pop CombatXYrc.right
        mov CombatXYrc.bottom,500       ;+font height

        mov PAINT_DMGMSG,ebx
        invoke InvalidateRect,hwnd,ADDR CombatXYrc,FALSE

    .endif                              

    ret
Combat ENDP

ThreadProc PROC Param:DWORD
;ThreadProc PROC USES ebx ecx edx edi esi Param:DWORD
; if signalled to start movement
; pick a random direction and move there
    LOCAL XYMobrc:RECT
    LOCAL pMOBMEM:DWORD
    LOCAL pMOBSHEET:DWORD
    LOCAL ThrX:DWORD
    LOCAL ThrY:DWORD
    LOCAL ThrMobID:DWORD
    LOCAL MobXY:DWORD
    LOCAL TargetXY:DWORD
    LOCAL pMOBLocalTab:DWORD
    LOCAL FONTW:DWORD
    LOCAL FONTH:DWORD
    
    invoke WaitForSingleObject,hEventMob,INFINITE

    mov eax,MobID
    mov ThrMobID,eax
    shr eax,16              ;first get area Mob belongs to
    
    mov edi,pWorldTable
    lea edi,[edi+8]         ;move ptr to first WorldX,WorldY entry
    dec eax
    shl eax,4
    mov esi,[edi+eax+8]     ;->LocalTable
    mov pMOBLocalTab,esi
    mov esi,[esi+4]         ;->MobsTable
        
    mov edi,ThrMobID
    and edi,0ffffh          ;get the Mob's ID
    lea edi,[esi+(edi*8)]   ;set edi->Mob's X,Y
    push edi                ;save for visibility ck later

    mov eax,[edi+4]         ;->char sheet
    mov pMOBSHEET,eax
    mov edx,[eax+OFFSET_MOBMEM]
    mov pMOBMEM,edx

    mov edx,[eax+OFFSET_FONTWH]
    shr edx,16
    mov FONTW,edx
    mov edx,[eax+OFFSET_FONTWH]
    and edx,0ffffh
    mov FONTH,edx
            
    .if (DWORD ptr [eax+OFFSET_STATUS]==STAT_COMBAT)
        mov eax,[eax+OFFSET_MOBMEM]
        mov eax,[eax+OFFSET_MOBMEM_COMBATANT]
        mov eax,[eax+OFFSET_MOB_OBJ_ID]
        and eax,0ffffh                  ;isolate ID# to use as index in LocalTable
        lea ecx,[esi+(eax*8)]           
        mov ebx,[ecx]                   ;ebx==target's X,Y
        mov eax,[edi]                   ;eax==mob's X,Y
        mov MobXY,edi
        mov TargetXY,ecx

        mov ecx,FONTH
        sub ax,cx
        sub ax,2
        xor ecx,ecx
        .if (bx<ax)
            mov cx,MobN2    ;target is OOR, due N
            shl ecx,16
            mov cx,ax
            sub cx,bx
            jmp @CkWorE
        .endif
        mov ecx,FONTH
        add ax,cx
        add ax,cx
        add ax,4
        xor ecx,ecx
        .if (bx>ax)
            mov cx,MobS2    ;target is OOR, due S
            shl ecx,16
            mov cx,bx
            sub cx,ax
        .endif
   @CkWorE:
        shr eax,16
        shr ebx,16
        mov edx,FONTW
        sub ax,dx
        sub ax,2
        xor edx,edx
        .if(bx<ax)
            mov dx,MobW2    ;target is OOR, due W
            shl edx,16
            mov dx,ax
            sub dx,bx
            jmp @Pursuit
        .endif
        mov edx,FONTW
        add ax,dx
        add ax,dx
        add ax,4
        xor edx,edx
        .if (bx>ax)
            mov dx,MobE2    ;target is OOR, due E
            shl edx,16
            mov dx,bx
            sub dx,ax
        .endif
   @Pursuit:
        .if (ecx==0)&&(edx==0)  ;target is in range!
            invoke Combat,MobXY,TargetXY
            mov ebx,[edi+4]
            mov ebx,[ebx+OFFSET_MOBMEM]
            .if (DWORD ptr [ebx+OFFSET_MOBMEM_DMGMSG]>0)
                mov ebx,[ebx+OFFSET_MOBMEM_DMGMSG]
                mov XYMobrc.left,0
                mov XYMobrc.top,450
                push wWin
                pop XYMobrc.right
                mov XYMobrc.bottom,500       ;+font height
 
                mov PAINT_DMGMSG,ebx
                invoke InvalidateRect,hwnd,ADDR XYMobrc,FALSE

                mov XYMobrc.left,0
                mov XYMobrc.top,535
                mov XYMobrc.right,700
                mov XYMobrc.bottom,590      
            
                mov PAINT_STATS1,1
                invoke InvalidateRect,hwnd,ADDR XYMobrc,FALSE
            .endif                              
        .else
            ;low word cx and dx == # steps
            ;high word ecx/edx == N or S / E or W
            ;determine which is farther and go that direction

            xor eax,eax
            .if (cx>dx)
                mov eax,1
            .endif
            shr ecx,16
            shr edx,16
            .if (eax==1)
                push ecx
                push edx
            .else
                push edx
                push ecx
            .endif
            mov ecx,pMOBMEM
            pop [ecx+OFFSET_MOBMEM_ALTPATH1]
            pop eax
            mov [ecx+OFFSET_MOBMEM_ORIGPATH],eax
                        
            mov ebx,[edi]
            and ebx,0ffffh
            mov ThrY,ebx
            mov ebx,[edi]
            shr ebx,16
            mov ThrX,ebx

            jmp @F
        .endif
    .elseif (DWORD ptr [eax+OFFSET_STATUS]==STAT_NORMAL)                               
     @MovMe:
        mov eax,[edi]
        and eax,0ffffh
        mov ThrY,eax
        mov ebx,[edi]
        shr ebx,16
        mov ThrX,ebx

        invoke nrandom,4    
     @@:
        push eax
        mov esi,pMOBLocalTab
        invoke CollisionDet,esi,edi,byte ptr [Mobdirs+eax]
        mov edx,pMOBMEM
        mov eax,[edx+OFFSET_MOBMEM_NUMSTEPS]                ;# steps
        pop ebx                                             ;ebx==numeric dir to go (N|E|W|S)

        mov edx,[edi+4]     ;->char sheet

        .if (DWORD ptr [edx+OFFSET_STATUS]==STAT_COMBAT)
            mov edx,pMOBMEM       
            .if (eax==0)                                        ;obstacle in the way
                ;if we're on original path, switch to alternate path
                .if (DWORD ptr [edx+OFFSET_MOBMEM_ORIGPATH]!=5)
                    mov eax,[edx+OFFSET_MOBMEM_ALTPATH1]
                    mov DWORD ptr [edx+OFFSET_MOBMEM_ORIGPATH],5
                    jmp @B
                .endif
                ;if we're already on alternate path, return mob to normal speed and end pursuit
                .if (DWORD ptr [edx+OFFSET_MOBMEM_ORIGPATH]==5)&&(DWORD ptr [edx+OFFSET_MOBMEM_ALTPATH1]!=5)
                    mov DWORD ptr [edx+OFFSET_MOBMEM_ORIGPATH],5
                    mov DWORD ptr [edx+OFFSET_MOBMEM_ALTPATH1],5
                    mov DWORD ptr [edx+OFFSET_MOBMEM_ALTPATH2],5
                    mov edx,pMOBSHEET
                    mov DWORD ptr [edx+OFFSET_STATUS],STAT_NORMAL

                    invoke SetTimer,hwnd,[edx+OFFSET_MOB_OBJ_ID],[edx+OFFSET_MOBSPEED],0  ;return to normal speed
                    
                    jmp @MovMe
                .endif
            .endif
        .endif
        
        mov ecx,ThrX
        mov edx,ThrY

        .if (ebx==0)&&(ecx>FONTW)
            push ThrY
            pop XYMobrc.top
            mov ecx,ThrX
            add ecx,FONTW
            mov XYMobrc.right,ecx
            mov ecx,ThrY
            add ecx,FONTH
            mov XYMobrc.bottom,ecx
            sub ThrX,eax                        
            push ThrX
            pop XYMobrc.left
        .elseif (ebx==1)&&(ecx<wWin)
            push ThrX
            pop XYMobrc.left
            push ThrY
            pop XYMobrc.top
            mov ecx,ThrY
            add ecx,FONTH
            mov XYMobrc.bottom,ecx
            add ThrX,eax
            mov ecx,ThrX
            add ecx,FONTW
            mov XYMobrc.right,ecx
        .elseif (ebx==2)&&(edx>FONTH)
            push ThrX
            pop XYMobrc.left
            mov ecx,ThrX
            add ecx,FONTW
            mov XYMobrc.right,ecx
            mov ecx,ThrY
            add ecx,FONTH
            mov XYMobrc.bottom,ecx
            sub ThrY,eax
            push ThrY
            pop XYMobrc.top
        .elseif (ebx==3)&&(edx<hWin)
            push ThrX
            pop XYMobrc.left
            push ThrY
            pop XYMobrc.top
            mov ecx,ThrX
            add ecx,FONTW
            mov XYMobrc.right,ecx
            add ThrY,eax
            mov ecx,ThrY
            add ecx,FONTH
            mov XYMobrc.bottom,ecx
        .endif

        invoke CramEAX,ThrX,ThrY
        mov [edi],eax

        mov eax,ThrMobID
        shr eax,16              ;get area Mob belongs to
    
        mov edi,pWorldTable
        mov edi,[edi+4]
        cmp edi,eax
        jnz ThreadProc          ;If the mob is not located in the current active area, do nothing else

        mov eax,ThrMobID
        and eax,0ffffh          ;get the Mob's ID
        mov PAINT_CHAR,eax
        invoke InvalidateRect,hwnd,ADDR XYMobrc,FALSE

        invoke UpdateWindow,hwnd

;Check if mob can be seen by char and update mob's [OFFSET_VISIBLE] color as needed

        mov edi,pMobsCurr
        lea edi,[edi+8]                 ;->first X|Y entry (self)
        mov eax,[edi]
        shr eax,16                      ;X
        push eax
        mov XYMobrc.left,eax
        mov eax,SightRadius
        mov ebx,FONTW
        mul ebx
        sub XYMobrc.left,eax            ;==CharX-(FONT1W*Radius)
        .if (SDWORD ptr XYMobrc.left<0)
            mov XYMobrc.left,0
        .endif
        pop XYMobrc.right
        add XYMobrc.right,eax
        mov eax,FONTW
        add XYMobrc.right,eax           ;==CharX+(FONT1W*Radius)+FONT1W
        mov eax,wWin
        .if (DWORD ptr XYMobrc.right>eax)
             mov XYMobrc.right,eax
        .endif
        mov eax,[edi]
        and eax,0ffffh                  ;Y
        push eax
        mov XYMobrc.top,eax
        mov eax,SightRadius
        mov ebx,FONTH
        mul ebx
        sub XYMobrc.top,eax              ;==CharY-(FONT1H*Radius)
        .if (SDWORD ptr XYMobrc.top<0)
            mov XYMobrc.top,0
        .endif
        pop XYMobrc.bottom
        add XYMobrc.bottom,eax
        mov eax,FONTH
        add XYMobrc.bottom,eax       ;==CharY+(FONT1H*Radius)+FONT1H
        mov eax,hWin
        .if (DWORD ptr XYMobrc.bottom>eax)
            mov XYMobrc.bottom,eax
        .endif
        
        pop esi                         ;restore ->mob's X,Y
        mov eax,[esi]           
        shr eax,16                      ;X
        mov edx,eax
        add edx,FONTW
        mov ebx,[esi]           
        and ebx,0ffffh                  ;Y
        mov ecx,ebx
        add ecx,FONTH
        .if (edx>XYMobrc.left)&&(eax<XYMobrc.right)&&(ecx>XYMobrc.top)&&(ebx<XYMobrc.bottom)                    
            ;Char is in sight!
            mov ebx,pMOBSHEET
            push [ebx+OFFSET_COLOR]
            pop [ebx+OFFSET_VISIBLE]
        .else
            ;Char is not in sight
            mov ebx,pMOBSHEET
            mov DWORD ptr [ebx+OFFSET_VISIBLE],COLOR_LIGHTGRAY
        .endif

    .endif

    mov eax,pMOBSHEET
    xor edi,edi
    mov esi,OFFSET STAT_DESC
    @Stat_Ck:
        .if (edi==DWORD ptr [eax+OFFSET_STATUS])
            mov DWORD ptr edi,[eax+OFFSET_STATUSMSG]
            mov ecx,len(esi)
            inc ecx
            rep movsb
        .else
            .REPEAT
                inc esi
            .UNTIL (BYTE ptr [esi]==0)
            inc esi
            inc edi
            jmp @Stat_Ck
        .endif

    jmp ThreadProc    
    ret
ThreadProc ENDP

DiceRoll4 PROC USES ebx ecx edx edi Dices:DWORD
        mov ecx,32
        .REPEAT
            mov ebx,Dices           ;contains (4) individual #d# dice rolls in binary form for stats
            sub ecx,8
            shl ebx,cl
            shr ebx,20              ;bh==1st die
            shr bl,4                ;bl==2nd die
            cmp bl,0
            jz @F                
            movzx edi,bl
            mov bl,bh               ;final stat min==1st die since nrandom can generate a 0
            push ecx
            push eax
            .REPEAT
                invoke nrandom,edi
                add bl,al
                dec bh
            .UNTIL (bh==0)
            pop eax
            pop ecx
         @@:mov al,bl
            ror eax,8
        .UNTIL (ecx==0)
        ret                         ;eax==(4) completed dice rolls
DiceRoll4 ENDP  

PlayMp3File PROC hWnd:DWORD,NameOfFile:DWORD
    LOCAL mciOpenParms:MCI_OPEN_PARMS,mciPlayParms:MCI_PLAY_PARMS
    
    mov eax,hWnd        
    mov mciPlayParms.dwCallback,eax
    mov eax,OFFSET Mp3Device
    mov mciOpenParms.lpstrDeviceType,eax
    mov eax,NameOfFile
    mov mciOpenParms.lpstrElementName,eax
    invoke mciSendCommand,0,MCI_OPEN,MCI_OPEN_TYPE or MCI_OPEN_ELEMENT,ADDR mciOpenParms
    mov eax,mciOpenParms.wDeviceID
    mov Mp3DeviceID,eax
    invoke mciSendCommand,Mp3DeviceID,MCI_PLAY,MCI_NOTIFY,ADDR mciPlayParms
    ret  
PlayMp3File ENDP

ScrBinCk PROC USES ebx esi edi BitX:DWORD,BitY:DWORD,BitW:DWORD,BitH:DWORD,FontW:DWORD,FontH:DWORD
;   IN:     Mob/Object X,Y
;           Width & Height of mob/obj (size is # of fonts)
;           Width & Height of Font used
;   OUT:    eax==0/1 (Coords Invalid/Valid)

    LOCAL StructBitsX:DWORD
    LOCAL StructBitsY:DWORD
    LOCAL WallCk:DWORD

    mov WallCk,0

  @StartBin:
    mov eax,FontH
    mul BitH
    mov StructBitsY,eax

    mov edi,BitY
   
    .REPEAT
        mov esi,pScrBin
        mov eax,wWin            ;adjust esi in reference to RoomY
        shr eax,3               ;/8 get bytes per row
        mul edi          
        lea esi,[esi+eax]       ;-> to starting row
        mov eax,BitX            ;ecx is the starting (1st) bit we need to set
        .if(eax>=32)            ;but if it's over 32, we need to adjust esi
            shr eax,5           ;/32
            lea esi,[esi+(4*eax)]  
            shl eax,5           ;*32
            mov ecx,BitX
            sub ecx,eax
            xchg eax,ecx
        .endif
        mov ecx,32
        sub ecx,eax
        mov eax,FontW
        mul BitW
        mov StructBitsX,eax
        
        ;esi->DWORD that contains the bit that corresponds to X,Y 
        ;ecx==offset to that bit that represents X
     
        .REPEAT    
            mov ebx,[esi]       ;load dword from ScrBin
         @bitck:
            dec ecx             ;offset to bit
            .if(WallCk==0)
                bt ebx,ecx      ;check bit
                jc @NoGo    
            .else
                bts ebx,ecx     ;set bit
            .endif                
            dec StructBitsX
            cmp StructBitsX,0   ;ck # of bits left in X wall
            jz @F           
            cmp ecx,0       
            jnz @bitck          ;keep checking/setting bits all the way to bit 0
         @@:
            .if(WallCk==1)
                mov [esi],ebx   ;save dword with set bits back into ScrBin
            .endif
            mov ecx,32          ;reset offset to bit
            lea esi,[esi+4]     ;->next dword from ScrBin
        .UNTIL(StructBitsX==0)
        inc edi
        dec StructBitsY
    .UNTIL(StructBitsY==0)
      
    .if(WallCk==0)
        mov WallCk,1
        jmp @StartBin
    .endif

  @NoGo:
    mov eax,WallCk
    ret
ScrBinCk ENDP

CramEAX PROC Var1:DWORD,Var2:DWORD
    mov eax,Var2
    push ax
    mov eax,Var1
    shl eax,16
    pop ax
    ret
CramEAX ENDP

end start

