.586
.model flat,stdcall
option casemap:none

include \masm32\include\windows.inc
include \masm32\include\user32.inc
include \masm32\include\kernel32.inc
include \masm32\include\comctl32.inc
include \masm32\include\gdi32.inc
includelib \masm32\lib\user32.lib
includelib \masm32\lib\kernel32.lib
includelib \masm32\lib\comctl32.lib
includelib \masm32\lib\gdi32.lib
include \masm32\include\masm32.inc      
include \masm32\macros\macros.asm       
includelib \masm32\lib\masm32.lib
include \masm32\include\winmm.inc
includelib \masm32\lib\winmm.lib

STATS STRUCT
    raceidptr   DWORD ?    
    symb        DWORD ?
    color       DWORD ?
    attribs     DWORD ?     ;STR INT DEX CON
    weaponptr   DWORD ?
STATS ENDS   

