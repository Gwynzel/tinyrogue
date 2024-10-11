Scr_Width   equ 1000  
Scr_Height  equ 545

FONT1W           equ 16
FONT1H           equ 24
TINYFONT15x12W   equ 15
TINYFONT15x12H   equ 12
LGFONT24x36W     equ 16
LGFONT24x36H     equ 24

;Char sheet offsets
OFFSET_MOB_OBJ_ID   equ 0
OFFSET_MOBSPEED     equ 4
OFFSET_NAME         equ 8
OFFSET_SYMB         equ 12
OFFSET_COLOR        equ 16
OFFSET_ALTCOLOR     equ 20
OFFSET_VISIBLE      equ 24
OFFSET_MOBMEM       equ 28
    OFFSET_MOBMEM_COMBATANT equ 0
    OFFSET_MOBMEM_NUMSTEPS  equ 4
    OFFSET_MOBMEM_ENCCS     equ 8
    OFFSET_MOBMEM_ORIGPATH  equ 12
    OFFSET_MOBMEM_ALTPATH1  equ 16
    OFFSET_MOBMEM_ALTPATH2  equ 20
    OFFSET_MOBMEM_DMGMSG    equ 24
OFFSET_FONTWH       equ 32
OFFSET_FONT         equ 36
OFFSET_STATS        equ 40
OFFSET_HPCURRHP     equ 44
OFFSET_ALLEGIANCE   equ 48
OFFSET_WEAPON       equ 52
OFFSET_DOORWAY      equ 56
OFFSET_BgXY         equ 60
OFFSET_STATUS       equ 64
OFFSET_STATUSMSG    equ 68

;(Reverse hex codes obtained from charts)
COLOR_SNOWWHITE     equ 0c9c9cdh
COLOR_CHARTREUSE    equ 00ff7fh
COLOR_MAROON4       equ 003399h
COLOR_SLATEGRAY     equ 708090h
COLOR_DARKSLATEGRAY equ 2f4f4fh
COLOR_BRICK         equ 2395FFh
COLOR_RED           equ 0ffh
COLOR_BLACK         equ 00h
COLOR_GREEN         equ 0ff00h
COLOR_TEST          equ 0CC6600h
COLOR_DARKGRAY      equ 333333h
COLOR_LIGHTGRAY     equ 0CCCCCCh
COLOR_KHAKI         equ 08CE6F0h

WM_CREATEWORLD equ WM_USER+100h

STAT_DEAD           equ 0
STAT_NORMAL         equ 1
STAT_COMBAT         equ 2
STAT_FATALITY       equ 3

MobN equ "w"
MobE equ "d"
MobW equ "a"
MobS equ "s"

MobN2 equ 2
MobE2 equ 1
MobW2 equ 0
MobS2 equ 3

LEGSHOT     equ 12
CHESTSHOT   equ 19
HEADSHOT    equ 20
