ClassName db "WinClassName",0
AppName db "WinAppName",0
TYPEFACE db "Courier New",0
char WPARAM 20h

CharX       DWORD 400
CharY       DWORD 400
MouseX      DWORD   0
MouseY      DWORD   0
MouseClick  DWORD   0
Targeted    DWORD   0
SightRadius DWORD 0
Mobdirs db "adws"
Wall_desc db "a wall",0
Wall_char   DWORD 177

;Bg_stones   db " ., ;' ",'. '
;bmpext      db ".bmp",0

;Bgbmpfile2 db "hmm.bmp",0

StatBar db "STR:%  INT:%  DEX:%  CON:%  HP:%h/%H # Areas:%a Curr Area:%A # Mobs:%m Mob HP:%x/%X",0

;Races
;ptr to name of race, symbol, color, STR|INT|DEX|CON, default weapon

human1 STATS <MOBTYPE_text1,'@',COLOR_KHAKI,01100011011000110110001101100011b,revolver1>    ;6d3 all stats
zombie1 STATS <MOBTYPE_text2,'Z',COLOR_LIGHTGRAY,01000100010001000100010001000100b,barehanded1>  ;4d4 all stats

MOBTYPE_text1 db "a human",0
MOBTYPE_text2 db "a zombie",0

;Weapons
;ptr to name of weapon, symbol, color, dmg dice, weapon dmg text

barehanded1 DWORD WEAPONTYPE_text1a,'|',COLOR_SNOWWHITE,00010011b,WEAPONTYPE_text1b    ;1d3
dagger1 DWORD WEAPONTYPE_text2a,'|',COLOR_SNOWWHITE,00010100b,WEAPONTYPE_text2b        ;1d4
longsword1 DWORD WEAPONTYPE_text3a,'|',COLOR_SNOWWHITE,00010110b,WEAPONTYPE_text3b     ;1d6
revolver1 DWORD WEAPONTYPE_text4a,'+',COLOR_SNOWWHITE,00010110b,WEAPONTYPE_text4b           ;1d6

WEAPONTYPE_text1a db "barehanded",0
WEAPONTYPE_text1b db "punch",0
WEAPONTYPE_text2a db "a dagger",0
WEAPONTYPE_text2b db "pierce",0
WEAPONTYPE_text3a db "a longsword",0
WEAPONTYPE_text3b db "slash",0
WEAPONTYPE_text4a db "a revolver",0
WEAPONTYPE_text4b db "blast",0
 
DmgMsgs DWORD 0,DmgMsg6,0,DmgMsg7,0,DmgMsg1,0,DmgMsg2,0,DmgMsg3,0,DmgMsg4,0,DmgMsg5

DmgMsg1 db "%a ANNIHILATE%S %b with %c %d!",0
DmgMsg2 db "%a OBLITERATE%S %b with %c %d!",0
DmgMsg3 db "%a massacre%s %b into tiny little pieces with %c %d!",0
DmgMsg4 db "%a %d%e%s %b very hard.",0
DmgMsg5 db "%a scratch%e%s %b with %c %d.",0
DmgMsg6 db "%a miss%e%s %b with %c %d.",0
DmgMsg7 db "%a KILLED %b!",0

Text_bs     db "backstab",0
Text_snatk  db "sneak attack",0

Word_You db "You",0
Word_you db "you",0
Word_your db "your",0
Word_his db "his",0

PAINT_BACKGROUND    DWORD   0
PAINT_CHAR          DWORD 0
PAINT_DMGMSG        DWORD 0
PAINT_STRUCTURE     DWORD 0
PAINT_STATS1        DWORD 0
PAINT_STATS2        DWORD 0

STAT_DESC db "DEAD",0,"NORMAL",0,"COMBAT",0,"FATALITY",0,"SNEAK",0

DebugText1  db  "# areas:",0
DebugText2  db  "active area #:",0
DebugText3  db  "# mobs:",0
DebugText4  db  "pStruct ADDR:",0
DebugText5  db  "Status     Area MobID (X,Y)",0

LegShotTxt      db  "Leg shot",0
ChestShotTxt    db  "Chest shot",0
HeadShotTxt     db  "HEADSHOT!",0

Mp3DeviceID dd 0
Mp3Device   db "MPEGVideo",0
gunshot1    db "C:\lab\asm\gun-gunshot-01.mp3",0
;gunshot1    db "C:\lab\asm\machine-gun-02.mp3",0
