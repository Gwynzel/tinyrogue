hInstance   HINSTANCE ?
hTable      DWORD   ?
MobID       DWORD   ?
hEventMob   HANDLE  ?
ThreadID    DWORD   ?
buffer      db 128 dup(?)
StatMsg     db 128 dup(?)
hwnd        HWND    ?
YY          DWORD   ?

pChar           DWORD ?
pWorldTable     DWORD ?
pLocalTabCurr   DWORD ?
pStructsCurr    DWORD ?
pMobsCurr       DWORD ?

pScrBin         DWORD ?

mobFont         HFONT ?
dmgmsgFont      HFONT ?
statsFont       HFONT ?
tinyFont15x12   HFONT ?
lgFont24x36     HFONT ?

hWin DWORD ?
wWin DWORD ?

WorldX DWORD ?
WorldY DWORD ?
WorldXW DWORD ?
WorldXE DWORD ?
WorldYN DWORD ?
WorldYS DWORD ?

XYrc RECT <?,?,?,?>

WordOff_a DWORD ?
WordOff_b DWORD ?
WordOff_c DWORD ?
WordOff_d DWORD ?

TS_MSG1 DWORD ?
TS_MSG2 DWORD ?
TS_MSG3 DWORD ?

BgX     DWORD   ?
BgY     DWORD   ?
BgXscr  DWORD   ?
BgYscr  DWORD   ?

BitmapInfo  BITMAPINFOHEADER <>
BitmapFile  BITMAPFILEHEADER <>
pBitmapBits     DWORD   ?
BackgroundBMP   DWORD   ?

