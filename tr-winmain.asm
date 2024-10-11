start:
    invoke GetModuleHandle,NULL
    mov hInstance,eax
    invoke WinMain,hInstance,NULL,NULL,SW_SHOWDEFAULT
    invoke ExitProcess,eax

WinMain proc hInst:HINSTANCE,hPrevInst:HINSTANCE,CmdLine:LPSTR,CmdShow:DWORD
    LOCAL wc:WNDCLASSEX
    LOCAL msg:MSG
    LOCAL rc:RECT
    
    mov wc.cbSize,SIZEOF WNDCLASSEX
    mov wc.style,CS_HREDRAW or CS_VREDRAW
    mov wc.lpfnWndProc,OFFSET WndProc
    mov wc.cbClsExtra,NULL
    mov wc.cbWndExtra,NULL
    push hInst
    pop wc.hInstance
    
    invoke CreateSolidBrush,COLOR_BLACK
    mov wc.hbrBackground,eax
    mov wc.lpszMenuName,NULL
    mov wc.lpszClassName,OFFSET ClassName
    invoke LoadIcon,NULL,IDI_APPLICATION
    mov wc.hIcon,eax
    mov wc.hIconSm,eax
    invoke LoadCursor,NULL,IDC_ARROW
    mov wc.hCursor,eax
    invoke RegisterClassEx,addr wc

    mov rc.left,0
    mov rc.top,0
    mov rc.right,Scr_Width
    mov rc.bottom,Scr_Height
    invoke GetSystemMetrics,SM_CXFIXEDFRAME
    shl eax,1
    add rc.right,eax
    invoke GetSystemMetrics,SM_CYFIXEDFRAME
    shl eax,1
    add rc.bottom,eax
    invoke GetSystemMetrics,SM_CYCAPTION
    add rc.bottom,eax
    invoke GetSystemMetrics,SM_CXSCREEN
    sub eax,Scr_Width
    shr eax,1
    mov rc.left,eax
    invoke GetSystemMetrics,SM_CYSCREEN
    sub eax,Scr_Height
    shr eax,1
    mov rc.top,eax    
    
    invoke CreateWindowEx,NULL,ADDR ClassName,ADDR AppName,WS_VISIBLE+WS_SYSMENU+WS_DLGFRAME+WS_MINIMIZEBOX+WS_MAXIMIZEBOX,rc.left,rc.top,rc.right,rc.bottom,NULL,NULL,hInst,NULL
    mov hwnd,eax
    
    .WHILE TRUE
        invoke GetMessage,ADDR msg,NULL,0,0
    .BREAK .IF(!eax)
        invoke TranslateMessage,ADDR msg
        invoke DispatchMessage,ADDR msg
    .ENDW
    
    mov eax,msg.wParam
    ret
WinMain endp
