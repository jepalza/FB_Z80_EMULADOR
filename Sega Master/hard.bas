' rutinas de emulacion hardware, en este caso, para un GALAXIAN
Sub inicia_pantalla()

    ' abrimos una de 640x480x32bpp con una copia oculta para dibujar	
	 Screen 18,,2
	
	
	 ' paleta de colores de la Sega MasterSystem/Game Gear	
    Dim X As integer, Y As Integer, bytArray(192) As uByte

    '// Store SMS Palette File, into bytArray.
    Open "prom\sms.pal" For Binary As #1
      Get #1, , bytArray()
    Close #1
     
    '// Convert Palette File to integer Color values.
    Y=0
    For X = 0 To 63
        SMS_PALETTE(X) = RGB(bytArray(Y), bytArray(Y + 1), bytArray(Y + 2))
        Palette X,SMS_PALETTE(X)
        Y = Y + 3
    Next X

End Sub

Sub inicia_teclado()
	 SmsButton=0
	 SmsButton1=255
	 'SmsButton2=0
    Do : loop while inkey$<>""
End Sub

Sub LeeTeclado()

    ' cada vez que entramos, borramos las teclas
    inicia_teclado

    ' escape
    if MultiKey(SC_ESCAPE)  Then end

    ' controles
    if MultiKey(SC_RSHIFT)  then SmsButton=0
    if MultiKey(SC_CONTROL) then SmsButton=0
    if MultiKey(SC_ENTER)   Then SmsButton=0
    if MultiKey(SC_SPACE)   then SmsButton=0

    ' letras
    if MultiKey(SC_A) then SmsButton=0
    if MultiKey(SC_B) then SmsButton=0
    if MultiKey(SC_C) then SmsButton=0
    if MultiKey(SC_D) then SmsButton=0
    if MultiKey(SC_E) then SmsButton=0
    if MultiKey(SC_F) then SmsButton=0
    if MultiKey(SC_G) then SmsButton=0
    if MultiKey(SC_H) then SmsButton=0
    if MultiKey(SC_I) then SmsButton=0
    if MultiKey(SC_J) then SmsButton=0
    if MultiKey(SC_K) then SmsButton=0
    if MultiKey(SC_L) then SmsButton=0
    if MultiKey(SC_M) then SmsButton=0
    if MultiKey(SC_N) then SmsButton=0
    if MultiKey(SC_O) then SmsButton=0
    if MultiKey(SC_P) then SmsButton=0
    if MultiKey(SC_Q) then SmsButton=0
    if MultiKey(SC_R) then SmsButton=0
    if MultiKey(SC_S) then SmsButton=0
    if MultiKey(SC_T) then SmsButton=0
    if MultiKey(SC_U) then SmsButton=0
    if MultiKey(SC_V) then SmsButton=0
    if MultiKey(SC_W) then SmsButton=0
    if MultiKey(SC_X) then SmsButton1 And=Not(16)
    if MultiKey(SC_Y) then SmsButton=0
    if MultiKey(SC_Z) then SmsButton1 And=Not(32)
  
    ' numeros
    if MultiKey(SC_0) then SmsButton=0
    if MultiKey(SC_1) then   inidatos-=100
    if MultiKey(SC_2) then   inidatos+=100 
    if MultiKey(SC_3) Then SmsButton=0
    if MultiKey(SC_4) Then SmsButton=0
    if MultiKey(SC_5) then SmsButton=0
    if MultiKey(SC_6) then SmsButton=0
    if MultiKey(SC_7) then SmsButton=0
    if MultiKey(SC_8) then SmsButton=0
    if MultiKey(SC_9) then SmsButton=0
                                                 
    ' cursores
    If MultiKey(SC_LEFT) Then SmsButton1 And=Not(4)
    If MultiKey(SC_UP)   Then SmsButton1 And=Not(1)
    If MultiKey(SC_RIGHT)Then SmsButton1 And=Not(8)
    If MultiKey(SC_DOWN) Then SmsButton1 And=Not(2)
    
End Sub


Sub ponpantalla()
    Dim e As integer, f As integer, g  As integer, h as Integer
      
    'veces_a_pintar+=1

    'If veces_a_pintar=20 Then 
    '	veces_a_pintar=0
    	h=0
    	For f=0 To SMS_HEIGHT-1
    		For g=0 To SMS_WIDTH-1
    			h+=1
    		   'PSet (g,f),display(h) ' para tamaño real cacurrio en pantallas grandes
    		   Line (g*2,f*2)-Step(1,1),display(h),bf ' para doble tamaño
    		Next
    	Next
    'EndIf

    ScreenCopy 1,0
End Sub


' control de puertos I/O
Function inb(port As integer) as Integer
    Dim inn As integer

    '----------------------------------------~
    '   Not all ports are coded, there's still
    '   Controller 2, Horizontal Port..
    '----------------------------------------~

    Select Case (port And &hff)

    	Case &h7E:     inn = getVCount         '// Vertical Port.
    	Case &hDC:     inn = smsbutton1        '// Controller 1.
    	Case &hC0:     inn = smsbutton1        '// Controller 1. (Mirrored)
    	Case &hBE:     inn = dataRead          '// VDP Data Port.
    	Case &hBF:     inn = controlRead       '// VDP Control Port.
        
    	Case Else:     inn = 255               '// Default Value.

    End Select
    
    Return inn And &hFF
    
End Function

Sub outb(port As integer, outbyte As Integer)

    '----------------------------------------~
    '   Not all ports are coded yet, there's
    '   still Sound, and Auto. Nationalisation.
    '----------------------------------------~
    
    outbyte And=&hFF 
    Select Case (port And &hFF)
    
    	Case &hBE:     dataWrite    (outbyte)  '// VDP Data Port.
    	Case &hBF:     controlWrite (outbyte)  '// VDP Control Port.
    	Case &hBD:     controlWrite (outbyte)  '// VDP Control Port.
    	Case 0 To 5:   dataWrite    (outbyte)  '// GG Serial Ports.

    End Select
    
End Sub
