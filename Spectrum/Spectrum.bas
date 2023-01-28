' rutinas de emulacion hardware del Spectrum 48k unicamente

Sub LeeTeclado()

    ' cada vez que entramos, borramos las teclas
    ResetKeyboard
    if MultiKey(SC_ESCAPE) then END

    ' *********************************************************
    ' teclas especiales combinadas, del teclado PC
    ' estas teclas se obtienen tambien en modo Spectrum
    ' pero es mas comodo en modo PC
    ' "
    if Multikey(SC_LSHIFT) and Multikey(SC_2) then
        keyY_P = (keyY_P And (Not 1&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' =
    if Multikey(SC_LSHIFT) and Multikey(SC_0) then
        keyH_ENT = (keyH_ENT And (Not 2&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' $
    if Multikey(SC_LSHIFT) and Multikey(SC_4) then
        key1_5 = (key1_5 And (Not 8&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' /
    if Multikey(SC_LSHIFT) and Multikey(SC_7) then
        keyCAPS_V = (keyCAPS_V And (Not 16&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' (
    if Multikey(SC_LSHIFT) and Multikey(SC_8) then
        key6_0 = (key6_0 And (Not 4&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' )
    if Multikey(SC_LSHIFT) and Multikey(SC_9) then
        key6_0 = (key6_0 And (Not 2&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' +
    if Multikey(SC_LSHIFT) and Multikey(SC_RIGHTBRACKET) then
        keyH_ENT = (keyH_ENT And (Not 4&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' _ (subrayado)
    if Multikey(SC_LSHIFT) and Multikey(SC_SLASH) then
        key6_0 = (key6_0 And (Not 1&)) 
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' -
    if Multikey(SC_SLASH) then
        keyH_ENT = (keyH_ENT And (Not 8&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' *
    if Multikey(SC_RIGHTBRACKET) then
        keyB_SPC = (keyB_SPC And (Not 16&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' ^
    if Multikey(SC_LEFTBRACKET) then
        keyH_ENT = (keyH_ENT And (Not 16&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' ;
    if Multikey(SC_LSHIFT) and Multikey(SC_COMMA) then
        keyY_P = (keyY_P And (Not 2&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' ,
    if Multikey(SC_COMMA) then
        keyB_SPC = (keyB_SPC And (Not 8&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' :
    if Multikey(SC_LSHIFT) and Multikey(SC_PERIOD) then
        keyCAPS_V = (keyCAPS_V And (Not 2&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' .
    if Multikey(SC_PERIOD) then
        keyB_SPC = (keyB_SPC And (Not 4&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' !
    if Multikey(SC_LSHIFT) and Multikey(SC_1) then
        key1_5 = (key1_5 And (Not 1&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' %
    if Multikey(SC_LSHIFT) and Multikey(SC_5) then
        key1_5 = (key1_5 And (Not 16&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' &
    if Multikey(SC_LSHIFT) and Multikey(SC_6) then
        key6_0 = (key6_0 And (Not 16&))
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
    ' ?
    if Multikey(SC_LSHIFT) and Multikey(SC_MINUS) then
        keyCAPS_V = (keyCAPS_V And (Not 8&)) 
        keyB_SPC = (keyB_SPC And (Not 2&))
        exit sub
    end if
   ' *********************************************************
 

    ' teclas exclusivas del Spectrum

    if MultiKey(SC_RSHIFT)  then keyCAPS_V = (keyCAPS_V And (Not 1&))
    if MultiKey(SC_CONTROL) then keyB_SPC = (keyB_SPC And (Not 2&))

    ' combinada CAPS+0 = Backspace
    if MultiKey(SC_BACKSPACE) then 
        key6_0 = (key6_0 And (Not 1&))
        keyCAPS_V = (keyCAPS_V And (Not 1&))
    End If

    if MultiKey(SC_ENTER) then keyH_ENT = (keyH_ENT And (Not 1&))
    if MultiKey(SC_SPACE) then keyB_SPC = (keyB_SPC And (Not 1&))

    if MultiKey(SC_A) then keyA_G = (keyA_G And (Not 1&))
    if MultiKey(SC_B) then keyB_SPC = (keyB_SPC And (Not 16&)) 
    if MultiKey(SC_C) then keyCAPS_V = (keyCAPS_V And (Not 8&)) 
    if MultiKey(SC_D) then keyA_G = (keyA_G And (Not 4&))
    if MultiKey(SC_E) then keyQ_T = (keyQ_T And (Not 4&))
    if MultiKey(SC_F) then keyA_G = (keyA_G And (Not 8&)) 
    if MultiKey(SC_G) then keyA_G = (keyA_G And (Not 16&)) 
    if MultiKey(SC_H) then keyH_ENT = (keyH_ENT And (Not 16&))
    if MultiKey(SC_I) then keyY_P = (keyY_P And (Not 4&)) 
    if MultiKey(SC_J) then keyH_ENT = (keyH_ENT And (Not 8&)) 
    if MultiKey(SC_K) then keyH_ENT = (keyH_ENT And (Not 4&))
    if MultiKey(SC_L) then keyH_ENT = (keyH_ENT And (Not 2&))
    if MultiKey(SC_M) then keyB_SPC = (keyB_SPC And (Not 4&))
    if MultiKey(SC_N) then keyB_SPC = (keyB_SPC And (Not 8&)) 
    if MultiKey(SC_O) then keyY_P = (keyY_P And (Not 2&))
    if MultiKey(SC_P) then keyY_P = (keyY_P And (Not 1&))
    if MultiKey(SC_Q) then keyQ_T = (keyQ_T And (Not 1&))
    if MultiKey(SC_R) then keyQ_T = (keyQ_T And (Not 8&))
    if MultiKey(SC_S) then keyA_G = (keyA_G And (Not 2&))
    if MultiKey(SC_T) then keyQ_T = (keyQ_T And (Not 16&))
    if MultiKey(SC_U) then keyY_P = (keyY_P And (Not 8&))
    if MultiKey(SC_V) then keyCAPS_V = (keyCAPS_V And (Not 16&))
    if MultiKey(SC_W) then keyQ_T = (keyQ_T And (Not 2&))
    if MultiKey(SC_X) then keyCAPS_V = (keyCAPS_V And (Not 4&))
    if MultiKey(SC_Y) then keyY_P = (keyY_P And (Not 16&))
    if MultiKey(SC_Z) then keyCAPS_V = (keyCAPS_V And (Not 2&))
  
    if MultiKey(SC_0) then key6_0 = (key6_0 And (Not 1&)) 
    if MultiKey(SC_1) then key1_5 = (key1_5 And (Not 1&))
    if MultiKey(SC_2) then key1_5 = (key1_5 And (Not 2&))
    if MultiKey(SC_3) then key1_5 = (key1_5 And (Not 4&))
    if MultiKey(SC_4) then key1_5 = (key1_5 And (Not 8&))
    if MultiKey(SC_5) then key1_5 = (key1_5 And (Not 16&))
    if MultiKey(SC_6) then key6_0 = (key6_0 And (Not 16&))
    if MultiKey(SC_7) then key6_0 = (key6_0 And (Not 8&))
    if MultiKey(SC_8) then key6_0 = (key6_0 And (Not 4&))
    if MultiKey(SC_9) then key6_0 = (key6_0 And (Not 2&))


     ' cursores
        If MultiKey(SC_LEFT) Then
            key1_5 = (key1_5 And (Not 16&))
            keyCAPS_V = (keyCAPS_V And (Not 1&))
        End If

        If MultiKey(SC_UP) Then
            key6_0 = (key6_0 And (Not 8&))
            keyCAPS_V = (keyCAPS_V And (Not 1&))
        End If

        If MultiKey(SC_RIGHT) Then
            key6_0 = (key6_0 And (Not 4&))
            keyCAPS_V = (keyCAPS_V And (Not 1&))
        End If

        If MultiKey(SC_DOWN) Then
            key6_0 = (key6_0 And (Not 16&))
            keyCAPS_V = (keyCAPS_V And (Not 1&))
        End If

End Sub

' refresca los caracteres FLASH
Sub refreshFlashChars()
    Dim addr As long, lne As long, i As long

    'bFlashInverse = Not (bFlashInverse)
    
    For addr = 6144& To 6911&
        If RAM(16384+addr) And 128& Then
            RAM(16384+addr)=RAM(16384+addr) xor &h7f
            'lne = Div32(addr - 6144&)
            'For i = lne * 8& To lne * 8& + 7&
            '    ScrnLines(i, 32&) = True
            '    ScrnLines(i, addr And 31&) = True
            'Next i
        End If
    Next addr

End Sub

Sub ScanlinePaint(lne As long)    
    Dim lLneIndex As long, lColIndex As long, X As long, sbyte As long, abyte As long, lIndex As long
        
    'If ScrnLines(lne, 32&) = True Then
        If lne < glTopMost Then glTopMost = lne
        If lne > glBottomMost Then glBottomMost = lne
        lLneIndex = glRowIndex(lne)
        lColIndex = glColIndex(lne)
        For X = 0& To 31&
            'If ScrnLines(lne, X) = True Then
                If X < glLeftMost Then glLeftMost = X
                If X > glRightMost Then glRightMost = X
                sbyte = RAM(16384+glScreenMem(lne, X))
                abyte = RAM(16384+6144+lLneIndex + X)
                If (abyte And 128&) And (bFlashInverse) Then
                    ' // Swap fore- and back-colours
                    abyte = abyte Xor 128&
                End If
               
                lIndex = (lColIndex + X + X)
                glBufferBits(lIndex) = gtBitTable(sbyte, abyte).dw0
                glBufferBits(lIndex + 1&) = gtBitTable(sbyte, abyte).dw1
                
                ScrnLines(lne, X) = False
                SpectrumScreen(lne, X)=sbyte
            'End If
        Next X
        ScrnLines(lne, 32&) = False ' // Flag indicates this line has been rendered on the bitmap
        ScrnNeedRepaint = True
    'End If
End Sub

Sub screenPaint()
    ' // Only update screen if necessary
    'If ScrnNeedRepaint = False Then Exit Sub
    
    glLeftMost = glLeftMost * 8&
    glRightMost = glRightMost * 8&
    
    glTopMost = 191&
    glBottomMost = 0&
    glLeftMost = 31&
    glRightMost = 0&
    
    'ScrnNeedRepaint = False

    DIM I as long, m as long, a as ubyte, b as ubyte
    Dim X As long, y As long,f as long, h as long,g as long

    DIM INK AS INTEGER, PAPER AS INTEGER, BRIGHT AS INTEGER, FLASH AS INTEGER
    dim XX AS INTEGER, YY AS INTEGER, FF AS INTEGER
 
    ' se mira si es necesario repintar el borde
    if glLastBorder<>glNewBorder then
      line (0,0)   -step(640,48), glNewBorder,bf
      line (0,48)  -step(64,384), glNewBorder,bf
      line (576,48)-step(64,384), glNewBorder,bf
      line (0,432) -step(640,48), glNewBorder,bf
      glLastBorder = glNewBorder
    end if

    x=0:y=0
    For i = 0 To 6143 
           f=1
           a = SpectrumScreen(y,x)
           FF = RAM(16384+6144+((y\8)*32)+x)
           INK  = FF AND 7
           PAPER= (FF AND 56) \ 8
           BRIGHT= FF AND 64
           FLASH= FF AND 128
           for m=(7*x2) to 0 step (-1*x2)
              b=(a and f)
              if b then h=INK+BRIGHT else h=PAPER+BRIGHT

              if x2=2 then 
                Line (((x*x2)*8)+m+(32*x2),(y*x2)+(24*x2))-step(1,1),h,b
               else
                Pset (((x*x2)*8)+m+(32*x2),(y*x2)+(24*x2)),h
              end if  
              
              f=f*2
           next m
           x=x+1:if x=32 then x=0:y=y+1
    Next i

    ' una vez dibujada oculta, la volcamos a la visible
    screencopy 1,0
End Sub



