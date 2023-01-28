'=========================================================================+
'                             vbSMS Todo List                             |
'=========================================================================+
'
'   Make changes to incorporate GGear roms, this will involve detecting if
'   a certain rom is GG and using different VDP Palette and Display Size.
'
'   Fix existing bugs, SRAM Writes are Buggy "The Flash"
'   Vdp.GetVCount > is this function 100% correct "Spiderman Sinister 6"?
'   Add other VDP Modes "F-16 Fighter".
'   Fix CPU errors, (Doesn't pass ZEXALL).
'   Fix "Back to the Future III" (PAL Timing), maybe Spiderman Sinister 6.
'   Extended display - "Not only words".
'   Add Codemaster Mappers.
'   "Chuck Rock" Has incorrect colors at the start?
'   "Galactic Protector" Does the controller work?
'
'   Pause Button, Sound, Interface.
'   Improve Speed, Add CPU timing (So emulation runs at correct speed).
'   Are lookup tables any faster than the actually math?
'
'=========================================================================+
'                  [vbSMS, by John Casey <xshifu@msn.com>]                |
'=========================================================================+





'--------------~
'   Reset VDP.
'--------------~
Sub inicia_VDP()

    first_byte = cierto
       lineint = falso
      frameint = falso
      location = 0
       counter = 0
        status = 0
    
     vdpreg(2) = 14                            'SMS Bios Value.
     vdpreg(5) = 126                           'SMS Bios Value.
     vdpreg(6) = 255                           'SMS Bios Value.
    vdpreg(10) = 1                             'SMS Bios Value.

    irqsetLine = falso
    
    Dim X               As Integer

    smsbutton1 = 255                  '// Default Controller Value.
    frame_two_rom = cierto
    
    '// Calculate Page Lookup. Upto 1MB.
    For X = 0 To 64
        Mul4000(X) = X * &h4000
    Next X

End Sub

'---------------------~
'   Read Vertical Port.
'---------------------~
Function getVCount() As Integer

     If (linenum > &hDA) Then getVCount = linenum - 6 Else getVCount = linenum
     
End Function

'---------------------------------~
'   Write to VDP Control Port (BF).
'---------------------------------~
Sub controlWrite(value As Integer)

    value And=&hFF
    '// Store First Byte of Command Word
    If (first_byte) Then
    
        first_byte = falso
        command_byte = value
        Exit Sub
        
    End If
    
        first_byte = cierto
        
        '// Set VDP Register
        If (value shr 4) = 8 Then
            
            vdpreg(value And 15) = command_byte

            If (lineint) Then
                If (((value And 15) = 0) And ((command_byte And 16) = 0)) Then
                    irqsetLine = falso
                ElseIf (((value And 15) = 0) And ((command_byte And 16) = 16)) Then
                    irqsetLine = cierto
                End If
            End If

            If (frameint) Then
                If (((value And 15) = 1) And ((command_byte And 32) = 0)) Then
                    irqsetLine = falso
                ElseIf (((value And 15) = 1) And ((command_byte And 32) = 32)) Then
                    irqsetLine = cierto
                End If
            End If
                
        Else

            '// Operation from B6 + B7
            operation = value Shr 6
            '// Set location in VRAM
            location = command_byte + ((value And 63) shl 8)
            
            If (operation = 0) Then
                
                read_buffer = VRAM(location)
                If (location > 16383) Then location = 0 Else location = location + 1
            
            End If
        End If
     
End Sub

'------------------------------~
'   Read VDP Control Port (BF).
'------------------------------~
Function controlRead() As Integer

    first_byte = cierto
    
    Dim statuscopy As Integer
    statuscopy = status
    
    status = status And Not &h80 And Not &h40 And Not &h20
    
    '// Clear pending interrupts
    lineint = falso
    frameint = falso
    
    '// Clear IRQ Line
    irqsetLine = falso
    
    controlRead = statuscopy

End Function

'------------------------------~
'   Write to VDP Data Port (BE).
'------------------------------~
Sub dataWrite(value As Integer)

    value And=&hFF
    first_byte = cierto
    
    Select Case (operation)
        
        '           // VRAM Write
    	Case 0 To 2:  VRAM(location) = value
        
        '           // CRAM Write
    	Case 3
                    CRAM(location And &h1F) = value And &h3f 'SMS_PALETTE(value And &h3F)
                    'Palette location And &h1F,SMS_PALETTE(value And &h3F)
                    
        'Case Else:  Debug.Print "Unknown dataWrite "  Hex(operation)
    End Select

    If (location < 16383) Then location = location + 1 Else location = 0
    
End Sub

'---------------------------~
'   Read VDP Data Port (BE).
'---------------------------~
Function dataRead() As Integer

    first_byte = cierto
    
    'VRAM Read
    If (operation < 2) Then
        dataRead = read_buffer
        read_buffer = VRAM(location)
    End If
    
    If (location < 16383) Then location = location + 1 Else location = 0
                        
End Function

'----------------------------------~
'   Render Line of Background Layer.
'----------------------------------~
Sub drawBg(lineno As Integer)

    Dim X               As Integer
    Dim Y               As Integer:    Y = lineno
    Dim bgt             As Integer:    bgt = ((vdpreg(2) And 15 And Not 1) shl 10)
    Dim pattern         As Integer
    Dim pal             As Integer
    Dim hscroll         As Integer
    Dim start_column    As Integer
    Dim start_row       As Integer
    Dim tilex           As Integer
    Dim tiley           As Integer
    Dim tile_props      As Integer
    Dim firstbyte       As Integer
    Dim secondbyte      As Integer
    Dim priority        As Integer
    Dim addr         As Integer
    Dim xpos            As Integer
    Dim row_precal      As Integer
    Dim addr0        As Integer
    Dim addr1        As Integer
    Dim addr2        As Integer
    Dim addr3        As Integer
    Dim bita            As Integer
    Dim a               As Integer
    Dim colour          As Integer


    '// Vertical Scroll Fine Tune
    If (Not vdpreg(9) = 0) Then lineno = lineno + (vdpreg(9) And 7)


    '// Vertical Scroll; Row of Tile to Plot.
    start_row = lineno: tiley = (lineno And 7)


    '// Top Two Rows Not Affected by Horizontal Scrolling (SMS Only)
    If (((vdpreg(0) And 64) = 64) And (Y < 16)) Then
        hscroll = 0
    ElseIf (Not vdpreg(8) = 0) Then
        hscroll = vdpreg(8) And 7
        start_column = 32 - (vdpreg(8) shr 3)
    End If


    'If (setup.is_gg) Then... [Add coding for GameGear Here]


    '// Adjust Vertical Scroll
    If (Not vdpreg(9) = 0) Then
        start_row = start_row + (vdpreg(9) And 248)
        If (start_row > 223) Then start_row = start_row - 224
    End If


    '// Cycle through background table
    For X = H_START To H_END Step 8
    
    
        '// wraps at 32
        If (start_column = 32) Then start_column = 0
        
        
        '// Rightmost 8 columns Not Affected by Vertical Scrolling
        If (((vdpreg(0) And 128) = 128) And (X > 184)) Then
            start_row = Y
            tiley = (Y And 7)
        End If
        

        '// Get the two bytes from VRAM containing the tile's properties
        tile_props = bgt + (start_column shl 1) + ((start_row - tiley) shl 3)
        firstbyte  = VRAM(tile_props) ' And 255
        secondbyte = VRAM(tile_props + 1) ' And 255
        
        
        '// Priority of tile
        priority = secondbyte And 16
        
        
        '// Select Palette (Extended Colors).
        If ((secondbyte And 8) = 8) Then pal = 16 Else pal = 0
        
        
        '// Pattern Number
        pattern = ((firstbyte + ((secondbyte And 1) Shl 8)) shl 5)
        

        '// Vertical Tile Flip
        If ((secondbyte And 4) = 0) Then addr = (tiley shl 2) + pattern Else addr = ((7 - tiley) Shl 2) + pattern
        
        
        '// Rowcount
        tilex = 0
        
        
        '// Precalculate Y Position
        row_precal = (Y Shl 8)


        addr0 = VRAM(addr)
        addr1 = VRAM(addr + 1)
        addr2 = VRAM(addr + 2)
        addr3 = VRAM(addr + 3)


        '//Plots row of 8 pixels
        bita = 128
        
        Do
            '// Horizontal Tile Flip
            If ((secondbyte And 2) = 0) Then xpos = tilex + hscroll + X Else xpos = 7 - tilex + hscroll + X
                colour = 0 '// Set Colour of Pixel (0-15)
                If (Not (addr0 And bita) = 0) Then colour = colour Or 1
                If (Not (addr1 And bita) = 0) Then colour = colour Or 2
                If (Not (addr2 And bita) = 0) Then colour = colour Or 4
                If (Not (addr3 And bita) = 0) Then colour = colour Or 8
                
            
                '// Set Priority Array (Sprites over background tile)
                If (priority = 16) And (Not colour = 0) Then bg_priority(xpos) = cierto Else bg_priority(xpos) = falso

                display(xpos + row_precal) = CRAM(colour + pal)
                tilex = tilex + 1
                bita = (bita shr 1)
        Loop Until bita = 0

        start_column = start_column + 1
    Next X

End Sub

'------------------------------~
'   Plot a single sprite pixel.
'------------------------------~
Sub plotSpritePixel(X As integer, Y As integer, location As integer, addr As integer, bita As Integer)

    Dim colour As Integer: colour = 0
    
    If (Not (VRAM(addr + 0) And bita) = 0) Then colour = colour Or 1
    If (Not (VRAM(addr + 1) And bita) = 0) Then colour = colour Or 2
    If (Not (VRAM(addr + 2) And bita) = 0) Then colour = colour Or 4
    If (Not (VRAM(addr + 3) And bita) = 0) Then colour = colour Or 8
    
    
    If (Not (bg_priority(X)) And (Not (colour) = 0)) Then
        
        'If (Not spritecol(X)) Then
            
        '    spritecol(X) = cierto
            display(location) = CRAM(colour + 16)
        
        'Else
            
        '    status = status Or 32
        'End If
    End If

End Sub

'-------------------------------~
'   Render Line of Sprite Layer.
'-------------------------------~
Sub drawSprite(lineno As Integer)


    Dim sat         As Integer:    sat = (vdpreg(5) And Not 1 And Not &h80) shl 7
    Dim count       As Integer
    Dim height      As Integer:    height = 8
    Dim zoomed      As Integer
    Dim spriteno    As Integer
    Dim Y           As Integer
    Dim addr     As Integer
    Dim X           As Integer
    Dim i           As Integer
    Dim row_precal  As Integer
    Dim adr         As Integer
    Dim bita         As Integer
    Dim pixel       As Integer
    
    '// Enabled 8x16 Sprites
    If ((vdpreg(1) And 2) = 2) Then height = 16


    '// Enable Zoomed Sprites
    If ((vdpreg(1) And 1) = 1) Then
        height = (height Shl 1)
        zoomed = cierto
    End If
    
    '// Search Sprite Attribute Table (64 Bytes)
    For spriteno = 0 To 63
    
        'If (count >= 8) Then
        '    status = status Or &h40
        '    Exit Sub
        'end if
    
        Y = VRAM(sat + spriteno)
        addr = sat + (spriteno Shl 1)
        X = VRAM(addr + &h80)
        i = VRAM(addr + &h81)
        
        If (Y = 208) Then Exit Sub
        
        Y = Y + 1
        
        If ((vdpreg(6) And 4) = 4) Then i = i Or &h100
        If ((vdpreg(1) And 2) = 2) Then i = i And Not 1
        If ((vdpreg(0) And 8) = 8) Then X = X - 8
        If (Y > 240) Then Y = Y - 256
        
        If ((lineno >= Y) And ((lineno - Y) < height)) Then
        
            row_precal = (lineno Shl 8)
            
            If zoomed = falso Then  '// Normal sprites (Width = 8)
            
                adr = (i Shl 5) + ((lineno - Y) shl 2)
                bita = &h80
                
                Do
                
                    If ((X >= 0) And (Y >= 0) And (X <= 255)) Then
                        plotSpritePixel X, lineno, X + row_precal, adr, bita
                    End If
                    
                    X = X + 1
                    bita = bita shr 1
                    
                Loop Until bita = 0
                
            Else                    '// Zoomed sprites (Width = 16)
            
                adr = (i shl 5) + (((lineno - Y) shr 1) Shl 2)
                bita = &h80
                
                Do

                    If ((X + pixel + 1 >= 0) And (Y >= 0) And (X <= 255)) Then
                        '// Plot Two Pixels
                        plotSpritePixel X + pixel, lineno, X + pixel + row_precal, adr, bita
                        plotSpritePixel X + pixel + 1, lineno, X + pixel + row_precal, adr, bita
                    End If
                    
                    X = X + 1
                    bita = bita shr 1
                
                Loop Until bita = 0
            End If
            count = count + 1
        End If
    Next spriteno

End Sub


'---------------------------------------~
'   Blank leftmost column of a scanline.
'   Replace with flllRect API.
'---------------------------------------~
Sub blankColumn(lineno As Integer)

    Dim colour      As Integer:    colour = CRAM(16 + (vdpreg(7) And &hF))
    Dim row_precal  As Integer:    row_precal = (lineno Shl 8)
    Dim X           As Integer
    
    For X = 8 To 0 Step -1
        display(X + row_precal) = colour
    Next X
    
End Sub

'--------------------------------~
'   Render Line of SMS/GG Display.
'--------------------------------~
Sub drawLine(lineno As Integer)
    
    'ReDim spritecol(SMS_WIDTH) As integer
    
    If (Not (vdpreg(1) And 64) = 0) Then
        drawBg (lineno)
        drawSprite (lineno)
    End If
    
    If ((vdpreg(0) And &h20) = &h20) Then blankColumn (lineno)

End Sub

'---------------------------~
'   Generate VDP Interrupts.
'---------------------------~
Sub vdp_interrupt(lineno As Integer)

    'linenum = lineno
    
    If (lineno <= 192) Then
    
        '// Frame Interrupt Pending
        If (lineno = 192) Then
            status = status Or &h80
            frameint = cierto
        End If
        
        '// Counter Expired = Line Interrupt Pendind
        If (counter = 0) Then
            counter = vdpreg(10)
            lineint = cierto
        Else
        '// Otherwise Decrement Counter
            counter = counter - 1
        End If
        
        '// Line Interrupts Enabled and Pending. Assert IRQ Line.
        If (lineint And ((vdpreg(0) And &h10) = &h10)) Then
            irqsetLine = cierto
        End If
    
    '// lineno >= 193
    Else
        
        '// Reload counter on every line outside active display + 1
        counter = vdpreg(10)
        
        '// Frame Interrupts Enabled and Pending. Assert IRQ Line.
        If (frameint And ((vdpreg(1) And &h20) = &h20) And (lineno < 224)) Then irqsetLine = cierto
        
    End If
End Sub

