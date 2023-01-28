' incluye definiciones de directorios
#include "dir.bi"

' inclusiones para las definiciones de teclado (comando multikey)
#include "fbgfx.bi"

' directorio que contiene las cintas *.SNA
Dim Shared Directorio As String 
Directorio = "CINTAS/"

' para que salga info por pantalla
Dim Shared deb As Byte = 0 ' 0 NO DEB, 1 DEB REGISTROS, 2 DEB REGISTROS+POKES-PEEKS
Dim Shared VelocidadReal As Byte = 0 ' 0 ultrarapido, 1 velocidad Spectrum real
Dim Shared x2 As Integer = 2 ' doble tamaño de pantalla

' Hardware del Spectrum (ver modulo "SPECTRUM.BAS")
Declare Sub refreshFlashChars()
Declare Sub ScanlinePaint(lne As long)
Declare Sub LeeTeclado()
Declare Sub screenPaint()

' sistema
Declare Function FicheroSNA() As String ' aqui vemos los SNA disponibles
Dim SNA As String ' aqui ponemos el fichero SNA leer

Declare Sub InitScreenMemTable()
Declare Sub LoadROM(sFileName As String)
Declare Sub LoadSNA(sFileName As String)
Declare Sub SaveSNA(sFileName As String)
Declare Sub LoadZ80(sFileName As String)
Declare Sub ReadZ80V1Snap(hFile As Long)
Declare Sub ReadZ80V2orV3Snap(hFile As Long)
Declare Sub InitColorArrays()
Declare Sub Set48kModel()
Declare Sub SetupContentionTable()
Declare Sub SetupTStatesToScanLines()

' interrupciones
Dim Shared glTstatesPerInterrupt As Long
Dim Shared glInterruptTimer As Long
Dim Shared interruptCounter As Long
Dim Shared glInterruptDelay As Long
Dim Shared glDelayOverage As Long

' estado del border
Declare Function GetBorderIndex(lRGB As Long) As Long
Dim Shared glNewBorder As Long
Dim Shared glLastBorder As long

Dim Shared RAM(65535) As Long ' RAM del Z80
Dim Shared glLastOut7FFD As Long
Dim Shared glLastOut1FFD As Long
Dim Shared glContentionTable(-30 To 70930) As Long
Dim Shared glTSToScanLine(70930) As Long

' sonido (sin programar aun)
Declare Sub CloseWaveOut()
Declare Function InitializeWaveOut() As integer
Dim Shared gbSoundEnabled As Long
Dim Shared glBeeperVal As Long

' teclado
Declare Sub ResetKeyboard()
Dim Shared glKeyPortMask As Long    ' // Mask used for reading keyboard port (&HBF on Speccys, &H1F on TC2048)

' //////////////////////////////////////////////////
' // Variables used by the video display rountines
' //////////////////////////////////////////////////
Dim Shared ScrnLines(191, 33) As Long    ' // 192 scanlines (0-191) and either 32 bytes per line (0-31), plus two flag bytes
Dim Shared ScrnNeedRepaint As Long       ' // Set to true when an area of the display changes, and back to false by the ScreenPaint function
Dim Shared bFlashInverse As Long         ' // Cycles between true/false to indicate the status of 'flashing' attributes
Dim Shared glScreenMem(191, 31) As Long  ' // Static lookup table that maps Y,X screen coords to the correct Speccy memory address
Dim Shared glLastFEOut As Long           ' // Contains the last value OUTed to any port with bit 0 reset (saved in snapshots, etc)
Dim Shared glTopMost As Long             ' // Top-most row of the screen that has changed since the last ScreenPaint
Dim Shared glBottomMost As Long          ' // Bottom-most row of the screen that has changed since the last ScreenPaint
Dim Shared glLeftMost As Long            ' // Left-most column of the screen that has changed since the last ScreenPaint
Dim Shared glRightMost As Long           ' // Right-most column "  "    "      "   "     "      "    "   "         "
Dim Shared SpectrumScreen(191,31) As Long' // Guardamos la pantalla organizada linealmente

' // Bob Woodring's (RGW) video performance improvements use the following lookup tables
Dim Shared glRowIndex(191) As Long
Dim Shared glColIndex(191) As Long

Type tBitTable
   dw0 As Long
   dw1 As Long
End Type

Dim Shared gtBitTable(255, 255) As tBitTable

' // RGW -- Variables used by scanline video routines
Dim Shared glTStatesPerLine As Long  ' // Contains the # of T-states per display line (different on 48K and 128K spectrums)
Dim Shared glTStatesAtTop As Long    ' // # of t-states before the start of the first screen line (excluding border)
Dim Shared glTStatesAtBottom As Long ' // # of t-states after the end of the last screen line (excluding border)
Dim Shared glTStates As Long         ' // Number of T-States for current frame (counts down towards zero, at which time an interrupt occurs)

' // Array of colour values (speeds up screen painting by avoiding
' // multiple calls to RGB() )
'Dim Shared glBrightColor(0 To 7) As Long
'Dim Shared glNormalColor(0 To 7) As Long

' // Flag whether SE BASIC ROM is to be used or not
Dim Shared gbSEBasicROM As Long

' // Global picDisplay variable to speed things up
'Dim Shared gpicDisplay As PictureBox
Dim Shared gpicDC As Long

' // Keypresses
Dim Shared keyB_SPC As Long
Dim Shared keyH_ENT As Long
Dim Shared keyY_P As Long
Dim Shared key6_0 As Long
Dim Shared key1_5 As Long
Dim Shared keyQ_T As Long
Dim Shared keyA_G As Long
Dim Shared keyCAPS_V As Long

Dim Shared glBufferBits(24576) As Long

Dim Shared glDisplayHeight As Long
Dim Shared glDisplayWidth As Long
Dim Shared glDisplayVSource As Long ' // Set to glDisplayHeight - 1 to improve display speed
Dim Shared glDisplayVSize As Long   ' // Set to -glDisplayHeight to improve display speed
'Dim Shared glDisplayXMultiplier As long
'Dim Shared glDisplayYMultiplier As long

' // incluimos el nucleo emulador Z80
#include "Z80.bas"

' // incluimos el nucleo Spectrum
#include "Spectrum.bas"

' // sonido 
'#include "5_WaveOut.bas"

Sub InitScreenIndexs()
    Dim n As Long
   
    For n = 0 To 191
        glRowIndex(n) = 6144 + (n \ 8) * 32
        glColIndex(n) = (n * 256) \ 4
        
    Next n

End Sub

Sub CloseWaveOut()
    Dim lRet As Long
    'lRet = waveOutReset(glphWaveOut)
    'For lRet = 1 To NUM_WAV_BUFFERS
        'waveOutUnprepareHeader glphWaveOut, gtWavHdr(lRet), Len(gtWavHdr(lRet))
    'Next lRet
    
    'For lRet = 1 To NUM_WAV_BUFFERS
        'GlobalUnlock ghMem(lRet)
        'GlobalFree ghMem(lRet)
    'Next lRet
    
    'waveOutClose glphWaveOut
End Sub

Function InitializeWaveOut()
    Dim lRet As Long, sErrMsg As String, lCounter As Long, lCounter2 As Long
    
    'glBeeperVal = 128
    
    'With gtWavFormat
    '    .wFormatTag = WAVE_FORMAT_PCM
    '    .nChannels = 1
    '    .nSamplesPerSec = WAVE_FREQUENCY
    '    .nAvgBytesPerSec = WAVE_FREQUENCY
    '    .nBlockAlign = 1
    '    .wBitsPerSample = 8
    '    .cbSize = 0
    'End With
    ''lRet = waveOutOpen(glphWaveOut, WAVE_MAPPER, gtWavFormat, 0, True, CALLBACK_NULL)
    'If lRet <> MMSYSERR_NOERROR Then
    '    sErrMsg = Space$(255)
    '    'waveOutGetErrorText lRet, sErrMsg, Len(sErrMsg)
    '    sErrMsg = Left$(sErrMsg, InStr(sErrMsg, Chr$(0)) - 1)
    '    Print "Error initialising WaveOut device." :sleep:end
    '    InitializeWaveOut = False
    '    Exit Function
    'End If
   ' 
   ' For lCounter = 1 To NUM_WAV_BUFFERS
   '     ghMem(lCounter) = GlobalAlloc(GPTR, WAV_BUFFER_SIZE)
   '     gpMem(lCounter) = GlobalLock(ghMem(lCounter))
   '     With gtWavHdr(lCounter)
   '         .lpData = gpMem(lCounter)
   '         .dwBufferLength = WAV_BUFFER_SIZE
   '         .dwUser = 0
   '         .dwFlags = 0
   '         .dwLoops = 0
   '         .lpNext = 0
   '     End With
   '     
   '     'lRet = waveOutPrepareHeader(glphWaveOut, gtWavHdr(lCounter), Len(gtWavHdr(lCounter)))
   '     If lRet <> MMSYSERR_NOERROR Then
   '         sErrMsg = Space$(255)
   '         waveOutGetErrorText lRet, sErrMsg, Len(sErrMsg)
   '         sErrMsg = Left$(sErrMsg, InStr(sErrMsg, Chr$(0)) - 1)
   '         Printx "Error preparing wave header." :sleep: end
   '         lRet = waveOutClose(glphWaveOut)
   '         For lCounter2 = 1 To NUM_WAV_BUFFERS
   '             GlobalUnlock ghMem(lCounter2)
   '             GlobalFree ghMem(lCounter2)
   '         Next lCounter2
   '         InitializeWaveOut = False
   '         Exit Function
   '     End If
    'Next lCounter
   ' 
   ' For lCounter = 0 To 48000
   '     gcWaveOut(lCounter) = glBeeperVal
   ' Next lCounter
   ' 
   ' InitializeWaveOut = True

End Function


Sub initscreen()
    Dim i As Long, X As Long
        
    glTopMost = 0
    glBottomMost = 191
    glLeftMost = 0
    glRightMost = 31
    
    For i = 0 To 191
        For X = 0 To 32
            ScrnLines(i, X) = True
        Next X
    Next i

    ScrnNeedRepaint = True
End Sub

Sub InitScreenMemTable()
    Dim X As Long, y As Long
    For y = 0 To 191
        For X = 0 To 31
            glScreenMem(y, X) =((((y \ 8) * 32) + (y Mod 8) * 256) + ((y \ 64) * 2048) - (y \ 64) * 256) + X
        Next X
    Next y
End Sub

Sub LoadROM(sROMFile As String)
    Dim hFile As Long, sROM As String, lCounter As Long

    hFile = Freefile
    Open sROMFile For Binary As hFile
     sROM = Input(16384, #hFile)
    Close hFile
    
    ' // Copy the ROM into the appropriate memory page
    For lCounter = 1 To 16384
        RAM(lCounter - 1) = Asc(Mid$(sROM, lCounter, 1))
    Next lCounter

End Sub

Sub LoadSNA(sFileName As String)
    Dim hFile As Long, sData As String, iCounter As Long
    
    hFile = Freefile
    Open sFileName For Binary As hFile
    
    sData = Input(1, #hFile)
    intI = Asc(sData)
    sData = Input(2, #hFile)
    regHL_ = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    sData = Input(2, #hFile)
    regDE_ = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    sData = Input(2, #hFile)
    regBC_ = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    sData = Input(2, #hFile)
    regAF_ = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    
    sData = Input(2, #hFile)
    regHL = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    sData = Input(2, #hFile)
    regDE = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    sData = Input(2, #hFile)
    setBC Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    sData = Input(2, #hFile)
    regIY = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    sData = Input(2, #hFile)
    regIX = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    
    glLastBorder = -1
    
    sData = Input(1, #hFile)
    If Asc(sData) And 4 Then
        intIFF1 = True
        intIFF2 = True
    Else
        intIFF1 = False
        intIFF2 = False
    End If
    
    sData = Input(1, #hFile)
    intR = Asc(sData)
    intRTemp = intR
    
    sData = Input(2, #hFile)
    setAF Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    
    sData = Input(2, #hFile)
    regSP = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))

    sData = Input(1, #hFile)
    intIM = Asc(sData)
    
    ' // Border color
    sData = Input(1, #hFile)
    glNewBorder = (Asc(sData) And &H7&)
    ' // Set the initial border color
    'frmMainWnd.BackColor = glNormalColor(glNewBorder)
    
        ' // Load a 48K Snapshot file
        sData = Input(49153, #hFile)
        Close hFile
       
        For iCounter = 16384 To 65535
            RAM(iCounter) = Asc(Mid$(sData, iCounter-16383, 1))
        Next iCounter

        initscreen
        screenPaint
        'resetKeyboard
        poppc

End Sub

Sub InitScreenMask()
   ' RGW Prefill the screen color & attribute lookup table
   '     with all possible combinations
   '     When drawing the screen in the bit buffer
   '     a simple lookup produces the required bytes
   
   Dim fC       As Long   ' fore color
   Dim BC       As Long   ' back color
   Dim Bright   As Long
   Dim Flash    As Long
   
   Dim bits     As Long
   Dim Colour(1) As Long
   Dim lTemp    As Long
               
   For Flash = 0 To 1
      For Bright = 0 To 1
         For fC = 0 To 7
            For BC = 0 To 7
               For bits = 0 To 255
                  If Flash = 0 Then
                     Colour(1) = fC + (Bright * 8)
                     Colour(0) = BC + (Bright * 8)
                  Else
                     Colour(1) = BC + (Bright * 8)
                     Colour(0) = fC + (Bright * 8)
                  End If
                  lTemp = (Flash * 128) + (Bright * 64) + (BC * 8) + fC
                  gtBitTable(bits, lTemp).dw0 = (Colour(Abs((bits And 16)  = 16)) * 16777216) + _
                                                (Colour(Abs((bits And 32)  = 32)) * 65536) + _
                                                (Colour(Abs((bits And 64)  = 64)) * 256) + _
                                                 Colour(Abs((bits And 128) = 128))
                  gtBitTable(bits, lTemp).dw1 = (Colour(Abs((bits And 1) = 1)) * 16777216) + _
                                                (Colour(Abs((bits And 2) = 2)) * 65536) + _
                                                (Colour(Abs((bits And 4) = 4)) * 256) + _
                                                 Colour(Abs((bits And 8) = 8))
               Next bits
            Next BC
         Next fC
      Next Bright
   Next Flash
End Sub

Sub InitColorArrays()
  ' PALETA BRILLANTE
  Dim B As Integer = 64
  Palette 0+B,0,0,0
  Palette 1+B,0,0,255
  Palette 2+B,255,0,0
  Palette 3+B,255,0,255
  Palette 4+B,0,255,0
  Palette 5+B,0,255,255
  Palette 6+B,255,255,0
  Palette 7+B,255,255,255
  ' PALETA NORMAL
  Palette 0,0,0,0
  Palette 1,0,0,192
  Palette 2,192,0,0
  Palette 3,192,0,192
  Palette 4,0,192,0
  Palette 5,0,192,192
  Palette 6,192,192,0
  Palette 7,192,192,192

    ' guardamos los tonos de color para su uso con el BORDER
    'glBrightColor(0) = 0
    'glBrightColor(1) = RGB(0, 0, 255)
    'glBrightColor(2) = RGB(255, 0, 0)
    'glBrightColor(3) = RGB(255, 0, 255)
    'glBrightColor(4) = RGB(0, 255, 0)
    'glBrightColor(5) = RGB(0, 255, 255)
    'glBrightColor(6) = RGB(255, 255, 0)
    'glBrightColor(7) = RGB(255, 255, 255)

    'glNormalColor(0) = 0
    'glNormalColor(1) = RGB(0, 0, 192)
    'glNormalColor(2) = RGB(192, 0, 0)
    'glNormalColor(3) = RGB(192, 0, 192)
    'glNormalColor(4) = RGB(0, 192, 0)
    'glNormalColor(5) = RGB(0, 192, 192)
    'glNormalColor(6) = RGB(192, 192, 0)
    'glNormalColor(7) = RGB(192, 192, 192)

End Sub

Sub resetKeyboard()
    keyB_SPC = &HFF&
    keyH_ENT = &HFF&
    keyCAPS_V= &HFF&
    keyY_P   = &HFF&
    key6_0   = &HFF&
    key1_5   = &HFF&
    keyQ_T   = &HFF&
    keyA_G   = &HFF&
    do : loop while inkey$<>""
End Sub


Sub SaveROM(sFileName As String)
    Dim hFile As Long, lCounter As Long
    
    On Error Goto SaveROM_Err
    
    hFile = Freefile
    Open sFileName For Output As hFile
    For lCounter = 0 To 65535
        Print #hFile, Chr$(RAM(lCounter));
    Next lCounter
    
SaveROM_Err:
    Close hFile
End Sub


Sub SaveSNA(sFileName As String)
    Dim hFile As Long, sData As String, lCounter As Long
    
    hFile = Freefile
    Open sFileName For Output As hFile
    
    pushpc
    
    Print #hFile, Chr$(intI);
    Print #hFile, Chr$(regHL_ And &HFF&); Chr$(regHL_ \ 256&);
    Print #hFile, Chr$(regDE_ And &HFF&); Chr$(regDE_ \ 256&);
    Print #hFile, Chr$(regBC_ And &HFF&); Chr$(regBC_ \ 256&);
    Print #hFile, Chr$(regAF_ And &HFF&); Chr$(regAF_ \ 256&);
    
    Print #hFile, Chr$(regHL And &HFF&); Chr$(regHL \ 256&);
    Print #hFile, Chr$(regDE And &HFF&); Chr$(regDE \ 256&);
    Print #hFile, Chr$(regC); Chr$(regB);
    Print #hFile, Chr$(regIY And &HFF&); Chr$(regIY \ 256&);
    Print #hFile, Chr$(regIX And &HFF&); Chr$(regIX \ 256&);
    
    ' Interrupt flipflops
    If intIFF1 = True Then
        Print #hFile, Chr$(4);
    Else
        Print #hFile, Chr$(0);
    End If

    ' R
    intRTemp = intRTemp And 127
    Print #hFile, Chr$((intR And &H80&) Or intRTemp);

    ' // AF
    Print #hFile, Chr$(getAF And &HFF&); Chr$(getAF \ 256&);
    
    ' // SP
    Print #hFile, Chr$(regSP And &HFF&); Chr$(regSP \ 256&);
    
    ' // Interrupt Mode
    Print #hFile, Chr$(intIM);
    
    Print #hFile, 0 'Chr$(GetBorderIndex(frmMainWnd.BackColor));

    For lCounter = 16384 To 65535
        Print #hFile, Chr$(RAM(lCounter));
    Next lCounter
    
    Close hFile
    poppc
End Sub

Sub Set48kModel()
    Dim sModel As String
    
    
        ' // A 48K Spectrum has 69888 tstates per interrupt (3.50000 MHz)
        glTstatesPerInterrupt = 69888
'        glWaveAddTStates = 158 ' 58
        
        glKeyPortMask = &HBF&

        ' // T-state information
        glTStatesPerLine = 224
        glTStatesAtTop = -glTstatesPerInterrupt + 14336
        glTStatesAtBottom = -glTstatesPerInterrupt + 14336 + 43007
        
        LoadROM "rom/spectrum.rom"

        SetupContentionTable
    
        SetupTStatesToScanLines

End Sub


Sub SetupContentionTable()
    Dim l As Long, z As Long, X(8) As Long, y As Long
  
    X(0) = 6 '6
    X(1) = 5 '5
    X(2) = 4 '4
    X(3) = 3 '3
    X(4) = 2 '2
    X(5) = 1 '1
    X(6) = 0 '0
    X(7) = 0 '0
    
    l = -glTstatesPerInterrupt
    Do While l <= 0
        If (l >= (glTStatesAtTop)) And (l <= glTStatesAtBottom) Then
            For y = 0 To glTStatesPerLine
                If y < 128 Then
                    glContentionTable(-l - y) = X(z)
                    z = z + 1
                    If z > 7 Then Let z = 0
                Else
                   glContentionTable(-l - y) = 0
                End If
            Next y
            z = 0
            l = l + glTStatesPerLine - 1
        Else
            glContentionTable(-l) = 0
        End If
        l = l + 1
    Loop
End Sub

 Sub SetupTStatesToScanLines()
    Dim n As Long
    
    For n = -glTstatesPerInterrupt To 0
        If (n >= glTStatesAtTop) And (n <= glTStatesAtBottom) Then
            glTSToScanLine(-n) = (n - glTStatesAtTop) \ glTStatesPerLine
        Else
            glTSToScanLine(-n) = -1 ' // In the border area or vertical retrace
        End If
    Next n
End Sub

Function FicheroSNA() As String
    Dim fichero As String
    Dim f As Integer = 0
    Dim x As Integer
    Dim y As Integer =0
    Dim Salir As Integer = 0
    Dim Encontrados( 7000 ) As String
   
    Cls
    fichero = Dir( directorio+"*", fbArchive )
    Do
        f+=1
        encontrados(f)=fichero
        fichero = Dir( )
    Loop While Len( fichero ) > 0
    ' "borramos" los ultimos 20 para crear un espacio vacio
    For x=f To f+20:encontrados(x)=" ":Next

    Locate 1,1:Print "Elige una Cinta para cargar o pulsa para entrar en modo Basic"
    Locate 2,1:Color 0,7:Print encontrados(y+1):Color 7,0
    For x=2 To 20
        Locate x+1,1:Print encontrados(x+y)
    Next
    While salir=0
       If Multikey(SC_ESCAPE) Then Salir=2
       If Multikey(SC_ENTER) Then Salir=1
       If Multikey(SC_DOWN) Then
          y+=1
          If y>f-2 Then y=f-2
          Locate 2,1:Color 0,7:Print encontrados(y+1);:Color 7,0:Print "            "
          For x=2 To 20
            Locate x+1,1:Print encontrados(x+y);"            ";
          Next
          'do : loop while multikey(SC_DOWN)
          Sleep  80
       End If
       If Multikey(SC_UP) Then 
          y-=1
          If y<0 Then y=0
          Locate 2,1:Color 0,7:Print encontrados(y+1);:Color 7,0:Print "            "
          For x=2 To 20
            Locate x+1,1:Print encontrados(x+y);"            ";
          Next
          'do : loop while multikey(SC_UP)
          Sleep  80
       End If
       If Multikey(SC_PAGEDOWN) Then
          y+=10
          If y>f-2 Then y=f-2
          Locate 2,1:Color 0,7:Print encontrados(y+1);:Color 7,0:Print "            "
          For x=2 To 20
            Locate x+1,1:Print encontrados(x+y);"            ";
          Next
          'do : loop while multikey(SC_PAGEDOWN)
          Sleep  50
       End If
       If Multikey(SC_PAGEUP) Then 
          y-=10
          If y<0 Then y=0
          Locate 2,1:Color 0,7:Print encontrados(y+1);:Color 7,0:Print "            "
          For x=2 To 20
            Locate x+1,1:Print encontrados(x+y);"            ";
          Next
          'do : loop while multikey(SC_PAGEUP)
          Sleep  50
       End If

    Wend
    
    ' Si pulsamos ESC no cargamos cinta, y entramos en modo Basic de Spectrum
    If Salir=2 Then encontrados(y+1)=""

    ' si pulsamos ENTER cargamos el fichero SNA elegido
    FicheroSNA = encontrados(y+1)
    do : loop while inkey$<>""
End Function

Sub LoadZ80(sFileName As String)
    Dim hFile As Long, sData As String, iCounter As Long
    Dim bCompressed As Byte
    
    hFile = Freefile
    Open sFileName For Binary As hFile
    
    glLastBorder = -1
    Z80Reset
    
    'If gbSoundEnabled Then AY8912_reset
    
    ' byte 0 - A register
    sData = Input(1, #hFile)
    regA = Asc(sData)
    ' byte 1 - F register
    sData = Input(1, #hFile)
    setF Asc(sData)
    ' bytes 2 + 3 - BC register pair (C first, then B)
    sData = Input(2, #hFile)
    setBC Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    ' bytes 4 + 5 - HL register pair
    sData = Input(2, #hFile)
    regHL = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    ' bytes 6 + 7 - PC (this is zero for v2.x or v3.0 Z80 files)
    sData = Input(2, #hFile)
    regPC = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    ' bytes 8 + 9 - SP
    sData = Input(2, #hFile)
    regSP = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    ' byte 10 - Interrupt register
    sData = Input(1, #hFile)
    intI = Asc(sData)
    ' byte 11 - Refresh register
    sData = Input(1, #hFile)
    intR = (Asc(sData) And 127)
    
    ' byte 12 - bitfield
    sData = Input(1, #hFile)
    ' if byte 12 = 255 then it must be treated as if it = 1, for compatibility with other emulators
    If Asc(sData) = 255 Then sData = Chr$(1)
    ' bit 0 - bit 7 of R
    If (Asc(sData) And 1) = 1 Then intR = intR Or 128
    intRTemp = intR
    ' bits 1,2 and 3 - border color
    glNewBorder = (Asc(sData) And 14) \ 2
    ' bit 4 - 1 if SamROM switched in (we don't care about this!)
    ' bit 5 - if 1 and PC<>0 then the snapshot is compressed using the
    '         rudimentary Z80 run-length encoding scheme
    If (Asc(sData) And &H20&) Then bCompressed = True
    ' bits 6 + 7 - no meaning

    ' bytes 13 + 14 - DE register pair
    sData = Input(2, #hFile)
    regDE = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    ' bytes 15 + 16 - BC' register pair
    sData = Input(2, #hFile)
    regBC_ = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    ' bytes 17 + 18 - DE' register pair
    sData = Input(2, #hFile)
    regDE_ = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    ' bytes 19 + 20 - HL' register pair
    sData = Input(2, #hFile)
    regHL_ = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    ' bytes 21 + 22 - AF' register pair (A first then F - not Z80 byte order!!)
    sData = Input(2, #hFile)
    regAF_ = Asc(Left$(sData, 1)) * 256& + Asc(Right$(sData, 1))
    ' byte 23 + 24 - IY register pair
    sData = Input(2, #hFile)
    regIY = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    ' byte 25 + 26 - IX register pair
    sData = Input(2, #hFile)
    regIX = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    ' byte 27 - Interrupt flipflop (0=DI, else EI)
    sData = Input(1, #hFile)
    If Asc(sData) = 0 Then
        intIFF1 = False
        intIFF2 = False
    Else
        intIFF1 = True
        intIFF2 = True
    End If
    ' byte 28 - IFF2 (ignored)
    sData = Input(1, #hFile)
    ' byte 29 - Interrupt mode (bits 2 - 7 contain info about joystick modes etc, which we ignore)
    sData = Input(1, #hFile)
    intIM = Asc(sData) And 3
    
    If regPC = 0 Then
        ' This is a V2 or V3 Z80 file
        ReadZ80V2orV3Snap hFile
        Close hFile
    Else
        ' // V1 .Z80 snapshots are all 48K
        ' // PC<>0, so lets check to see if this is a compressed V1 Z80 file
        If bCompressed Then
            ' Uncompress the RAM data
            ReadZ80V1Snap hFile
            Close hFile

        Else
            ' // Uncompressed Z80 file
            sData = Input(49153, #hFile)
            Close hFile
            
            ' // Copy the RAM data to addressable memory space
            For iCounter = 16384 To 65535
                 RAM(iCounter) = Asc(Mid$(sData, iCounter-16383, 1))
            Next iCounter
        End If
    End If
    
    'initscreen
    
    ' // Set the initial border color
    'Select Case glNewBorder
    'Case 0
    '    frmMainWnd.BackColor = 0&
    'Case 1
    '    frmMainWnd.BackColor = RGB(0, 0, 192)
    'Case 2
    '    frmMainWnd.BackColor = RGB(192, 0, 0)
    'Case 3
    '    frmMainWnd.BackColor = RGB(192, 0, 192)
    'Case 4
    '    frmMainWnd.BackColor = RGB(0, 192, 0)
    'Case 5
    '    frmMainWnd.BackColor = RGB(0, 192, 192)
    'Case 6
    '    frmMainWnd.BackColor = RGB(192, 192, 0)
    'Case 7
    '    frmMainWnd.BackColor = RGB(192, 192, 192)
    'End Select

    'frmMainWnd.Caption = App.ProductName & " - " & GetFilePart(sFileName)
    
    'initscreen
    'screenPaint
    'resetKeyboard
        
    'gpicDisplay.REFRESH
    'DoEvents
End Sub

Sub ReadZ80V1Snap(hFile As Long)

    Dim lDataLen As Long, sData As String, lBlockLen As Long
    Dim lCounter As Long, lMemPos As Long, lBlockCounter As Long
 
    lDataLen = Lof(hFile) - Seek(hFile) + 1
    ' // read the compressed data into sData
    sData = Input(lDataLen, #hFile)
        
    ' // Uncompress the block to memory
    lCounter = 1
    lMemPos = 16384
    Do
        If Asc(Mid$(sData, lCounter, 1)) = &HED& Then
            If Asc(Mid$(sData, lCounter + 1, 1)) = &HED& Then
                ' // This is an encoded block
                lCounter = lCounter + 2
                lBlockLen = Asc(Mid$(sData, lCounter, 1))
                lCounter = lCounter + 1
                For lBlockCounter = 1 To lBlockLen
                     RAM(((lMemPos\16384)*&h4000)+ (lMemPos And 16383)) = Asc(Mid$(sData, lCounter, 1))
                    lMemPos = lMemPos + 1
                Next lBlockCounter
            Else
                ' // Just a single ED, write it out
                 RAM(((lMemPos\16384)*&h4000)+ (lMemPos And 16383)) = &HED
                lMemPos = lMemPos + 1
            End If
        Else
             RAM(((lMemPos\16384)*&h4000)+ (lMemPos And 16383)) = Asc(Mid$(sData, lCounter, 1))
            lMemPos = lMemPos + 1
        End If
        lCounter = lCounter + 1
    Loop Until lCounter > Len(sData) - 4
    
    If Mid$(sData, lCounter, 4) <> Chr$(0) & Chr$(&HED) & Chr$(&HED) & Chr$(0) Then
        Cls:Print "Error en el Z80 comprimido: falta bloque 0x00EDED00."
        Sleep:End
    End If

End Sub

Sub ReadZ80V2orV3Snap(hFile As Long)
    Dim lHeaderLen As Long, sData As String
    Dim lCounter As Long, bHardwareSupported As Byte
    Dim lMemPage As Long, lBlockCounter As Long, lMemPos As Long
    Dim lOutFFFD As Long, bTimex As Byte
    
    sData = Input(2, #hFile)
    lHeaderLen = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
    
    ' // offset 32 - 2 bytes - PC
    If lCounter < lHeaderLen Then
        sData = Input(2, #hFile)
        regPC = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
        lCounter = lCounter + 2
    End If
    
    ' // offset 34 - 1 byte - hardware mode
    If lCounter < lHeaderLen Then
        sData = Input(1, #hFile)
        Select Case Asc(sData)
        Case 0 ' // 48K spectrum
            bHardwareSupported = True
        Case Else
            bHardwareSupported = True
            Cls
            Print "Esta cinta fue capturada en un modelo 128k (o puede ser solo 128k):"
            print "Puede haber problemas de compatibilidad al ejecutar en un 48k"
            Sleep
        End Select
        lCounter = lCounter + 1
    End If
    
    ' // offset 35 - 1 byte - last out to 0x7FFD - not required for 48K spectrum
    If lCounter < lHeaderLen Then
        sData = Input(1, #hFile)
        lCounter = lCounter + 1
    End If
    
    ' // offset 36 - 1 byte - 0xFF if Interface 1 ROM is paged in
    If lCounter < lHeaderLen Then
        sData = Input(1, #hFile)
        lCounter = lCounter + 1
        If bTimex Then
           Cls:Print "Juegos con Interface I o II no soportado"
           Sleep:End
        End If
    End If
    
    ' // offset 37 - 1 byte (bit 0: 1=intR emulation on, bit 1: 1=LDIR emulation on
    If lCounter < lHeaderLen Then
        sData = Input(1, #hFile)
        lCounter = lCounter + 1
    End If
    
    ' // offset 38 - Last out to 0xFFFD (+2/+3 sound chip register number)
    If lCounter < lHeaderLen Then
        lOutFFFD = Asc(Input(1, #hFile))
        lCounter = lCounter + 1
    End If
    
    ' // offset 39 - 16 bytes - contents of the sound chip registers
    If lCounter < lHeaderLen Then
        ' el sonido AY no se emula, por lo que lo leemos en vacio
        sData = Input(16, #hFile)
        lCounter = lCounter + 16
    End If

    
    ' // read the remaining bytes of the header (we don't care what information they hold)
    If lCounter < lHeaderLen Then
        sData = Input(lHeaderLen - lCounter, #hFile)
    End If
    
    Do
        ' // read a block
        sData = Input(2, #hFile)
        If Eof(hFile) Then Exit Do
        
        lHeaderLen = Asc(Right$(sData, 1)) * 256& + Asc(Left$(sData, 1))
        sData = Input(1, #hFile)
        Select Case Asc(sData)
        Case 0 ' // Spectrum ROM
            'If b128K Then lMemPage = 9 Else lMemPage = 8
            lMemPage = -1
        Case 1 ' // Interface 1 ROM, or similar (we discard these blocks)
            lMemPage = -1
        Case 2 ' // 128K ROM (reset)
            'If b128K Then lMemPage = 8 Else lMemPage = -1
            lMemPage = -1
        Case 3 ' // Page 0 (not used by 48K Spectrum)
            'If b128K Then lMemPage = 0 Else lMemPage = -1
            lMemPage = -1
        Case 4 ' // Page 1 RAM at 0x8000
            lMemPage = 2
        Case 5 ' // Page 2 RAM at 0xC000
            lMemPage = 3
        Case 6 ' // Page 3 (not used by 48K Spectrum)
            'If b128K Then lMemPage = 3 Else lMemPage = -1
            lMemPage = -1
        Case 7 ' // Page 4 (not used by 48K Spectrum)
            'If b128K Then lMemPage = 4 Else lMemPage = -1
            lMemPage = -1
        Case 8 ' // Page 5 RAM at 0x4000
            lMemPage = 1
        Case 9 ' // Page 6 (not used by 48K Spectrum)
            'If b128K Then lMemPage = 6 Else lMemPage = -1
            lMemPage = -1
        Case 10 ' // Page 7 (not used by 48K Spectrum)
            'If b128K Then lMemPage = 7 Else lMemPage = -1
            lMemPage = -1
        Case 11 ' // Multiface ROM
            lMemPage = -1
        Case Else
            lMemPage = -1
        End Select
        
        
        If lMemPage <> -1 Then
            If lHeaderLen = &HFFFF& Then
                sData = Input(16384, #hFile)
                ' Not a compressed block, just copy it straight into RAM
                For lCounter = 0+(lMemPage*&h4000) To 16383+(lMemPage*&h4000)
                    RAM(lCounter) = Asc(Mid$(sData, lCounter, 1))
                Next lCounter
            Else
                sData = Input(lHeaderLen, #hFile)
                ' // Uncompress the block to memory
                lCounter = 1
                lMemPos = 0
                Do
                    If Asc(Mid$(sData, lCounter, 1)) = &HED& Then
                        If Asc(Mid$(sData, lCounter + 1, 1)) = &HED& Then
                            ' // This is an encoded block
                            lCounter = lCounter + 2
                            lHeaderLen = Asc(Mid$(sData, lCounter, 1))
                            lCounter = lCounter + 1
                            If lMemPos + lHeaderLen - 1 > 16383 Then Goto ErrBlockTooBig
                            For lBlockCounter = 0 To lHeaderLen - 1
                                'RAM(lMemPage, lMemPos + lBlockCounter) = Asc(Mid$(sData, lCounter, 1))
                                 RAM((lMemPage*&h4000)+ lMemPos + lBlockCounter) = Asc(Mid$(sData, lCounter, 1))
                            Next lBlockCounter
                            lMemPos = lMemPos + lBlockCounter
                        Else
                            ' // Just a single ED, write it out
                            RAM((lMemPage*&h4000)+lMemPos) = &HED
                            lMemPos = lMemPos + 1
                        End If
                    Else
                        RAM((lMemPage*&h4000)+lMemPos) = Asc(Mid$(sData, lCounter, 1))
                        lMemPos = lMemPos + 1
                    End If
                    
                    If lMemPos > 16384 Then Goto ErrBlockTooBig
                    lCounter = lCounter + 1
                Loop Until lCounter > Len(sData)
            End If
        End If
    Loop Until Eof(hFile)
Exit Sub
ErrBlockTooBig:
    Print "Error: Z80 incompleto, no compatible o diferente modelo de 48k."
    Sleep:End
End Sub

'/////////////////////////////////////////////////////////////////////////////////////////////////////
'*******************************************************************************************************
'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

    InitColorArrays
    InitScreenMemTable
    
    ' // RGW's performance improvements
    'InitScreenMask
    InitScreenIndexs
      
    
    ' solo las declaramos por que por ahora no se usan
    'glDisplayWidth = 256
    'glDisplayHeight = 192

    'AY8912_init 1773000, WAVE_FREQUENCY, 8
    
    glInterruptDelay = 20
    
    ' PRINCIPAL: ARRANCAMOS LA PANTALLA Y LA EMULACION
    ' abrimos una de 640x480x32bpp con una copia oculta para dibujar
    Screen 18,,2
        
    ' primero cargamos un SNA o una ROM en su defecto
    SNA = FicheroSNA

    ' // Initialize everything

    'initscreenmask
    'initscreenindexs
    'initscreenmemtable
    'initscreen
    'initParity
    ResetKeyboard
    initcolorarrays
    Z80Reset
    Set48kModel

    glInterruptTimer = Timer()*1000
    gbSoundEnabled = InitializeWaveOut()
    

    If SNA<>"" Then
         SNA=Ucase$(SNA)
         If Right$(SNA,4)=".SNA" Then LoadSNA directorio+SNA
         If Right$(SNA,4)=".Z80" Then LoadZ80(directorio+SNA)
    End If

    ' habilitamos lapagina 1 para dibujar oculto, la pagina 0 para la visible
    Screenset 1,0 

' rom alternativa basica estilo "plus" con diferente fuente grafica
'LoadROM "ROM/sebasic.rom"

'loadsna "sna/SKOLDAZE.sna"
'loadsna "sna/SHERLOCK.sna" ' no funciona el teclado
'loadsna "sna/underwul.sna" ' fallan los graficos
'loadsna "cintas/JETWILLY.sna"

    ' // Begin the Z80 execution loop, this drives the whole emulation
    sleep 100 ' necesario para que le de tiempo al teclado a vaciarse
    execute





