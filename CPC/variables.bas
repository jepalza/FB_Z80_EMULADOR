' RAM del Z80 (2*64Kb en un CPC 6128)
Dim Shared RAM(65536*2) As Integer 
         Dim Shared kkk As Integer=&h4000 ' para pruebas cpm pantalla modo 2
' Espacio para ROMS (3 en total para un CPC 6128)
Dim Shared ROM(16384*3) As Integer

'---------------------------- PARA LA CPU Z80 --------------------------------
' boleanas
Dim Shared Falso As Integer = 0
Dim Shared cierto As Integer = -1

' para que salga info por pantalla
Dim Shared deb As Byte = 0 ' 0 NO DEB, 1 DEPURACION
Dim Shared VelocidadReal As Byte = 0 ' 0 ultrarapido, 1 velocidad real
Dim Shared x2 As Integer = 1 ' doble tamaño de pantalla

' interrupciones
Dim Shared TstatesPerInterrupt As Integer
Dim Shared interruptCounter As Integer
Dim Shared InterruptDelay As Integer
Dim Shared DelayOverage As Integer
Dim Shared ResTStates As Integer ' contador hacia atras. al llegar a cero se genera IRQ

' usada para acelerar calculos (matriz precalculada)
Dim Shared Parity(256) As Integer

' tabla de divisiones, mas rapido que dividir en tiempo real 
Dim Shared Div4    (82000) As Integer
Dim Shared Div32   (82000) As Integer
Dim Shared Div256  (82000) As Integer 
Dim Shared Div16384(82000) As Integer 

' Main Z80 registers
Dim Shared regA As Integer
Dim Shared regHL As Integer
Dim Shared regB As Integer
Dim Shared regC As Integer
Dim Shared regDE As Integer

' Z80 Flags
Dim Shared fS As Integer
Dim Shared fZ As Integer
Dim Shared f5 As Integer
Dim Shared fH As Integer
Dim Shared f3 As Integer
Dim Shared fPV As Integer
Dim Shared fN As Integer
Dim Shared fC As Integer

' Flag bit positions
Const F_C As Integer = 1
Const F_N As Integer = 2
Const F_PV As Integer = 4
Const F_3 As Integer = 8
Const F_H As Integer = 16
Const F_5 As Integer = 32
Const F_Z As Integer = 64
Const F_S As Integer = 128

' Alternate registers
Dim Shared regAF_ As Integer
Dim Shared regHL_ As Integer
Dim Shared regBC_ As Integer
Dim Shared regDE_ As Integer

' Index registers  - ID used as temp for ix/iy
Dim Shared regIX As Integer
Dim Shared regIY As Integer
Dim Shared regID As Integer

' Stack pointer and program counter
Dim Shared regSP As Integer
Dim Shared regPC As Integer

' Interrupt registers and flip-flops, and refresh registers
Dim Shared intI As Integer
Dim Shared intR As Integer
Dim Shared intRTemp As Integer
Dim Shared intIFF1 As Integer
Dim Shared intIFF2 As Integer
Dim Shared intIM As Integer
Dim Shared Halt As integer


' ------------------------------------------------------------------
' exclusivas AMSTRAD CPC
Dim Shared veces_a_pintar2 As Integer=0 ' para pintar la pantalla cada X veces
Dim Shared linenum     As Integer       ' Current Line Number.
Dim Shared irqsetLine  As Integer       ' Interrupt Line.
Dim Shared vsync       As Integer=0     ' VSYNC.

' para el VGA (Video Graphics Array)
Dim Shared As integer ROMINF_OFF = 4
Dim Shared As integer ROMSUP_OFF = 8
Dim Shared As Integer ROMINF=100
Dim Shared As Integer ROMSUP=200
Dim Shared As Integer ROMDSK=300
Dim Shared As Integer TabCoul(32)
Dim Shared As Integer TabInk(32)
Dim Shared As Integer RamSelect
Dim Shared As Integer lastMode ' modo de pantalla
Dim Shared As Integer BancoMemoria
Dim Shared As integer adrEcr
Dim Shared As Integer RomExt
Dim Shared As Integer PenSelect
' ----------------------------------

' para almacenar los bancos de memoria de un Amstrad CPC
Dim Shared As Integer TabPOKE(4)
Dim Shared As Integer TabPEEK(4)

' Banco de memoria de VIDEO del Amstrad CPC
Dim Shared As Integer VRAM(&h10000)

' almacen de colores del CPC
Dim Shared As Integer CPC_Colores_modo0(16) 
Dim Shared As Integer CPC_Colores_modo1(4) 
Dim Shared As Integer CPC_Colores_modo2(2) 

' alamacen de valores enviados al controlador 8255 PPI del Amstrad CPC
Dim Shared As Integer TablaPPI(4) 

Dim Shared As Integer TablaPSG(16) ' para almacenear los valores del sonido
Dim Shared As Integer ModoPSG=0 ' modo de ondas de sonido
Dim Shared As Integer LineaTeclado=0 ' linea de direccion de teclado pulsada
Dim Shared As Integer EstadoLineaTeclado(16) ' almacen de teclas pulsadas
Dim Shared As Integer NumReg = 0 ' registro PSG (sonido AY)

' contador de sincronismo del CPC
Dim Shared As Integer CntHSync=0


Dim Shared AdjRam(4,8) As Integer ={ _
    {&h00000,&h00000,&h10000,&h00000,&h00000,&h00000,&h00000,&h00000}, _
    {&h04000,&h04000,&h14000,&h0C000,&h10000,&h14000,&h18000,&h1C000}, _
    {&h08000,&h08000,&h18000,&h08000,&h08000,&h08000,&h08000,&h08000}, _
    {&h0C000,&h1C000,&h1C000,&h1C000,&h0C000,&h0C000,&h0C000,&h0C000}}

coloresCPC:
Data 0,0,0	
Data 0,0,128	
Data 0,0,255	
Data 128,0,0	
Data 128,0,128	
Data 128,0,255	
Data 255,0,0	
Data 255,0,128	
Data 255,0,255	
Data 0,128,0	
Data 0,128,128	
Data 0,128,255	
Data 128,128,0	
Data 128,128,128	
Data 128,128,255	
Data 255,128,0	
Data 255,128,128	
Data 255,128,255	
Data 0,255,0	
Data 0,255,128	
Data 0,255,255	
Data 128,255,0	
Data 128,255,128	
Data 128,255,255	
Data 255,255,0	
Data 255,255,128	
Data 255,255,255	
     
     
Dim Shared as integer TablaTeclas( 128,2 ) = _
    { _
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h00    ---- 
        { &hFE, &h08 }, _       ' RAW KEY CODE = &h01    '1'
        { &hFD, &h08 }, _       ' RAW KEY CODE = &h02    '2'
        { &hFD, &h07 }, _       ' RAW KEY CODE = &h03    '3'
        { &hFE, &h07 }, _       ' RAW KEY CODE = &h04    '4'
        { &hFD, &h06 }, _       ' RAW KEY CODE = &h05    '5'
        { &hFE, &h06 }, _       ' RAW KEY CODE = &h06    '6'
        { &hFD, &h05 }, _       ' RAW KEY CODE = &h07    '7'
        { &hFE, &h05 }, _       ' RAW KEY CODE = &h08    '8'
        { &hFD, &h04 }, _       ' RAW KEY CODE = &h09    '9'
        { &hFE, &h04 }, _       ' RAW KEY CODE = &h0a    '0'
        { &hFD, &h03 }, _       ' RAW KEY CODE = &h0b    '='
        { &hFE, &h03 }, _       ' RAW KEY CODE = &h0c    '£'
        { &hBF, &h02 }, _       ' RAW KEY CODE = &h0d    '\'
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h0e    ----
        { &h7F, &h01 }, _       ' RAW KEY CODE = &h0f    F0
        { &hF7, &h08 }, _       ' RAW KEY CODE = &h10    'Q'
        { &hF7, &h07 }, _       ' RAW KEY CODE = &h11    'W'
        { &hFB, &h07 }, _       ' RAW KEY CODE = &h12    'E'
        { &hFB, &h06 }, _       ' RAW KEY CODE = &h13    'R'
        { &hF7, &h06 }, _       ' RAW KEY CODE = &h14    'T'
        { &hF7, &h05 }, _       ' RAW KEY CODE = &h15    'Y'
        { &hFB, &h05 }, _       ' RAW KEY CODE = &h16    'U'
        { &hF7, &h04 }, _       ' RAW KEY CODE = &h17    'I'
        { &hFB, &h04 }, _       ' RAW KEY CODE = &h18    'O'
        { &hF7, &h03 }, _       ' RAW KEY CODE = &h19    'P'
        { &hFB, &h03 }, _       ' RAW KEY CODE = &h1a    '@'
        { &hFD, &h02 }, _       ' RAW KEY CODE = &h1b    '['
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h1c    ----
        { &hDF, &h01 }, _       ' RAW KEY CODE = &h1d    F1
        { &hBF, &h01 }, _       ' RAW KEY CODE = &h1e    F2
        { &hDF, &h00 }, _       ' RAW KEY CODE = &h1f    F3
        { &hDF, &h08 }, _       ' RAW KEY CODE = &h20    'A'
        { &hEF, &h07 }, _       ' RAW KEY CODE = &h21    'S'
        { &hDF, &h07 }, _       ' RAW KEY CODE = &h22    'D'
        { &hDF, &h06 }, _       ' RAW KEY CODE = &h23    'F'
        { &hEF, &h06 }, _       ' RAW KEY CODE = &h24    'G'
        { &hEF, &h05 }, _       ' RAW KEY CODE = &h25    'H'
        { &hDF, &h05 }, _       ' RAW KEY CODE = &h26    'J'
        { &hDF, &h04 }, _       ' RAW KEY CODE = &h27    'K'
        { &hEF, &h04 }, _       ' RAW KEY CODE = &h28    'L'
        { &hDF, &h03 }, _       ' RAW KEY CODE = &h29    '*'
        { &hEF, &h03 }, _       ' RAW KEY CODE = &h2a    '+'
        { &hF7, &h02 }, _       ' RAW KEY CODE = &h2b    ']'
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h2c    ----
        { &hEF, &h02 }, _       ' RAW KEY CODE = &h2d    F4
        { &hEF, &h01 }, _       ' RAW KEY CODE = &h2e    F5
        { &hEF, &h00 }, _       ' RAW KEY CODE = &h2f    F6
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h30    ---- 
        { &h7F, &h08 }, _       ' RAW KEY CODE = &h31    'Z'
        { &h7F, &h07 }, _       ' RAW KEY CODE = &h32    'X'
        { &hBF, &h07 }, _       ' RAW KEY CODE = &h33    'C'
        { &h7F, &h06 }, _       ' RAW KEY CODE = &h34    'V'
        { &hBF, &h06 }, _       ' RAW KEY CODE = &h35    'B'
        { &hBF, &h05 }, _       ' RAW KEY CODE = &h36    'N'
        { &hBF, &h04 }, _       ' RAW KEY CODE = &h37    'M'
        { &h7F, &h04 }, _       ' RAW KEY CODE = &h38    '<'
        { &h7F, &h03 }, _       ' RAW KEY CODE = &h39    '>'
        { &hBF, &h03 }, _       ' RAW KEY CODE = &h3a    '?'
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h3b    ----
        { &h7F, &h00 }, _       ' RAW KEY CODE = &h3c    F'.'
        { &hFB, &h01 }, _       ' RAW KEY CODE = &h3d    F7
        { &hF7, &h01 }, _       ' RAW KEY CODE = &h3e    F8
        { &hF7, &h00 }, _       ' RAW KEY CODE = &h3f    F9
        { &h7F, &h05 }, _       ' RAW KEY CODE = &h40    ESPACE
        { &h7F, &h09 }, _       ' RAW KEY CODE = &h41    DEL
        { &hEF, &h08 }, _       ' RAW KEY CODE = &h42    TAB
        { &hBF, &h00 }, _       ' RAW KEY CODE = &h43    ENTER
        { &hFB, &h02 }, _       ' RAW KEY CODE = &h44    RETURN
        { &hFB, &h08 }, _       ' RAW KEY CODE = &h45    ESC
        { &hFE, &h02 }, _       ' RAW KEY CODE = &h46    CLR
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h47    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h48    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h49    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h4a    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h4b    ----
        { &hFE, &h00 }, _       ' RAW KEY CODE = &h4c    Fleche Haut
        { &hFB, &h00 }, _       ' RAW KEY CODE = &h4d    Fleche Bas
        { &hFD, &h00 }, _       ' RAW KEY CODE = &h4e    Fleche Droite
        { &hFE, &h01 }, _       ' RAW KEY CODE = &h4f    Fleche Gauche
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h50    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h51    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h52    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h53    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h54    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h55    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h56    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h57    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h58    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h59    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h5a    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h5b    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h5c    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h5d    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h5e    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h5f    ----
        { &hDF, &h02 }, _       ' RAW KEY CODE = &h60    SHIFT
        { &hDF, &h02 }, _       ' RAW KEY CODE = &h61    SHIFT
        { &hBF, &h08 }, _       ' RAW KEY CODE = &h62    CAPS LOCK
        { &h7F, &h02 }, _       ' RAW KEY CODE = &h63    CTRL
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h64    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h65    ----
        { &hFD, &h01 }, _       ' RAW KEY CODE = &h66    COPY
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h67    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h68    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h69    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h6a    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h6b    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h6c    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h6d    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h6e    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h6f    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h70    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h71    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h72    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h73    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h74    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h75    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h76    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h77    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h78    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h79    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h7a    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h7b    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h7c    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h7d    ----
        { &hFF, &h0F }, _       ' RAW KEY CODE = &h7e    ----
        { &hFF, &h0F }  _       ' RAW KEY CODE = &h7f    ----
    }