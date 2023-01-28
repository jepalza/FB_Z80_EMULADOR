' RAM del Z80
Dim Shared RAM(65535) As Integer 

' boleanas
Dim Shared Falso As Integer = 0
Dim Shared cierto As Integer = -1

' para que salga info por pantalla
Dim Shared deb As Byte = 0 ' 0 NO DEB, 1 DEB REGISTROS, 2 DEB REGISTROS+POKES-PEEKS
Dim Shared VelocidadReal As Byte = 0 ' 0 ultrarapido, 1 velocidad Spectrum real
Dim Shared x2 As Integer = 1 ' doble tamaño de pantalla

' interrupciones
Dim Shared TstatesPerInterrupt As Integer
Dim Shared InterruptTimer As Integer
Dim Shared interruptCounter As Integer
Dim Shared InterruptDelay As Integer
Dim Shared DelayOverage As Integer
Dim Shared TStates As Integer ' contador hacia atras. al llegar a cero se genera IRQ

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


' ------------------------------------------------------------------

' exclusivas del GALAXIAN
Dim Shared galaxbutton As Integer ' nada, solo temporal para teclado (sin uso)
Dim Shared galaxbutton1 As Integer ' estado botones
Dim Shared galaxbutton2 As Integer ' insertar monedas
Dim shared inidatos As integer=0 ' para depuracion, borrar cuando se acabe
Dim Shared galax_version As Integer ' para elegir una de las multiples versiones GALAXIAN
Dim Shared rom_graf_ini As Integer=&h3000 ' direccion de inicio de las ROMS graficas
Dim Shared directorio As String ' directorio de las roms (dentro a su vez de "ROMS")
Dim Shared pp(3) As Integer={0,16,8,24} ' posiciones de los graficos de los bichos
