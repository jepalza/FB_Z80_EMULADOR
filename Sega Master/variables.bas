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

' exclusivas del SMS/GG
Dim Shared SmsButton As Integer ' nada, solo temporal para teclado (sin uso)
Dim Shared SmsButton1 As Integer ' estado botones
Dim Shared SmsButton2 As Integer ' insertar monedas
Dim shared inidatos As integer=0 ' para depuracion, borrar cuando se acabe
Dim Shared veces_a_pintar As Integer=0 ' para pintar la pantalla cada X veces
Dim Shared veces_a_pintar2 As Integer=0 ' para pintar la pantalla cada X veces

Dim Shared cartRom(&hc000)         As Byte                'ROM Pages, 0,1,2 (SaveRAM)
Dim Shared cartRam(8192)           As Byte                'Cart RAM.  (8K)

Dim Shared Page1 						  As Integer = &h4000    'Value For Page 1, (Memory Mapping).
Dim Shared Page2                   As Integer = &h8000    'Value For Page 2, (Memory Mapping).
Dim Shared Mul4000(64)             As Integer             'Lookup table for * &h4000

Dim Shared pages()                 As Byte                'Holds ROM file data.
Dim Shared number_of_pages         As Integer             'How many pages exist in a ROM.
Dim Shared frame_two_rom           As Integer             'Does frame 2 use ROM or SRAM.
Dim Shared SRAM(16384)             As Byte                'Cartridge Ram Page 1 (Save RAM). 

Dim Shared SMS_WIDTH               As Integer = 256       'SMS Screen width
Dim Shared SMS_HEIGHT              As Integer = 192       'SMS Screen height

Dim Shared VRAM(16*1024+1)           As Integer             'Video RAM.
Dim Shared CRAM(64)                As Integer             'Colour RAM.
Dim Shared display(49408)          As Integer

Dim Shared vdpreg(16)              As Integer             'VDP Registers.
Dim Shared status                  As Integer             'Status Register.

Dim Shared first_byte              As Integer             'First or Second Byte of Command Word.
Dim Shared command_byte            As Integer             'Command Word First Byte Latch.
Dim Shared location                As Integer             'Location in VRAM.
Dim Shared operation               As Integer             'Store type of operation taking place.
Dim Shared read_buffer             As Integer             'Buffer VRAM Reads.

Dim Shared linenum                 As Integer             'Current Line Number.
Dim Shared counter                 As Integer             'Vertical Line Interrupt Counter.
Dim Shared lineint                 As Integer             'Line interrupt Pending.
Dim Shared frameint                As Integer             'Frame interrupt Pending.

Dim Shared bg_priority(270)        As Integer             'Background Priorites.
Dim Shared SMS_PALETTE(63)         As Integer             'SMS Color palette

Dim Shared H_START As Integer = 0         'Horizontal viewport start
Dim Shared H_END   As Integer 
           H_END = SMS_WIDTH 'Horizontal viewport end
           
'Dim shared lineno        As Integer         'Current VDP Scanline.
Dim shared Tcycles       As Integer = 228   'Number of Tcycles Per Second.
Dim Shared irqsetLine    As Integer         'Interrupt Line.


