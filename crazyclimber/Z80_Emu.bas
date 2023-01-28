' incluye definiciones de directorios
#include "dir.bi"

' inclusiones para las definiciones de teclado (comando multikey)
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#endif

' declaraciones
#Include "declaraciones.bas"

' variables
#Include "variables.bas"

' rutinas de manejo de memoria (PEEK, POKE, IRQ)
#Include "memoria.bas"

' incluimos el nucleo emulador Z80
#include "Z80.bas"

' incluimos el nucleo Spectrum
#include "hard.bas"


Sub LoadROM(sROMFile As String, inicio As Integer, tamano As Integer)
    Dim hFile As integer, sROM As String, lCounter As Integer

    hFile = Freefile
    Open sROMFile For Binary As hFile
        sROM = Input(tamano, hFile) ' leemos de golpe todo el fichero
    Close hFile
    
    ' lo metemos en RAM en la dir "inicio"
    For lCounter = 1 To tamano
        RAM(inicio+(lCounter - 1)) = Asc(Mid$(sROM, lCounter, 1))
    Next lCounter

End sub


Sub LoadGRF(sROMFile As String, inicio As Integer, tamano As Integer)
    Dim hFile As integer, sROM As String, lCounter As Integer

    hFile = Freefile
    Open sROMFile For Binary As hFile
        sROM = Input(tamano, hFile) ' leemos de golpe todo el fichero
    Close hFile
    
    ' lo metemos en RAM en la dir "inicio"
    For lCounter = 1 To tamano
        GRAF(inicio+(lCounter - 1)) = Asc(Mid$(sROM, lCounter, 1))
    Next lCounter

End sub



'/////////////////////////////////////////////////////////////////////////////////////////////////////
'*******************************************************************************************************
'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


    ' maximo error de una interrupcion antes de "frenar" al PC, en milisegundos
    InterruptDelay = 20
    
    ' PRINCIPAL: ARRANCAMOS LA PANTALLA Y LA EMULACION
    ' abrimos una de 640x480x32bpp con una copia oculta para dibujar
    Screen 19 ,,2 'habilitar las dos paginas si queremos dibujar sin parpadeos

    Print "teclas avanzar y retroceder pagina para mover la zona de ram vista."
    Print "F1 para parar la ejecucion"
    Print "F2 para continuar"
    Print "de prueba, emulo (mal) un MPF-1P, para ver que funciona"
    Print "de muestra, saco a un fichero PP.TXT los resultados del OUT e IN"
    Print "borra estas lineas una vez comprendido"
    'Sleep
	cls

    ResetKeyboard
    Z80Reset
        
    ' 69888 tstates per interrupt (3.50000 MHz)
    ' se calcula como MHZ/50hz (o 60hz si es pal)
    ' ejemplo: 3.5mhz=3500000/50=70000 aprox. 69888 (mas real)
    TotalStates = 69888
    TiempoReal = int(Timer()*1000)
    
    ' zona de ram que desamos ver en pantalla para depurar
    verram=&h9000 ' 48*10 los caracteres qu vemos de golpe

    ' salida de datos para depuracion
	 Open "pp.txt" For Output As 1
	 
    ' habilitamos la pagina 1 para dibujar oculto, la pagina 0 para la visible
    ' sino esta habilitado al crear el SCREEN , no afecta, y siempre es visible
    Screenset 1,0 

    MAXROM=&h5000 ' para que la rutina POKE no grabe en rom por accidente
    LoadROM "roms/prog.bin",&h0000,&h5000 ' 20k de rom
    
    ' en la   0x0000 primer banco de graficos ->primer plano
    ' en la   0x0800 nada, vacio
    ' seguido 0x1000    ->segundo plano
    ' en la   0x1800 nada, vacio
    
    ' en la   0x2000 segundo banco de graficos ->primer plano (letras)
    ' en la   0x2800 nada, vacio
    ' seguido 0x3000     ->segundo plano
    ' en la   0x3800 nada, vacio
    
   loadgrf "roms/gfx1.bin",&h0000,&h4000 ' 16k (8+8) pero en realidad, cada 2k esta vacio, solo son 8k
    ' big sprites, dos planos, 0x0000 y 0x0800
    loadgrf "roms/gfx2.bin",&h4000,&h1000 ' 4k de rom  
    

    ' // Begin the Z80 execution loop, this drives the whole emulation
    sleep 100 ' necesario para que le de tiempo al teclado a vaciarse
    execute