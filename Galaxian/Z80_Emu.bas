' incluye definiciones de directorios
#include "dir.bi"

' inclusiones para las definiciones de teclado y graficos (comando multikey o get_graphics)
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#EndIf


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


Sub Carga_ROM(fich As String, Direccion As Integer)
    Dim rom As String, f As Integer

    rom="roms\"
    If directorio<>"" Then rom=rom+directorio+"\"
    fich=rom+fich ' si no hay directorio, se cogen de ROMS
    'hFile = Freefile
    Open fich For Binary As 1
    
    ' cogemos toda la ROM de golpe
    rom = Input(Lof(1), 1) 
 
    
    For f = 1 To Lof(1)
        RAM((f - 1)+Direccion) = Asc(Mid(ROM, f, 1))
    Next
 
    Close 1
End Sub



'/////////////////////////////////////////////////////////////////////////////////////////////////////
'*******************************************************************************************************
'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


    
    InterruptDelay = 20
    
    ' PRINCIPAL: ARRANCAMOS LA PANTALLA Y LA EMULACION
    inicia_pantalla()
    

    ResetKeyboard
    Z80Reset
        
    ' 50000 tstates per interrupt (3.072 MHz) (galaxian)
    ' 69888 tstates per interrupt (3.500 MHz) (spectrum sinclair)
    TstatesPerInterrupt = 50000
    TStates = -TstatesPerInterrupt
    InterruptTimer = Timer()*1000

    ' habilitamos la pagina 1 para dibujar oculto, la pagina 0 para la visible
    Screenset 1,0 

    ' cargamos una de las diferentes versiones de GALAXIAN conocidas (1 a 8)
    galax_version=1 ' 1 orig, 2 midway, 3 turbo, 4 pirata, (8 test)
    #Include "roms.bas"

    ' // Begin the Z80 execution loop, this drives the whole emulation
    sleep 100 ' necesario para que le de tiempo al teclado a vaciarse
    execute





