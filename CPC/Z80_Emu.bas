' emulador de SEGA MASTER SYSTEM y GAME GEAR.
' el nucleo Z80 esta copiado de otro y convertido a FREEBASIC por Joseba Epalza <jepalza@gmail.com>
' el nucleo VDP de la SEGA SMS es de John Casey <xshifu@msn.com>
' el resto de rutinas son un compendio de información rebuscada por internet y adaptada por mi
' para que funcione en el FREEBASIC

' incluye definiciones de directorios
#include "dir.bi"

' inclusiones para las definiciones de teclado y graficos (comando multikey o get_graphics)
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB '' Scan code constants are stored in the FB namespace in lang FB
#EndIf

' directorio que contiene los "carchutos"
Dim Shared Directorio As String 
Directorio = "CART/"

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

' Video Gate Array (VGA) de Amstrad CPC
#include "VGA.bas"

' VRTC 8255 control de video
#Include "Crtc.bas"

' FloppyDiscController FDC del CPC
#Include "FDC.bas"


Sub Carga_ROM(fich As String, Direccion As Integer)
    Dim datos As String, f As Integer, directorio As string

    directorio="roms\"
    fich=directorio+fich
    Open fich For Binary As 1
    
    ' cogemos toda la ROM de golpe
    datos = Input(Lof(1), 1) 
 
    
    For f = 1 To Lof(1)
        ROM((f - 1)+Direccion) = Asc(Mid(datos, f, 1))
    Next
 
    Close 1
End Sub


Sub eligedisco()
    Dim fichero As String
    Dim f As Integer = 0
    Dim x As Integer
    Dim y As Integer =0
    Dim Salir As Integer = 0
    Dim Encontrados( 7000 ) As String
    Dim directorio As String
    
    Directorio="discos/"
   
    Cls
    fichero = Dir( directorio+"*", fbArchive )
    Do
        f+=1
        encontrados(f)=fichero
        fichero = Dir( )
    Loop While Len( fichero ) > 0
    f+=1
    ' "borramos" los ultimos 20 para crear un espacio vacio
    For x=f To f+20:encontrados(x)=" ":Next

    Locate 1,1:Print "Elige un disco para cargar"
    Locate 2,1:Color 0,7:Print encontrados(y+1):Color 7,0
    For x=2 To 20
        Locate x+1,1:Print encontrados(x+y)
    Next
    While salir=0
       If Multikey(SC_ESCAPE) Then end
       If Multikey(SC_SPACE) Then Salir=2
       If Multikey(SC_ENTER) Then Salir=1
       If Multikey(SC_DOWN) Then
          y+=1
          If y>f-2 Then y=f-2
          Locate 2,1:Color 0,7:Print encontrados(y+1);:Color 7,0:Print "                   "
          For x=2 To 20
            Locate x+1,1:Print encontrados(x+y);"                  ";
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
    
    ' Si pulsamos ESC salimos
    If Salir=2 Then Exit Sub

    ' si pulsamos ENTER cargamos el fichero elegido
    loaddsk (directorio+encontrados(y+1))

End sub



'/////////////////////////////////////////////////////////////////////////////////////////////////////
'*******************************************************************************************************
'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


    ' PRINCIPAL: ARRANCAMOS TODO desde cero
    inicia_pantalla
    Z80Reset   
    InitParity
    Divisores
    
    ' pone a cero las lineas del teclado
    Dim f As integer
	 For f=0 To 9
	 	EstadoLineaTeclado(f)=255
	 Next
	 
    ' por defecto, ponemos los bancos a "0" en el VideoGateArray del Amstrad CPC
    WriteVGA(128)
        
    ' 69888 tstates per interrupt (3.50000 MHz) : velocidad de un Z80 real
    ' 80000 = 4mhz (CPC?)
    TstatesPerInterrupt = 80000
    ResTStates = -TstatesPerInterrupt

    ' menu de carga 
    'If Command="" Then eligedisco

    ' habilitamos la pagina 1 para dibujar oculto, la pagina 0 para la visible
    ScreenSet 1,0 
    
    ' cargamos roms 6128
    'Carga_ROM("cpc6128.rom",&h0000)
    Carga_ROM("os6128.rom",&h0000)
    Carga_ROM("basic6128.rom",&h04000)
    Carga_ROM("AMSDOS.rom",&h8000)
    'Carga_ROM("A.rom",&h8000)


    'loaddsk("discos/6128P_SP_1.dsk") ' para el CPM debe ser TabPOKE(1) en HARD
    'loaddsk("discos/venus52.dsk")
    'loaddsk("discos/16-utilities.dsk")
    loaddsk("discos/abracadabra.dsk")

    ' inicia la emulacion sin fin
    execute





