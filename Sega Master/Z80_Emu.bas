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

' modulo VDP en la Sega SMS/GG
#Include "vdp.bas"

' rutinas de manejo de memoria (PEEK, POKE, IRQ)
#Include "memoria.bas"

' incluimos el nucleo emulador Z80
#include "Z80.bas"

' incluimos el nucleo Spectrum
#include "hard.bas"


Sub Carga_ROM(fich As String, Direccion As Integer)
    Dim rom As String, f As Integer


    Open fich For Binary As 1
     f = LOF(1)
     ReDim pages(f)
     Get #1, , pages()
    Close 1
              
    number_of_pages = (f / &h4000) ' numero de paginas de tamaño 16384

    If (f >= &hC000) Then f = &hC000
    CopyMemory @cartRom(0), @pages(0), f

  
    Close 1
End Sub


Sub eligecartucho()
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
    f+=1
    ' "borramos" los ultimos 20 para crear un espacio vacio
    For x=f To f+20:encontrados(x)=" ":Next

    Locate 1,1:Print "Elige un cartucho para cargar o pulsa ESPACIO para carga MY_HERO.SMS"
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
    
    ' Si pulsamos ESC cargamos la ROM interna de la SMS (MY_HERO.SMS)
    If Salir=2 Then Carga_ROM directorio+"MY_HERO.SMS",0:Exit Sub

    ' si pulsamos ENTER cargamos el fichero SNA elegido
    carga_ROM directorio+encontrados(y+1),0:exit sub
    do : loop while inkey$<>""
End sub



'/////////////////////////////////////////////////////////////////////////////////////////////////////
'*******************************************************************************************************
'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


    
    'InterruptDelay = 20
    
    ' PRINCIPAL: ARRANCAMOS TODO desde cero
    inicia_pantalla
    inicia_VDP ' este modulo no es mio, es "copiado" de John Casey
    Z80Reset   
    InitParity
    Divisores
        
    '     
    TstatesPerInterrupt = TCycles ' cada 228 ciclos una IRQ???? 

    ' menu de carga de cartuchos GG y SMS
    If Command="" Then eligecartucho Else Carga_ROM (Command,0)
        
    ' cargamos un cartucho de prueba
    'Carga_ROM directorio+"Strider (Unknown) (Demo).sms.SMS",0
    'Carga_ROM directorio+"MY_HERO.SMS",0


    ' habilitamos la pagina 1 para dibujar oculto, la pagina 0 para la visible
    ScreenSet 1,0 
    
    ' // Begin the Z80 execution loop, this drives the whole emulation
    sleep 100 ' necesario para que le de tiempo al teclado a vaciarse
    execute





