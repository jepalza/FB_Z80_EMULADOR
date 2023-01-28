
Sub Z80Reset()
    regPC = 0
    regSP = 65535
    regA = 0
    setF 0
    setBC 0
    regDE = 0
    regHL = 0
    
    exx
    ex_af_af
    
    regA = 0
    setF 0
    setBC 0
    regDE = 0
    regHL = 0
    
    regIX = 0
    regIY = 0
    intR = 128
    intRTemp = 0
    
    intI = 0
    intIFF1 = Falso
    intIFF2 = Falso
    intIM = 0

    InitParity
    Divisores
    
End Sub


Sub Divisores()
    ' es mas rapido recuperar un dato precalculado, que hacerlo en tiempo real 
    Dim f as integer
    For f = 0 To 81919
        Div4    (f) = (f And 65535) \ 4
        Div32   (f) = (f And 65535) \ 32
        Div256  (f) = (f And 65535) \ 256
        Div16384(f) = (f And 65535) \ 16384
    Next f
End Sub

' precalculamos una matriz para acelerar procesos de CPU 
Sub InitParity()
    Dim b as byte
    Dim f As Integer
    Dim l As Integer
    
    For f = 0 To 255
        l = cierto
        For b = 0 To 7
            If (f And (2 ^ b)) <> 0 Then l = Not l
        Next b
        Parity(f) = l
    Next f
End Sub


''''''''''''''''''''''''''''''''''''''
''''' control de la memoria RAM ''''''
''''''''''''''''''''''''''''''''''''''
Function peekb(addr As integer) As Integer
    addr=addr and &hffff
    'if addr=&h11e3 then BreakPoint=1
    
    ' ................GALAXIAN
       ' DISWICHT
       'If addr=&h6000 Then LeeTeclado(): Return galaxbutton  
       ' DISWICHT
       'If addr=&h6800 Then LeeTeclado(): Return galaxbutton  
       ' DISWICHT
       If addr=&h7000 Then Return 1 Shl 2   
    ' ................FIN GALAXIAN

    peekb = RAM(addr) and &HFF
    
    if deb>1 then locate 1,40:print "PeekB:";hex$(addr),"Dato:";hex$(RAM(addr));"       "  

End Function

Function peekw(addr As integer) As Integer
    peekw = peekb(addr)+(256 * peekb(addr+1))
    if deb>1 then locate 2,40:print "PeekW:";hex$(addr),"Dato:";hex$(RAM(addr)+(256*RAM(addr+1)));"       "

    'If ((addr And 49152) = 16384) Then
    '    glContendedMemoryDelay = glContendedMemoryDelay + glContentionTable(-glTStates)
    'End If
    
End Function

Sub pokeb(addr As integer, Dato As Integer)
    addr=addr and &hffff

    ' ROMs del GALAXIAN hasta 16384
    If addr < &h4000 Then
        ' conjunto de ROMs, no permitimos escribir, salimos
        Exit Sub
    End If

    ' memoria de video espejo en el GALAXIAN (&h5000=&h5400) (entre la 5000 y la 57FF
    'If (addr>=&h5400 And addr<=&h57FF) Then addr=addr-&h400

    ''' GALAXIAN
       ' fondo activo?
       If addr=&h6800 Then Locate 1,55:Print "fondo:";dato
       ' sonido activo?
       If addr=&h6803 Then Locate 2,55:Print "sonido:";dato
       ' disparo activo?
       If addr=&h6805 Then Locate 3,55:Print "disparo:";dato
       ' volumen
       If addr=&h6806 Then Locate 4,55:Print "volumen:";dato
       ' estrellas 
       If addr=&h7004 Then Locate 5,55:Print "estrellas:";dato
       ' espejo X
       If addr=&h7006 Then Locate 6,55:Print "espejo X:";dato
       ' espejo Y
       If addr=&h7007 Then Locate 7,55:Print "espejo Y:";dato
       ' pitido?
       If addr=&h7800 Then Locate 8,55:Print "pitido:";dato
       ' frecuencia?
       If addr=&h6004 Then Locate 9,55:Print "frecuencia:";dato
       ' interrupciones (ver INTERRUPT abajo)
       If addr=&h7001 Then Locate 10,55:Print "Interrupcion:";dato
    '''' FIN GALAXIAN
    
    RAM(addr) = Dato And &hff

    if deb>1 then locate 3,40:print "PokeB:";hex$(addr),"Dato:";hex$(RAM(addr));"       "
    
End Sub

Sub pokew(addr As integer, word As Integer)
    addr=addr and &hffff
    word=word and &hffff

    pokeb addr, word And &HFF
    pokeb addr + 1, Div256((word And &HFF00))
    if deb>1 then locate 4,40:print "PokeW:";hex$(addr),"Dato:";hex$(RAM(addr)+(256*RAM(addr+1)));"       "
End Sub

Sub poppc()
    regPC = peekb(regSP) Or (peekb(regSP + 1) * 256)
    regSP = (regSP + 2 And &HFFFF)
End Sub

Function popw() As Integer
    popw = peekb(regSP) Or (peekb(regSP + 1) * 256)
    regSP = (regSP + 2 And &HFFFF)
End Function

 Sub pushpc()
    regSP = (regSP - 2) And &HFFFF
    pokew regSP, regPC
End Sub

Sub pushw(word As Integer)
    regSP = (regSP - 2) And &HFFFF
    pokew regSP, word
End Sub





''''''''''''''''''''''''''''''''''''
'''''''  MANEJO DE LAS IRQ '''''''''
''''''''''''''''''''''''''''''''''''

Sub RefreshIRQ(t As Integer)
    intRTemp = intRTemp + t
End Sub

Function interrupt() As Integer
    Dim lSleep As integer, lCounter As Integer
    Static hola As Integer=0
    ' PERFORM Z80 hardware functions
       interruptCounter = interruptCounter + 1
 
  
   'If (interruptCounter Mod 256)=0 Then
        'refreshFlashChars
   'End If
    'Locate 22,35:Print hola:hola=hola+1
      ' actualiza hardware
      For hola=0 To 3
        ponpantalla
      Next
        LeeTeclado
   'End if
    


    
   ' miramos el tiempo transcurrido
    lSleep = (Timer()*1000) - InterruptTimer
    
    If lSleep = 0 Then lSleep = 1
    
    If lSleep < InterruptDelay + 1 Then
        ' // too quick, we need to put in a delay
                 
        ' // 20 - 12 - overage  (overage=2)
        lCounter = InterruptDelay - lSleep - DelayOverage
        If lCounter < 0 Then
            DelayOverage = -lCounter
        Else
            DelayOverage = DelayOverage - ((InterruptDelay - lSleep) - lCounter)
            ' este sleep causa una pausa superalta y detiene el emul. estudiarlo
            if VelocidadReal then Sleep lCounter,1 ' EL SEGUNDO PARAMETRO IMPIDE QUE SE INTERRUMPA CON UNA TECLA
        End If
    Else
        DelayOverage = DelayOverage + lSleep - InterruptDelay
        If DelayOverage > 48 Then DelayOverage = 48
    End If

    InterruptTimer = timer()*1000
     
    ' GALAXIAN IRQ
    If RAM(&h7001) Then
    	pushpc
      intIFF1 = falso
      intIFF2 = falso
    	regPC= &h66 ' generamos NMI 
    	interrupt = 11' 11 ciclos que emplea la NMI
    End If   
    
    ' If it's a maskable interrupt
    If intIFF1 = Falso Then
        interrupt = 0
    Else
        Select Case intIM
        Case 0, 1
            pushpc
            intIFF1 = Falso
            intIFF2 = Falso
            regPC = 56
            interrupt = 13
        	Case 2
            pushpc
            intIFF1 = Falso
            intIFF2 = Falso
            regPC = peekw((intI * 256) Or &HFF)
            interrupt = 19
        End Select
    End If
    
End Function

