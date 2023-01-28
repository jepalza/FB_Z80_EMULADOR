
Sub Z80Reset()
    regPC = 0
    regSP = &hDFF0 ' pila en la SMS/GG
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
    intRTemp = 128
    
    intI = 0
    intIFF1 = Falso
    intIFF2 = Falso
    intIM = 0
    
End Sub


' mapea bloques del cartucho mediante punteros
' a=destino, b=origen, c=tamaño (normalmente 16384)
Sub CopyMemory (a As integer , b As integer , c As Integer)
	Dim f As Integer
	Dim d As UInteger Ptr
	Dim s As uinteger Ptr
	
	d=a
	s=b

   ' si copio de 4 en 4 (entidad UINTEGER) es 4 veces mas rapido
	For f=0 To (c/4)-1
		d[f]=*s
		s+=1
	Next


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
    Dim peekv As integer
    addr=addr and &hffff
    

    If      (addr < &hC000) Then     '// ROM (And Cart RAM).
                                        peekv = cartRom(addr)
    ElseIf  (addr < &hE000) Then     '// RAM (Onboard). 8k
                                        peekv = cartRam(addr - &hC000)
    ElseIf (addr < &h10000) Then     '// RAM (Mirrored) 8k
                                        peekv = cartRam(addr - &hE000)
    End If

    'peekb = RAM(addr) and &hFF
    Return peekv And &hff
    'if deb>1 then locate 1,40:print "PeekB:";hex$(addr,4),"Dato:";hex$(peekv,2) 

End Function

Function peekw(addr As integer) As Integer
    peekw = peekb(addr)+(256 * peekb(addr+1))
    'if deb>1 then locate 2,40:print "PeekW:";hex$(addr,4),"Dato:";hex$(RAM(addr)+(256*RAM(addr+1)),4)
   
End Function

Sub pokeb(addr As integer, Dato As Integer)

   Dim temp As Integer

   'dato And=&hff
   'addr And=&hFFFF
   
   If (addr < &h8000) Then
                                   Exit Sub ' ROM entre 0 y 16383, no se graba nada
   ElseIf (addr < &hC000) Then     '// ROM (And Cart RAM).
                                        cartRom(addr) = Dato
   ElseIf (addr < &hE000) Then     '// RAM (Onboard) 8k
                                        cartRam(addr - &hC000) = Dato
   ElseIf (addr < &hFFFC) Then     '// RAM (Mirrored)
                                        cartRam(addr - &hE000) = Dato
   ElseIf (addr < &h10000) Then
    
    '-----------------------------+
    '       Paging Registers      |
    '-----------------------------+
    
    '-----------------------------+
    '   SRAM/ROM Select Register. |
    '-----------------------------+
    If (addr = &hFFFC) Then

        If ((Dato And 8) = 8) Then     '// Uses SRAM.
        frame_two_rom = falso
                
            If ((Dato And 4) = 4) Then '// Uses SRAM Page 0.
                '[Enter SRAM Page 0 Code]
            Else
                CopyMemory @cartRom(Page2), @SRAM(0), Page1
            End If
            
        Else
            If frame_two_rom = falso Then CopyMemory @SRAM(0), @cartRom(Page2), Page1
            frame_two_rom = cierto
        End If
    
    '-----------------------------+
    '       Page 0 Rom Bank       |
    '-----------------------------+
    ElseIf (addr = &hFFFD) Then

        temp = Mul4000(Dato Mod number_of_pages) + &h400
        CopyMemory @cartRom(&h400), @pages(temp), &h3C00
        
    '-----------------------------+
    '      Page 1 Rom Bank.       |
    '-----------------------------+
    ElseIf (addr = &hFFFE) Then

        temp = Mul4000(Dato Mod number_of_pages)
        CopyMemory @cartRom(Page1), @pages(temp), Page1
    
    '-----------------------------+
    '      Page 2 Rom Bank.       |
    '-----------------------------+
    ElseIf (addr = &hFFFF) Then  

        If (frame_two_rom) Then
            temp = Mul4000(Dato Mod number_of_pages)
            CopyMemory @cartRom(Page2), @pages(temp), Page1
        End If
    End If

    cartRam(addr - &hE000) = Dato
   End If

    
    'RAM(addr) = Dato And &hff

    'if deb>1 then locate 3,40:print "PokeB:";hex$(addr,4),"Dato:";hex$(RAM(addr),2)
    
End Sub

Sub pokew(addr As integer, word As Integer)
    addr=addr and &hffff
    word=word and &hffff

    pokeb addr, word And &hFF
    pokeb addr + 1, Div256((word And &hFF00))
    'if deb>1 then locate 4,40:print "PokeW:";hex$(addr,4),"Dato:";hex$(RAM(addr)+(256*RAM(addr+1)),4)
End Sub

Sub poppc()
    regPC = peekb(regSP) Or (peekb(regSP + 1) * 256)
    regSP = (regSP + 2 And &hFFFF)
End Sub

Function popw() As Integer
    popw = peekb(regSP) Or (peekb(regSP + 1) * 256)
    regSP = (regSP + 2 And &hFFFF)
End Function

Sub pushpc()
    regSP = (regSP - 2) And &hFFFF
    pokew regSP, regPC
End Sub

Sub pushw(word As Integer)
    regSP = (regSP - 2) And &hFFFF
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
    'Static hola As Integer=0
    ' PERFORM Z80 hardware functions
       'interruptCounter = interruptCounter + 1
 
  
   'If (interruptCounter Mod 256)=0 Then
        'refreshFlashChars
   'End If
    'Locate 22,35:Print hola:hola=hola+1
      ' actualiza hardware
      'For hola=0 To 3
        'ponpantalla
      'Next
        'LeeTeclado
   'End if
    


   ' 
   '' miramos el tiempo transcurrido
   ' lSleep = (Timer()*1000) - InterruptTimer
   ' 
   ' If lSleep = 0 Then lSleep = 1
   ' 
   ' If lSleep < InterruptDelay + 1 Then
   '     ' // too quick, we need to put in a delay
   '              
   '     ' // 20 - 12 - overage  (overage=2)
   '     lCounter = InterruptDelay - lSleep - DelayOverage
   '     If lCounter < 0 Then
   '         DelayOverage = -lCounter
   '     Else
   '         DelayOverage = DelayOverage - ((InterruptDelay - lSleep) - lCounter)
   '         ' este sleep causa una pausa superalta y detiene el emul. estudiarlo
   '         if VelocidadReal then Sleep lCounter,1 ' EL SEGUNDO PARAMETRO IMPIDE QUE SE INTERRUMPA CON UNA TECLA
   '     End If
   ' Else
   '     DelayOverage = DelayOverage + lSleep - InterruptDelay
   '     If DelayOverage > 48 Then DelayOverage = 48
   ' End If

   ' InterruptTimer = timer()*1000
    If halt Then halt=falso :regPC = (regPC + 1)
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
            regPC = peekw((intI * 256) Or &hFF)
            interrupt = 19
        End Select
    End If
    
End Function

