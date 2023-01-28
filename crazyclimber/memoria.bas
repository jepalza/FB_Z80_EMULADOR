
Sub Divisores()
    ' es mas rapido recuperar un dato precalculado, que hacerlo en tiempo real 
    ' por eso, hacemos los calculos y los guardamos para futuras operaciones
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
        l = True
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
    ' simple y rapido punto de parada
    'if addr=&h1ef then BreakPoint=1
        'if addr>&h8000 and addr<&h83ff then if ram(addr)=0 then print #1,hex(addr)
        
'if addr=&h83cb then return int(rnd(1)*256)
'if addr=&h8325 then return int(rnd(1)*256)
'if addr=&h8395 then return int(rnd(1)*256)
'if addr=&h83cd then return int(rnd(1)*256)

       ' dip suiches
        if addr=&hb000 then         	
        	 return 255-&b10000100
        endif
       ' dip suiches 2
        if addr=&hb800 then         	
        	 return 255-&b0
        endif
        
        '' dip suiches 2
        if addr=&h461 then   
        	        'intIM=2
        	        intIFF1=true
        	'return &hac
        endif
        '        ' dip suiches 2
        'if addr=&h82Fd then     
        '	RAM(addr)=(RAM(addr) and &HFF)+1
        '	 return (RAM(addr) and &HFF)
        'endif
        '        ' dip suiches 2
        'if addr=&h82Fe then     
        '	RAM(addr)=(RAM(addr) and &HFF)+1
        '	 return (RAM(addr) and &HFF)
        'endif
        '        ' dip suiches 2
        'if addr=&h82Ff then     
        '	RAM(addr)=(RAM(addr) and &HFF)+1
        '	 return (RAM(addr) and &HFF)
        'endif
        '        ' dip suiches 2
        'if addr=&h8122 then     
        '	RAM(addr)=(RAM(addr) and &HFF)+1
        '	 return (RAM(addr) and &HFF)
        'endif

               
    peekb = RAM(addr) and &HFF
    if deb>1 then locate 1,40:msg "PeekB:"+hex$(addr)+" Dato:"+hex$(RAM(addr))+"       ",0  
end Function

Function peekw(addr As integer) As Integer
    peekw = peekb(addr)+(256 * peekb(addr+1))
    if deb>1 then locate 2,40:msg "PeekW:"+hex$(addr)+" Dato:"+hex$(RAM(addr)+(256*RAM(addr+1)))+"       ",0   
End Function

Sub pokeb(addr As integer, Dato As Integer)
    addr=addr and &hffff

    If addr < MAXROM Then
        ' estamos en ROM
        Exit Sub
    End If

    RAM(addr) = Dato And &hff
    if deb>1 then locate 3,40:msg "PokeB:"+hex$(addr)+" Dato:"+hex$(RAM(addr))+"       ",0
    
End Sub

Sub pokew(addr As integer, word As Integer)
    addr=addr and &hffff
    word=word and &hffff

    pokeb addr, word And &HFF
    pokeb addr + 1, word Shr 8 'Div256((word And &HFF00))
    if deb>1 then locate 4,40:msg "PokeW:"+hex$(addr)+" Dato:"+hex$(RAM(addr)+(256*RAM(addr+1)))+"       ",0
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
