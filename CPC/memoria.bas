
Sub Z80Reset()
    regPC = 0
    regSP = 0
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
	Dim addr2 As Integer
	
	 addr2=addr
    addr=addr and &h3fff
    
    ' ................ AMSTRAD CPC
    ' Banco de lectura '0' (exclusivo ROM principal solo o compartido con RAM)
    If (addr2<&h4000                ) Then 
    	If TabPEEK(0)=100 Then 
    		' parches de ROM BIOS de pruebas
    		'If addr2=&h1cf6 Then Return 90
    		'If addr2=&h1cf7 Then Return 90
    		'If addr2=&h1cf8 Then end
    		Return ROM(addr) ' ROM PRINCIPAL
    	EndIf
    	addr+=TabPEEK(0)
    EndIf
    If (addr2>&h3FFF And addr2<&h8000) Then addr+=TabPEEK(1)
    If (addr2>&h7FFF And addr2<&hC000) Then addr+=TabPEEK(2)
    ' banco de lectura '3' , RAM, ROM de DISCO o ROM de BASIC
    If (addr2>&hBFFF                ) Then 
    	If TabPEEK(3)=200 Then Return ROM(addr+&h4000) ' ROM BASIC
    	If TabPEEK(3)=300 Then Return ROM(addr+&h8000) ' ROM DISCO
    	addr+=TabPEEK(3)
    EndIf
    ' ................FIN CPC

    peekb = RAM(addr) and &hFF
End Function

Function peekw(addr As integer) As Integer
    peekw = peekb(addr)+(256 * peekb(addr+1))  
End Function

Sub pokeb(addr As integer, Dato As Integer)
	Dim addr2 As Integer
	
	 addr2=addr
    addr=addr and &h3fff

    ' ................ AMSTRAD CPC: bancos de RAM/ROM
    ' en el Amstrad, cuando se escribe entre 0 y &hFFFF va todo a la RAM
    ' independientemente del banco activo     
    If (addr2<&h4000                 ) Then addr+=TabPOKE(0)
    If (addr2>&h3FFF And addr2<&h8000) Then addr+=TabPOKE(1)
    If (addr2>&h7FFF And addr2<&hC000) Then addr+=TabPOKE(2)
    If (addr2>&hBFFF                 ) Then addr+=TabPOKE(3)
    
    RAM(addr) = Dato And &hff   
End Sub

Sub pokew(addr As integer, word As Integer)
    addr=addr and &hffff
    word=word and &hffff

    pokeb addr, word And &hFF
    pokeb addr + 1, Div256((word And &hFF00))
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


