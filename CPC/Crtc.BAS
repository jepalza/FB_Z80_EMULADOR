Dim Shared As Integer RegCRTCSel
Dim Shared As Integer adr

Dim Shared As integer RegsCRTC(32)
Dim Shared As Integer OfsEcr

RegCRTCSel=0

Sub CalcAdrEcrCPC()

	Dim tc As Integer =RegsCRTC(1) shl 1
	Dim nl As Integer =RegsCRTC(6)
	Dim offset As Integer =(RegsCRTC(13)+(RegsCRTC(12) shl 8)) shl 1
	Dim ofsL As Integer =(46-RegsCRTC(2)) shl 1
	Dim reste As Integer =2048-nl * tc
	Dim adr As Integer = 0
	Dim decal As Integer
	Dim AdrCPC As Integer
	Dim TailleColonne As Integer
	Dim NbLignes As Integer
	Dim NbBlocks As Integer
	Dim TailleChar As Integer =RegsCRTC(9)+1
	Dim offset7 As Integer =259+RegsCRTC(5)-(RegsCRTC(7)*TailleChar)
	Dim f As Integer
	
    If (offset7<0) Then offset7=0
    If (ofsL<0) Then ofsL=0
 
    ' se pone a "0" cad vez que se llama
    For f=0 To &h10000: VRAM(f)=&hff:Next
    
    for NbBlocks=0 To TailleChar-1
        decal=NbBlocks+offset7
        for NbLignes=nl To 0 Step -1
            TailleColonne=tc
            AdrCPC=decal*80+ofsL
            For f=AdrCPC To 0 Step -1
              TailleColonne-=1
              If TailleColonne=0 Then Exit For
              vRAM(AdrCPC)=(adr and &h7800)+((offset+adr) and &h7FF)
              adr+=1
              AdrCPC+=1
            Next
            decal+=TailleChar
        Next
        adr+=reste
    Next

End Sub


Sub CRTC6845( adr As Integer, dato As Integer)
    If (adr And &hBF00)=&hBC00 Then
        RegCRTCSel=dato And &h1F
    else
        If (adr And &hBF00)=&hBD00 Then
            If RegsCRTC(RegCRTCSel) <> dato Then
                'si hay cambio de banco de pantalla, memorizamos solo el registro
                If (RegCRTCSel=12) and ((RegsCRTC(12) and &h0F)=(dato And &h0F)) Then
                    RegsCRTC(RegCRTCSel)=dato
                else
                    RegsCRTC(RegCRTCSel)=dato
                    CalcAdrEcrCPC()
                End If
            End If
            OfsEcr=(RegsCRTC(12) and &h30) shl 10
        End If
    End If
End sub













