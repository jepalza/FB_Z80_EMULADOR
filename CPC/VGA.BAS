


Sub SetMemCPC( )

    TabPOKE(0)= AdjRam(0,RamSelect)
    TabPEEK(0)= IIf ((BancoMemoria and ROMINF_OFF),AdjRam(0,RamSelect),ROMINF)
    
    TabPOKE(1)= AdjRam(1,RamSelect)
    TabPEEK(1)= AdjRam(1,RamSelect)
    
    TabPOKE(2)= AdjRam(2,RamSelect)
    TabPEEK(2)= AdjRam(2,RamSelect)
    
    TabPOKE(3)= AdjRam(3,RamSelect)
    TabPEEK(3)= IIf ((BancoMemoria and ROMSUP_OFF),AdjRam(3,RamSelect),IIf((RomExt=7),ROMDSK,ROMSUP))

End Sub

Sub WriteVGA(dato As Integer)
  Dim newVal As Integer = dato and &h1F

    Select Case (dato shr 6)
        case 0 
            PenSelect=newVal
        case 1
            TabCoul(PenSelect)=newVal 
            TabInk (PenSelect)=newVal
    	  Case 2 
            BancoMemoria=dato
            lastMode=dato And 3
            SetMemCPC()
            if(dato And 16) Then
                CntHSync=0 ' contador de sincronismos a "0"
                irqsetLine=0 ' se apagan las interrupciones
            End If
        case 3 
            RamSelect=dato And 7
            SetMemCPC()
    End Select
    
End Sub

Sub WriteROM( dato as Integer)
    RomExt=dato
    SetMemCPC()
End Sub

















