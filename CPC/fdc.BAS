'Dim Shared As Integer startreal
Dim Shared As Integer motoron
Dim Shared As Integer discon
'Dim Shared As Integer endread
'Dim Shared As Integer salida

Dim Shared As Integer fdcint=0
Dim Shared As UByte fdcstatus=&h80 ' necesario para que la primera vez se indique al 6128 que tenemos FLOPPY
Dim Shared As Integer params=0,readparams=0
Dim Shared As UByte paramdat(16)
Dim Shared As UByte comando
Dim Shared As UByte st0,st1,st2,st3
Dim Shared As Integer fdctrack
Dim Shared As Integer starttrack,startsector,endsector
Dim Shared As Integer posinsector
Dim Shared As Integer reading=0
Dim Shared As Integer disctracks
Dim Shared As Integer discsects(40)
Dim Shared As UByte discdat(42,11,512)
Dim Shared As ubyte discid(42,11,4)


Sub loaddsk( discname As String )
	Dim numsect As Integer
	Dim c As Integer
	Dim d As Integer
	Dim e As Integer
	Dim head(40) As uByte
	Dim As UByte dskhead(256),trkhead(256)
	
	Dim a As string
	  a=Space(256)
	Dim b As string
	  b=Space(512)

        Open discname For Binary As 3
        
	        Get #3,,a      
	        for e=0 To Len(a)-1 :dskhead(e)=Asc(Mid(a,e+1,1)): Next
	        For e=0 To 39:head(e)=0:next
	        for e=0 To &h20 :head(e)=dskhead(e): Next
	        disctracks=dskhead(&h30)
	
	        for d=0 To disctracks-1
	        	  
	           notrackinfo: a=Space(256)
	        	  If Eof(3) Then Close 3:Exit sub  
		        Get #3,,a   
	           For e=0 To Len(a)-1 :trkhead(e)=Asc(Mid(a,e+1,1)): Next
		        a="":For e=0 To 9:a=a+Chr(trkhead(e)):Next
		        If a<>"Track-Info" Then GoTo notrackinfo 
		        
              numsect=trkhead(&h15)
              discsects(d)=numsect          
              For c=0 To numsect-1
               discid(d,c,0)=trkhead(&h18+(c shl 3))
               discid(d,c,1)=trkhead(&h19+(c shl 3))
               discid(d,c,2)=trkhead(&h1A+(c shl 3))
               discid(d,c,3)=trkhead(&h1B+(c shl 3))
               Get #3,,b     
               For e=0 To Len(b)-1
                	discdat(d,(discid(d,c,2)-1) and 15,e)=Asc(Mid(b,e+1,1))
               Next
              Next
	        next
                
        Close 3
End Sub

function ReadFDC(addr As Integer) As Integer
   Dim As Integer c
   Dim As ubyte temp

        if(addr and 1) Then
                'if(readparams=0) Then
                '    Print "Reading but no params-last comando ";comando
                '    end
                '    Return -1
                'End If
                Select Case (comando)
                        case &h04: 'Sense drive status
	                        readparams=0
	                        fdcstatus=&h80
	                        return st3
	                        
                        case &h06: 'Read sectors
                            If (reading) Then
                                temp=discdat(fdctrack,startsector-1,posinsector)
                                posinsector+=1
                                if(posinsector=512) Then
                                        if((startsector and 15)=(endsector and 15)) Then
                                                reading=0
                                                readparams=7
                                                fdcstatus=&hD0
'                                                endread=1
                                                fdcint=1
                                                discon=0

                                        else
                                                posinsector=0
                                                startsector+=1
                                                Print "nunca llega 3":ScreenCopy:Sleep 3000,1:end
                                                if((startsector and 15)=(discsects(fdctrack)+1)) Then
                                                        if(comando and &h80) Then
                                                          fdctrack+=1
                                                        End If
                                                        startsector=&hC1
                                                        Print "nunca llega 2":ScreenCopy:Sleep 3000,1:end
                                                End If
 '                                               startreal=0
                                                for c=0 To 10
                                                     if((discid(starttrack,c,2) and 15)=(startsector and 15)) Then
'                                                          startreal=c
																				Print "nunca llega 1":ScreenCopy:Sleep 3000,1:end
                                                          Exit for
                                                     End If
                                                Next
                                        End If
                                End If
                                Return temp
	                         End If
	                         readparams-=1
	                         Select Case (readparams)
	                                case 6: st0=&h40: st1=&h80: st2=0: return st0
	                                case 5: return st1
	                                case 4: return st2
	                                case 3: Return fdctrack
	                                case 2: return 0
	                                case 1: return startsector
	                                case 0: fdcstatus=&h80: return 2
	                         End Select
                       
                        case &h86: 'Read sector fail
	                        readparams-=1
	                        Select Case (readparams)
	                                case 6: st0=&h40: st1=&h84: st2=0: return st0
	                                case 5: return st1
	                                case 4: return st2
	                                case 3: return fdctrack
	                                case 2: return 0
	                                case 1: return startsector
	                                case 0: fdcstatus=&h80: return 2
	                        End Select
                       
                        case &h08: 'Sense interrupt state
	                        readparams-=1
	                        if(readparams=1) Then Return st0
	                        fdcstatus=&h80
	                        return fdctrack
	                        
                        case &h0A: 'Read sector ID
	                        readparams-=1
	                        Select Case (readparams)
	                                case 6: return st0
	                                case 5: return st1
	                                case 4: return st2
	                                case 3: return discid(fdctrack,startsector,0)
	                                case 2: return discid(fdctrack,startsector,1)
	                                case 1: return discid(fdctrack,startsector,2)
	                                case 0: fdcstatus=&h80: return discid(fdctrack,startsector,3)
	                        End Select
                       
                        'Case Else:
                        'Print "Error leyendo comando ";comando
                        'Return -1
                End Select
        Else
            return fdcstatus
        End If
End function

Sub writefdc(addr As Integer ,dato As integer)
    Dim c As Integer
      
        if(addr=&hFA7E) Then
                motoron=dato and 1
                return
        End If

        if(addr and 1) Then
                If (params) Then
                        paramdat(params-1)=dato
                        params-=1
                        if( params=0) Then
                                Select Case (comando)
                                        case &h03: 'Specify
	                                        fdcstatus=&h80
                                       
                                        case &h04: 'Sense drive status
	                                        st3=&h60
	                                        If (fdctrack=0) Then st3 Or=&h10
	                                        fdcstatus=&hD0
	                                        readparams=1
                                       
                                        case &h06: 'Read sectors
	                                        starttrack=paramdat(6)
	                                        startsector=paramdat(4) and 15
	                                        endsector=paramdat(2) and 15
'	                                        startreal=0
	                                        posinsector=0
	                                        readparams=1
	                                        reading=1
	                                        fdcstatus=&hF0
                                       
                                        case &h07: 'Recalibrate
	                                        fdcstatus=&h80
	                                        fdctrack=0
	                                        fdcint=1
                                       
                                        case &h0A: 'Read sector ID
	                                        fdcstatus Or=&h60
	                                        readparams=7
                                       
                                        case &h0F: 'Seek
	                                        fdcstatus=&h80
	                                        fdctrack=paramdat(0)
	                                        fdcint=1
                                       
                                        'Case Else:
                                        '  Print "Executing bad comando ",comando
                                        '  Exit Sub
                                          
                                End Select
                        End If
                Else
                             comando=dato and &h1F
                             Select Case (comando)
                             	  Case 0: 
                             		  Exit Sub 'invalid
                             		  
                                Case &h1F: 
                             		  Exit sub 'invalid
                             		  
                                case &h03: 'Specify
	                                params=2
	                                fdcstatus Or=&h10
                               
                                case &h04: 'Sense drive status
	                                params=1
	                                fdcstatus Or=&h10
                               
                             	  Case &h06: 'Read sectors
	                                params=8
	                                fdcstatus Or=&h10
	                                discon=1
                               
                                case &h07: 'Recalibrate
	                                params=1
	                                fdcstatus Or=&h10
                               
                                case &h08: 'Sense interrupt state
	                                st0=&h21
	                                If (fdcint=0) Then 
	                                	 st0 Or=&h80
	                                else 
	                                  fdcint=0
	                                End If
	                                fdcstatus Or=&hD0
	                                readparams=2
                               
                                case &h0A: 'Read sector ID
	                                params=1
	                                fdcstatus Or=&h10
                               
                                case &h0F: 'Seek
	                                params=2
	                                fdcstatus Or=&h10
                               
                                'Case Else:
                                '	Print "Starting bad comando ",comando
                                
                             End Select
                End If
        End If


End Sub
