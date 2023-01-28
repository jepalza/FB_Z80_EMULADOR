' rutinas de emulacion hardware
Sub inicia_pantalla()

   ' abrimos una de 64&h48&h32bpp con una copia oculta para dibujar	
	Screen 18,,2

	Dim As Integer a,r,g,b
	
	Restore coloresCPC
	For a=0 To 26
			Read r,g,b
			Palette a,r,g,b
	Next   
   
   ' colores iniciales modo 0 (160x200)
   CPC_Colores_Modo0( 0)=1
   CPC_Colores_Modo0( 1)=24
   CPC_Colores_Modo0( 2)=20
   CPC_Colores_Modo0( 3)=6
   CPC_Colores_Modo0( 4)=26
   CPC_Colores_Modo0( 5)=0
   CPC_Colores_Modo0( 6)=2
   CPC_Colores_Modo0( 7)=8
   CPC_Colores_Modo0( 8)=10
   CPC_Colores_Modo0( 9)=12
   CPC_Colores_Modo0(10)=14
   CPC_Colores_Modo0(11)=16
   CPC_Colores_Modo0(12)=18
   CPC_Colores_Modo0(13)=22
   CPC_Colores_Modo0(14)=3
   CPC_Colores_Modo0(15)=5

   ' colores iniciales modo 1 (320x200)
   CPC_Colores_Modo1(0)=1
   CPC_Colores_Modo1(1)=24
   CPC_Colores_Modo1(2)=20
   CPC_Colores_Modo1(3)=6
   
   ' colores iniciales modo 2 (640x200)
   CPC_Colores_Modo2(0)=1
   CPC_Colores_Modo2(1)=24
End Sub

Sub inicia_teclado()
	 'Dim f As Integer
	 ' vacia el teclado
    'Do : loop while inkey$<>""
        ' pone a cero las lineas del teclado
    Dim f As integer
    For f=0 To 15
    	EstadoLineaTeclado(f)=255
    Next
End Sub

Sub LeeTeclado()
	
	' cada vez que entramos, borramos las teclas
	inicia_teclado
	
	' escape con numerico FIN
	if MultiKey(SC_END)  Then end
	
	' controles
	'if MultiKey(SC_LSHIFT)   Then EstadoLineaTeclado(2)=&hdf
	'if MultiKey(SC_RSHIFT)   Then EstadoLineaTeclado(2)=&hdf
	if MultiKey(SC_CONTROL)  Then EstadoLineaTeclado(2)=&h7f
	if MultiKey(SC_ENTER)    Then EstadoLineaTeclado(0)=&hbf
	if MultiKey(SC_SPACE)    Then EstadoLineaTeclado(5)=&h7f
	if MultiKey(SC_BACKSPACE)Then EstadoLineaTeclado(9)=&h7f
	if MultiKey(SC_TAB)      Then EstadoLineaTeclado(8)=&hef
	if MultiKey(SC_HOME)     Then EstadoLineaTeclado(2)=&hfe
	if MultiKey(SC_ESCAPE)   Then EstadoLineaTeclado(8)=&hfb
	
	' letras
	'if MultiKey(SC_A) then print
	'if MultiKey(SC_B) then Print
	'if MultiKey(SC_C) then Print
	'if MultiKey(SC_D) then Print
	'if MultiKey(SC_E) then Print
	'if MultiKey(SC_F) then Print
	'if MultiKey(SC_G) then Print
	'if MultiKey(SC_H) then Print
	'if MultiKey(SC_I) then Print
	'if MultiKey(SC_J) then Print
	'if MultiKey(SC_K) then Print
	'if MultiKey(SC_L) then Print
	'if MultiKey(SC_M) then Print
	'if MultiKey(SC_N) then Print
	'if MultiKey(SC_O) then Print
	'if MultiKey(SC_P) then Print
	'if MultiKey(SC_Q) then Print
	'if MultiKey(SC_R) then Print
	'if MultiKey(SC_S) then Print
	'if MultiKey(SC_T) then Print
	'if MultiKey(SC_U) then Print
	'if MultiKey(SC_V) then Print
	'if MultiKey(SC_W) then Print
	'if MultiKey(SC_X) then Print
	'if MultiKey(SC_Y) then Print
	'if MultiKey(SC_Z) then Print
	
	' letras, pero separadas por filas QWERTY
	If MultiKey( SC_Q ) Then EstadoLineaTeclado ( &h08 ) = &hF7
	If MultiKey( SC_W ) Then EstadoLineaTeclado ( &h07 ) = &hF7
	If MultiKey( SC_E ) Then EstadoLineaTeclado ( &h07 ) = &hFB
	If MultiKey( SC_R ) Then EstadoLineaTeclado ( &h06 ) = &hFB
	If MultiKey( SC_T ) Then EstadoLineaTeclado ( &h06 ) = &hF7
	If MultiKey( SC_Y ) Then EstadoLineaTeclado ( &h05 ) = &hF7
	If MultiKey( SC_U ) Then EstadoLineaTeclado ( &h05 ) = &hFB
	If MultiKey( SC_I ) Then EstadoLineaTeclado ( &h04 ) = &hF7
	If MultiKey( SC_O ) Then EstadoLineaTeclado ( &h04 ) = &hFB
	If MultiKey( SC_P ) Then EstadoLineaTeclado ( &h03 ) = &hF7
	'
	If MultiKey( SC_A ) Then EstadoLineaTeclado ( &h08 ) = &hDF
	If MultiKey( SC_S ) Then EstadoLineaTeclado ( &h07 ) = &hEF
	If MultiKey( SC_D ) Then EstadoLineaTeclado ( &h07 ) = &hDF
	If MultiKey( SC_F ) Then EstadoLineaTeclado ( &h06 ) = &hDF
	If MultiKey( SC_G ) Then EstadoLineaTeclado ( &h06 ) = &hEF
	If MultiKey( SC_H ) Then EstadoLineaTeclado ( &h05 ) = &hEF
	If MultiKey( SC_J ) Then EstadoLineaTeclado ( &h05 ) = &hDF
	If MultiKey( SC_K ) Then EstadoLineaTeclado ( &h04 ) = &hDF
	If MultiKey( SC_L ) Then EstadoLineaTeclado ( &h04 ) = &hEF
	'
	If MultiKey( SC_Z ) Then EstadoLineaTeclado ( &h08 ) = &h7F
	If MultiKey( SC_X ) Then EstadoLineaTeclado ( &h07 ) = &h7F
	If MultiKey( SC_C ) Then EstadoLineaTeclado ( &h07 ) = &hBF
	If MultiKey( SC_V ) Then EstadoLineaTeclado ( &h06 ) = &h7F
	If MultiKey( SC_B ) Then EstadoLineaTeclado ( &h06 ) = &hBF
	If MultiKey( SC_N ) Then EstadoLineaTeclado ( &h05 ) = &hBF
	If MultiKey( SC_M ) Then EstadoLineaTeclado ( &h04 ) = &hBF	
	
	' numeros
	If MultiKey( SC_0 ) Then EstadoLineaTeclado ( &h04 ) = &hFE
	If MultiKey( SC_1 ) Then EstadoLineaTeclado ( &h08 ) = &hFE
	If MultiKey( SC_2 ) Then EstadoLineaTeclado ( &h08 ) = &hFD
	If MultiKey( SC_3 ) Then EstadoLineaTeclado ( &h07 ) = &hFD
	If MultiKey( SC_4 ) Then EstadoLineaTeclado ( &h07 ) = &hFE
	If MultiKey( SC_5 ) Then EstadoLineaTeclado ( &h06 ) = &hFD
	If MultiKey( SC_6 ) Then EstadoLineaTeclado ( &h06 ) = &hFE
	If MultiKey( SC_7 ) Then EstadoLineaTeclado ( &h05 ) = &hFD
	If MultiKey( SC_8 ) Then EstadoLineaTeclado ( &h05 ) = &hFE
	If MultiKey( SC_9 ) Then EstadoLineaTeclado ( &h04 ) = &hFD
	                                           
	' cursores
	If MultiKey(SC_LEFT) then EstadoLineaTeclado(1)=&hfe
	If MultiKey(SC_UP)   then EstadoLineaTeclado(0)=&hfe
	If MultiKey(SC_RIGHT)then EstadoLineaTeclado(0)=&hfd
	If MultiKey(SC_DOWN) then EstadoLineaTeclado(0)=&hfb

   ' tecla de comandos RSX especiales del AMSTRAD, la barra vertical "|"
   ' mapeada en la "\" del PC. se obtiene con SHIFT a la vez, mientras que sin SHIFT es arroba @
	If MultiKey( SC_TILDE ) Then EstadoLineaTeclado ( &h03 ) = &hFB
	
	If MultiKey( SC_LEFTBRACKET ) Then EstadoLineaTeclado ( &h02 ) = &hFD
	If MultiKey( SC_ALT ) Then EstadoLineaTeclado ( &h01 ) = &hFD ' tecla COPIAR

	' teclado de funcion
	If MultiKey( SC_F10) Then EstadoLineaTeclado ( &h01 ) = &h7F ' es la F0
	If MultiKey( SC_F1 ) Then EstadoLineaTeclado ( &h01 ) = &hDF
	If MultiKey( SC_F2 ) Then EstadoLineaTeclado ( &h01 ) = &hBF
	If MultiKey( SC_F3 ) Then EstadoLineaTeclado ( &h00 ) = &hDF
	If MultiKey( SC_F4 ) Then EstadoLineaTeclado ( &h02 ) = &hEF
	If MultiKey( SC_F5 ) Then EstadoLineaTeclado ( &h01 ) = &hEF
	If MultiKey( SC_F6 ) Then EstadoLineaTeclado ( &h00 ) = &hEF
	If MultiKey( SC_F7 ) Then EstadoLineaTeclado ( &h01 ) = &hFB
	If MultiKey( SC_F8 ) Then EstadoLineaTeclado ( &h01 ) = &hF7
	If MultiKey( SC_F9 ) Then EstadoLineaTeclado ( &h00 ) = &hF7





    
   ' para poder hacer combinaciones de teclas especiales, debo hacer una verificacion
   ' de la tecla SHIFT para evitar que se quede pulsada si no hay mas teclas con ella
   If MultiKey(SC_LSHIFT)   Then
   	Dim f As Integer
   	'For f=0 To 15:Print EstadoLineaTeclado(f):next 
   	EstadoLineaTeclado(2)=&hdf
   	' algunas teclas debo mirarlas por separado, como el 0 y el 7 (= y /)
   	If MultiKey(SC_0) Then GoTo aa
   	If MultiKey(SC_7) Then GoTo aa
	   For f=0 To 1
	   	If EstadoLineaTeclado(f)<>&hff Then Exit Sub
	   Next
	   For f=3 To 15
	   	If EstadoLineaTeclado(f)<>&hff Then Exit Sub
	   Next	
aa:
	   inicia_teclado()
   End If


	' combinaciones por motivos de adaptacion al español
   If MultiKey( SC_LSHIFT ) Then ' con SHIFT IZQUIERDA
		if MultiKey( SC_RIGHTBRACKET ) Then 
			EstadoLineaTeclado ( &h02 ) = &hDF ' LSHIFT PULSADO
			EstadoLineaTeclado ( &h03 ) = &hDF ' asterisco *
			Exit sub
		EndIf
		If MultiKey( SC_COMMA ) Then 
			EstadoLineaTeclado ( &h02 ) = &hFF ' LSHIFT NO PULSADO
			EstadoLineaTeclado ( &h03 ) = &hEF ' punto y coma ;
			Exit Sub
		EndIf
	   if MultiKey( SC_PERIOD ) Then 
	     	EstadoLineaTeclado ( &h02 ) = &hFF ' LSHIFT NO PULSADO
	     	EstadoLineaTeclado ( &h03 ) = &hDF ' dos puntos :
	     	Exit sub
	   EndIf
		If MultiKey( SC_0 ) Then 
		  	EstadoLineaTeclado ( &h02 ) = &hDF ' LSHIFT NO PULSADO
		  	EstadoLineaTeclado ( &h03 ) = &hFD ' igual =
		  	Exit sub
		EndIf
		If MultiKey( SC_SLASH ) Then 
		  	EstadoLineaTeclado ( &h02 ) = &hDF ' LSHIFT PULSADO
		  	EstadoLineaTeclado ( &h04 ) = &hFE ' subrayado _
		  	Exit sub
		EndIf
      If MultiKey( SC_QUOTE ) Then ' tecla PC español "{"
		  	EstadoLineaTeclado ( &h02 ) = &hDF ' LSHIFT PULSADO
		  	EstadoLineaTeclado ( &h04 ) = &h7F ' mayor que >
		  	Exit Sub
      End If
      If MultiKey( SC_BACKSLASH ) Then ' tecla PC español "}"
		  	EstadoLineaTeclado ( &h02 ) = &hDF ' LSHIFT PULSADO
		  	EstadoLineaTeclado ( &h03 ) = &h7F ' menor que <
		  	Exit sub
      End If
      If MultiKey( SC_7 ) Then 
		  	EstadoLineaTeclado ( &h02 ) = &hFF ' LSHIFT NO PULSADO
			EstadoLineaTeclado ( &h03 ) = &hBF ' barra /
			Exit Sub
      EndIf
      If MultiKey( SC_MINUS ) Then 
		  	EstadoLineaTeclado ( &h02 ) = &hDF ' LSHIFT PULSADO
			EstadoLineaTeclado ( &h03 ) = &hBF ' tilde '
			Exit Sub
      EndIf
    
    
   ElseIf MultiKey ( SC_RSHIFT ) Then ' caso especial con SHIFT DERECHA
   	If MultiKey( SC_RIGHTBRACKET ) Then 
   		EstadoLineaTeclado ( &h02 ) = &hFF ' LSHIFT NO PULSADO 
   		EstadoLineaTeclado ( &h02 ) = &hF7 ' corchete ]
   		Exit sub
   	EndIf
   	
   	
   Else ' sin SHIFT
		If MultiKey( SC_RIGHTBRACKET ) Then 
			EstadoLineaTeclado ( &h02 ) = &hDF ' LSHIFT PULSADO
			EstadoLineaTeclado ( &h03 ) = &hEF ' mas +
			Exit Sub
		EndIf
		If MultiKey( SC_PERIOD ) Then 
		  	EstadoLineaTeclado ( &h02 ) = &hFF ' LSHIFT NO PULSADO
		  	EstadoLineaTeclado ( &h03 ) = &h7F ' punto .
		  	Exit sub
		EndIf
		If MultiKey( SC_SLASH ) Then 
		  	EstadoLineaTeclado ( &h02 ) = &hFF ' LSHIFT NO PULSADO
		  	EstadoLineaTeclado ( &h03 ) = &hFD ' igual =
		  	Exit sub
		EndIf
		If MultiKey( SC_QUOTE ) Then 
		  	' el LSHIFT PULSADO aqui debemos hacerlo con XOR 32 por que usa la misma linea
		  	EstadoLineaTeclado ( &h02 ) = &hFD xor 32 ' llave {
		  	Exit sub
		EndIf
      If MultiKey( SC_BACKSLASH ) Then 
		  	' el LSHIFT PULSADO aqui debemos hacerlo con XOR 32 por que usa la misma linea
		  	EstadoLineaTeclado ( &h02 ) = &hF7 xor 32 ' llave }
		  	Exit Sub
      End If
		If MultiKey( SC_COMMA ) Then 
		  	EstadoLineaTeclado ( &h02 ) = &hFF ' LSHIFT NO PULSADO
			EstadoLineaTeclado ( &h04 ) = &h7F ' coma ,
			Exit Sub
		EndIf
      If MultiKey( SC_MINUS ) Then 
		  	EstadoLineaTeclado ( &h02 ) = &hDF ' LSHIFT PULSADO
			EstadoLineaTeclado ( &h05 ) = &hFD ' tilde '
			Exit Sub
      EndIf
		
   End If

End Sub

Sub drawLine(lineno As Integer)
    
   ' soy tan vago, que prefiero pintar todo de golpe, sabiendo que no refresca bien la imagen....

End Sub

Sub ponpantalla()

    DIM a as integer, b as Integer, c  As Integer, d as Integer
    Dim e As integer, f As integer, g  As integer, h as Integer, k As Integer
    Dim x As Integer, y As integer, xx As Integer,yy As Integer
    Dim As Integer x2,x3,y2,y3     
    Dim kk As Integer
    
    Dim modo As integer
    
    modo=lastMode ' modo de pantalla
    
    If modo=0 Then
     x=0:y=0
     xx=0:yy=0
     k=0
     kk=0
     h=0
     For f=0 To 16383-384
     	  a=ram(k+TabPOKE(3)):k+=1
     	  g=((a And 128) Shr 4)+((a And 32) Shr 3)+((a And 8) Shr 2)+((a And 2) Shr 1)
     	  Line (x+0,y+0)-Step(4,1),CPC_Colores_modo0(g),bf
     	  'PSet (x+0,y+0),CPC_Colores_modo0(g)
     	  'PSet (x+1,y+0),CPC_Colores_modo0(g)
     	  'PSet (x+2,y+0),CPC_Colores_modo0(g)
     	  'PSet (x+3,y+0),CPC_Colores_modo0(g)
     	  'PSet (x+0,y+1),CPC_Colores_modo0(g)
     	  'PSet (x+1,y+1),CPC_Colores_modo0(g)
     	  'PSet (x+2,y+1),CPC_Colores_modo0(g)
     	  'PSet (x+3,y+1),CPC_Colores_modo0(g)
     	  g=((a And 64) Shr 3)+((a And 16) Shr 2)+((a And 4) Shr 1)+(a And 1)
     	  Line (x+4,y+0)-Step(4,1),CPC_Colores_modo0(g),bf
     	  'PSet (x+4,y+0),CPC_Colores_modo0(g)
     	  'PSet (x+5,y+0),CPC_Colores_modo0(g)
     	  'PSet (x+6,y+0),CPC_Colores_modo0(g)
     	  'PSet (x+7,y+0),CPC_Colores_modo0(g)
     	  'PSet (x+4,y+1),CPC_Colores_modo0(g)
     	  'PSet (x+5,y+1),CPC_Colores_modo0(g)
     	  'PSet (x+6,y+1),CPC_Colores_modo0(g)
     	  'PSet (x+7,y+1),CPC_Colores_modo0(g)
	     x+=8
	     h+=1
	     If h=80 Then h=0:x=0:y+=16:If y>399 Then yy+=2:y=yy:kk+=2048:k=kk
     Next
    End If
        
    If modo=1 Then
     x=0:y=0
     xx=0:yy=0
     k=0
     kk=0
     h=0
     For f=0 To 16383-384
     	  a=ram(k+TabPOKE(3)):k+=1
     	  b=a Shr 4
     	  c=a And 15	  
     	  d=8
     	  g=((b And d)+(c And d)) Shr 3
     	  Line (x+0,y)-Step(2,1),CPC_Colores_modo1(g),bf
     	  'PSet (x+0,y+0),CPC_Colores_modo1(g)
     	  'PSet (x+1,y+0),CPC_Colores_modo1(g)
     	  'PSet (x+0,y+1),CPC_Colores_modo1(g)
     	  'PSet (x+1,y+1),CPC_Colores_modo1(g)
     	  d=4
     	  g=((b And d)+(c And d)) Shr 2
     	  Line (x+2,y)-Step(2,1),CPC_Colores_modo1(g),bf
     	  'PSet (x+2,y+0),CPC_Colores_modo1(g)
     	  'PSet (x+3,y+0),CPC_Colores_modo1(g)
     	  'PSet (x+2,y+1),CPC_Colores_modo1(g)
     	  'PSet (x+3,y+1),CPC_Colores_modo1(g)
     	  d=2
     	  g=((b And d)+(c And d)) Shr 1
     	  Line (x+4,y)-Step(2,1),CPC_Colores_modo1(g),bf
     	  'PSet (x+4,y+0),CPC_Colores_modo1(g)
     	  'PSet (x+5,y+0),CPC_Colores_modo1(g)
     	  'PSet (x+4,y+1),CPC_Colores_modo1(g)
     	  'PSet (x+5,y+1),CPC_Colores_modo1(g)
     	  d=1
     	  g=((b And d)+(c And d))
     	  Line (x+6,y)-Step(2,1),CPC_Colores_modo1(g),bf
     	  'PSet (x+6,y+0),CPC_Colores_modo1(g)
     	  'PSet (x+7,y+0),CPC_Colores_modo1(g)
     	  'PSet (x+6,y+1),CPC_Colores_modo1(g)
     	  'PSet (x+7,y+1),CPC_Colores_modo1(g)
	     x+=8
	     h+=1
	     If h=80 Then h=0:x=0:y+=16:If y>399 Then yy+=2:y=yy:kk+=2048:k=kk
     Next
    End If
    
    If modo=2 Then
     x=0:y=0
     xx=0:yy=0
     k=0
     kk=0
     h=0
     'Locate 6,50:Print Hex(Tabpoke(3))
     If MultiKey(SC_F10) Then kkk=&h4000' borrar el KKK de variables tambien
     If MultiKey(SC_F11) Then kkk=&hc000
     For f=0 To 16383-384
     	  a=ram(k+kkk)'TabPOKE(3))
     	  k+=1
        For g=7 To 0 Step -1
     	   Line (x+(7-g),y)-Step(1,1),CPC_Colores_modo2((a Shr g) And 1),bf
        	'PSet (x+(7-g),y),CPC_Colores_modo2((a Shr g) And 1)
        Next
	     x+=8
	     h+=1
	     If h=80 Then h=0:x=0:y+=16:If y>399 Then yy+=2:y=yy:kk+=2048:k=kk
     Next 
    EndIf
     
'Locate 29,1:Print "pos vram:",ram(&hb726),ram(&hb727),ram(&hb72d)

    ScreenCopy 1,0
End Sub

Sub WritePPI(puerto As Integer, valor As Integer)

	' el 8255 solo tiene tres puertos
	' el "0" es el de entrada/salida principal
	' el 1 esta dedicado en exclusiva a identificar el modelo de CPC, por lo que solo es de lectura 
    Select Case( ( puerto shr 8 ) and 3 )
    	  Case 0
            TablaPPI( 0 ) = Valor
        case 2 
            TablaPPI( 2 ) = Valor
            ModoPSG = valor and &hC0
            LineaTeclado = valor and 15
            Select case( ModoPSG shr 6 )
            	case 1
                    TablaPSG( 14 ) = EstadoLineaTeclado( LineaTeclado )
                    TablaPPI(  0 ) = TablaPSG( NumReg )
            	Case 2
                    TablaPSG( NumReg ) = TablaPPI( 0 )
               case 3 
                    NumReg = TablaPPI( 0 )
            End Select
        case 3 
            TablaPPI( 3 ) = Valor
            if ( ( valor and &h80 )=0 ) then
                Dim As integer BitDeEstado = valor and 1
                valor = valor shr 1
                if ( BitDeEstado ) Then
                    TablaPPI( 2 ) = TablaPPI( 2 ) or ( 1 shl valor )
                else
                    TablaPPI( 2 ) = TablaPPI( 2 ) and (( 1 shl valor )*-1-1)
                End If
                WritePPI( &hF600, TablaPPI( 2 ) ) ' puerto 2 del PPI (F6=puerto 2)
            End If
    End Select
    	
End Sub

Function ReadPPI( port As Integer) As Integer
	Dim puerto As Integer
	Dim valor As Integer
	valor=&hff
	
	' el 8255 tiene tres puertos A,B,C
	' el "0" es el de entrada/salida principal
	' el "1" es fijo, cableado en placa, para identificar el modelo de CPC
	puerto=( port shr 8 ) and 3
	
	' puerto 0 
	If puerto=0 Then valor=TablaPPI(0)':Print "porfin en puerto 0":ScreenCopy:sleep
	
	' puerto 1 deteccion fisica de modelo
	If puerto=1 Then valor=&b01011110+vsync ' identifica el sistema AMSTRAD (cableado en placa en el 8255)
		
	' puerto 2 
	If puerto=2 Then valor=TablaPPI(2)

	'valor=Int(Rnd(1)*255) ' pruebas aleatorias, quitar al acabar de depurar
	Return valor
End Function

' control de puertos I/O
Function inb(port As integer) as Integer
    Dim valor As Integer
    valor=255

    ' si no devolvemos un &hFF(255) en el puerto de la disquetera (&H0480) el NO se muestra el READY
    if ( port And &h0480 )=0 Then valor=ReadFDC( port ) ' disquetera (255=NO EXISTE)
    if ( port And &h0800 )=0 then valor=ReadPPI( port )' chip 8255 I/O

	 'valor=Int(Rnd(1)*255) ' pruebas aleatorias, quitar al acabar de depurar
    Return valor
    
End Function

Sub outb(port As integer, outbyte As Integer)

    outbyte And=&hFF 
    if ( port And &h8000 )=0 Then WriteVGA ( outbyte )
    if ( port And &h4000 )=0 Then CRTC6845 ( port, outbyte )
    If ( port And &h2000 )=0 Then WriteROM ( outbyte )
    If ( port And &h0800 )=0 Then WritePPI ( port, outbyte ) ' chip 8255 I/O
    If ( port And &h0480 )=0 Then WriteFDC ( port, outbyte ) ' disquetera
    
End Sub
