' rutinas de emulacion hardware, en este caso, para un GALAXIAN
Sub inicia_pantalla()

   ' abrimos una de 640x480x32bpp con una copia oculta para dibujar	
	Screen 18,,2
	
	' paleta de colores aproximada del GALAXIAN		
	Palette 0,0,0,0
	Palette 1,0,0,0
	Palette 2,0,0,0
	Palette 3,224,224,224
	
	Palette 4,0,0,0
	Palette 5,195,61,0
	Palette 6,0,224,0
	Palette 7,224,224,0
	
	Palette 8 ,0,0,0
	Palette 9 ,0,91,217
	Palette 10,224,0,0
	Palette 11,224,224,0
	
	Palette 12,0,0,0
	Palette 13,0,0,224
	Palette 14,133,0,224
	Palette 15,224,0,0
	
	Palette 16,0,0,0
	Palette 17,0,0,224
	Palette 18,0,133,147
	Palette 19,224,0,0
	
	Palette 20,0,0,0
	Palette 21,0,0,0
	Palette 22,0,0,0
	Palette 23,224,0,0
	
	Palette 24,0,0,0
	Palette 25,224,224,224
	Palette 26,224,0,0
	Palette 27,0,133,147
	
	Palette 28,0,0,0
	Palette 29,224,224,0
	Palette 30,224,0,0
	Palette 31,195,0,217

End Sub

Sub resetKeyboard()
	 galaxbutton=0
	 galaxbutton1=0
	 galaxbutton2=0
    Do : loop while inkey$<>""
End Sub

Sub LeeTeclado()

    ' cada vez que entramos, borramos las teclas
    resetKeyboard

    ' escape
    if MultiKey(SC_ESCAPE)  Then end

    ' controles
    if MultiKey(SC_RSHIFT)  then galaxbutton=0
    if MultiKey(SC_CONTROL) then galaxbutton=0
    if MultiKey(SC_ENTER)   Then galaxbutton=0
    if MultiKey(SC_SPACE)   then galaxbutton1 Or=1 Shl 4 ' disparo

    ' letras
    if MultiKey(SC_A) then galaxbutton=0
    if MultiKey(SC_B) then galaxbutton=0
    if MultiKey(SC_C) then galaxbutton=0
    if MultiKey(SC_D) then galaxbutton=0
    if MultiKey(SC_E) then galaxbutton=0
    if MultiKey(SC_F) then galaxbutton=0
    if MultiKey(SC_G) then galaxbutton=0
    if MultiKey(SC_H) then galaxbutton=0
    if MultiKey(SC_I) then galaxbutton=0
    if MultiKey(SC_J) then galaxbutton=0
    if MultiKey(SC_K) then galaxbutton=0
    if MultiKey(SC_L) then galaxbutton=0
    if MultiKey(SC_M) then galaxbutton=0
    if MultiKey(SC_N) then galaxbutton=0
    if MultiKey(SC_O) then galaxbutton1 Or=1 Shl 2 ' izquierda
    if MultiKey(SC_P) then galaxbutton1 Or=1 Shl 3 ' derecha
    if MultiKey(SC_Q) then galaxbutton=0
    if MultiKey(SC_R) then galaxbutton=0
    if MultiKey(SC_S) then galaxbutton=0
    if MultiKey(SC_T) then galaxbutton=0
    if MultiKey(SC_U) then galaxbutton=0
    if MultiKey(SC_V) then galaxbutton=0
    if MultiKey(SC_W) then galaxbutton=0
    if MultiKey(SC_X) then galaxbutton=0
    if MultiKey(SC_Y) then galaxbutton=0
    if MultiKey(SC_Z) then galaxbutton=0
  
    ' numeros
    if MultiKey(SC_0) then galaxbutton=0
    if MultiKey(SC_1) then galaxbutton2 Or=1 Shl 0 ' inicio jugador 1
    if MultiKey(SC_2) then galaxbutton=0
    if MultiKey(SC_3) Then galaxbutton1 Or=1 Shl 0 ' monedas jugador 1 
    if MultiKey(SC_4) Then galaxbutton=0
    if MultiKey(SC_5) then galaxbutton=0
    if MultiKey(SC_6) then galaxbutton=0
    if MultiKey(SC_7) then galaxbutton1 Or=1 Shl 5 ' servicio?
    if MultiKey(SC_8) then galaxbutton1 Or=1 Shl 6 ' reset?
    if MultiKey(SC_9) then galaxbutton1 Or=1 Shl 7 ' modo test?
                                                 
    ' cursores
    If MultiKey(SC_LEFT) Then galaxbutton=0 
    If MultiKey(SC_UP)   Then inidatos-=1
    If MultiKey(SC_RIGHT)Then galaxbutton=0
    If MultiKey(SC_DOWN) Then inidatos+=1
    
    RAM(&h6000)=galaxbutton1 ' posicion de lectura de botones en el GALAXIAN
    RAM(&h6800)=galaxbutton2 ' metemos moneda
End Sub


Sub ponpantalla()

    DIM a as integer, b as Integer, c  As Integer, d as integer
    Dim e As integer, f As integer, g  As integer, h as Integer, k As Integer
    Dim x As Integer, y As integer, xx As Integer,yy As Integer
    Dim As Integer x2,x3,y2,y3 ' auxiliares de bichos
    Dim As Integer bicho, espejo_x,espejo_y ' para manejar objetos

    ' espacio para hacer los desplazamientos de porciones de pantalla (estrellas y nave principal)
    Dim As fb.Image Ptr imagen = ImageCreate(256,8)
    
    'Cls
    Dim VRAM      As Integer =&h5000 ' RAM de video principal
    Dim MVRAM     As Integer =&h5400 ' RAM de video secundaria (espejo o MIRROR)
    Dim ATRIBUTOS As Integer =&h5800 ' atributos de color y desplazamientos????
    Dim BICHOS    As Integer =&h5840 ' espacio de los 64 bichos posibles
    Dim BALAS     As Integer =&h5860 ' espacio de las BALAS (32)
    Dim GROM1     As Integer =&h3000 ' direccion de la ROM 1 de caracteres
    Dim GROM2     As Integer =&h3800 ' direccion de la ROM 2 de caracteres

   ' depuracion solo: ponemos en pantalla datos hexadecimales de una zona de RAM
   Locate 1,1
   k=ATRIBUTOS
   ' nota:INIDATOS es para subir o bajar la zona visible con cursores ARRIBA/ABAJO
   Print "Direccion:";Hex(k+inidatos,4)
   Print "ATRIBUTOS:"
   For f=0 To 15
	  For g=0 To 15
		a=RAM(k+inidatos)
		Print Hex(a,2);" ";
		k+=1
	  Next
	Print
   If k=BICHOS Then Print "BICHOS:"
   If k=BALAS  Then Print "BALAS:"
   Next 
   
   ' mostrar en pantalla la VRAM en forma ASCII (depuracion solo)
   'Locate 1,1
   'For y=0 To 28
   '	k=VRAM+(y+32*31)
   '	For x=0 To 31
   '		a=RAM(k)
   '		a+=32
   '		If a>255 Then a=128
   '		Print Chr(a);
   '		k-=32
   '	Next
   '	Print
   'Next
    
    ' desplazamientos temporales del marco de ventana
          xx=383:yy=200
          Line (xx-1,yy-1)-Step (258,258),2,bf
          x=xx
    
     'ventana grafica general, formada por caracteres que indican un grafico a colocar
     'GALAXIAN: resolucion 256*256 (32*32), pero en ancho, solo visibles 224 (28 filas)  
     For d=&h3FF To 0 Step -1
        y=(d Mod 32)
        x=31-(d \ 32)
        'If RAM(&h7006) Then y=31-y
        'If RAM(&h7007) Then x=31-x
        	 h=RAM(VRAM+d)':If k>42 Then Locate 40,1:Print k    	
        	 'If RAM(&h6800) Then k=y        	
        	 For f=0 To 7
        	 	a=RAM(GROM1+f+(h*8)) ' primer plano de graficos en ROM
        	 	b=RAM(GROM2+f+(h*8)) ' segundo plano de graficos en ROM 
        	 	For g=7 To 0 Step -1
        	 		c=(a Shr g) And 1
        	 		c=(c Shl 1)+((b Shr g) And 1)	
        	 		'If RAM(&h6800) And c=1 Then
        	 		'	rem
        	 		'Else
         	 	If c<>0 Then 
         	 		'Print y,c,RAM(ATRIBUTOS+(2*y)+1),RAM(ATRIBUTOS+(2*y)+1)*4:ScreenCopy:sleep
         	 		PSet (xx+(x*8)+(7-f),yy+(y*8)+(7-g)),c+(RAM(ATRIBUTOS+(2*y)+1)*4)	
         	 	Else
         	 		PSet (xx+(x*8)+(7-f),yy+(y*8)+(7-g)),0 ' fondo siempre negro????
         	 	EndIf
        	 		'EndIf
        	 	Next        	 
        	 Next
     Next 
     
   ' realiza los diferentes desplazamientos de filas en vertical (32)
   ' debemos realizarlos antes de poner los bichos y las balas...
   g=256+8
   For f=63 To 0 Step -2
   	g-=8 'vamos subiendo filas de 31 a 0

      ' el atributo de color, lo empleo por independiente de esta rutina
   	'a=RAM(ATRIBUTOS+f) ' grupo de color de esa fila (0 a 7 de 4 colores cada)
   	b=RAM(ATRIBUTOS+f+1) ' coordenadas del desplazamiento
   	If (b And &h80) Then b=-(256-b) ' ajustamos el centro del desplazamiento

   	Get  (xx,yy+g)-step(255,7),imagen ' copiamos la fila a mover
      Line (xx,yy+g)-Step(255,7),0,bf ' la borramos ahora que esta copiada
   	Put (xx+b,yy+g),imagen ' la volvemos a poner, pero desplazada
      'Line (a+xx,yy+g)-Step(255,15),Int(Rnd(1)*16),bf
   Next
   
   
   ' ponemos los bichejos
   For f=32-4 To 0 Step -4
   	Dim color_bicho As Integer
   	x=RAM(BICHOS+f+0) ' HORIZONTAL
		y=RAM(BICHOS+f+3) ' VERTICAL
		If x+y=0 Then Continue For ' si el bicho esta apagado
		If (x=&hF8 And y=&hF8) Then Continue For ' si el bicho esta apagado

   	y=y+4 ' sumamos 4 a Y para ajustar un desfasse vertical

		color_bicho=RAM(BICHOS+f+2) And &h7 ' COLOR
		bicho=RAM(BICHOS+f+1) ' BICHO
		espejo_x=bicho And &h40
		espejo_y=bicho And &h80
		bicho=bicho And &h3f ' nos quedamos solo con el numero del objeto (0 a 63)

		x2=xx+x
		y2=yy+y
		x=0:y=0
		For e=0 To 3
		  d=pp(e) ' cogemos la posicion de cada cuadrante del bicho a dibujar
		   For g=0 To 7
        	 a=RAM(d+GROM1+g+(bicho*32))
        	 b=RAM(d+GROM2+g+(bicho*32))
        	 	For h=7 To 0 Step -1
        	 		c=(a Shr h) And 1
        	 		c=(c Shl 1)+((b Shr h) And 1)
        	 		If c<>0 Then ' solo dibujamos colores, los "0" son trasparencias (creo)
        	 			c+=(color_bicho*4)
        	 			x3=g+x
        	 			y3=(7-h)+y
        	 			If espejo_y Then x3=(x3 Xor 15)
        	 		   If espejo_x Then y3=(y3 Xor 15)
        	 		   PSet (x2+x3,y2+y3),c
        	 		End If
        	 	Next	
		   Next
		 x=x+8
		 If e=1 Then x=x-16: y=y+8
		Next 
		
   Next ' siguiente bicho

   
   
   
   
   ' las balas no existen como grafico, las genera el GALAXIAN (en este caso nosotros)   
   For f=0 To 31 Step 4
   	Dim color_bala As Integer
   	If f=28 Then color_bala=3 Else color_bala=7 ' la bala 28 es la nuestra, de otro color
		y=250-(RAM(BALAS+f+3))
		x=    (RAM(BALAS+f+1))
		line (x+xx,y+yy)-Step(0,3),color_bala
   Next

    ' una vez dibujada oculta, la volcamos a la visible
    screencopy 1,0
End Sub


' control de puertos I/O
Function inb(port As integer) as Integer


    If (port And &HFF) = &HFF Then

    End If
    
    Return 0
    
End Function

Sub outb(port As integer, outbyte As Integer)

    If (port And 1) = 0 Then
   
    End If
    
End Sub