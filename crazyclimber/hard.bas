' rutinas de emulacion hardware del Spectrum 48k unicamente

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
    intIFF1 = False
    intIFF2 = False
    intIM = 0

    InitParity
    Divisores
    
End Sub

Sub resetKeyboard()
    do : loop while inkey$<>""
End Sub

function LeeTeclado() As Integer
    Dim tecla As Integer
    tecla=0

    ' cada vez que entramos, borramos las teclas
    resetKeyboard
    
    ' escape
    if MultiKey(SC_ESCAPE)  Then tecla=27: Close:End

    ' controles
    if MultiKey(SC_RSHIFT)  then tecla=1
    if MultiKey(SC_CONTROL) then tecla=1
    if MultiKey(SC_ENTER)   Then tecla=1
    if MultiKey(SC_SPACE)   then tecla=1

    ' letras
    if MultiKey(SC_A) then tecla=65
    if MultiKey(SC_B) then tecla=1
    if MultiKey(SC_C) then tecla=2
    if MultiKey(SC_D) then tecla=3
    if MultiKey(SC_E) then tecla=4
    if MultiKey(SC_F) then tecla=5
    if MultiKey(SC_G) then tecla=6
    if MultiKey(SC_H) then tecla=7
    if MultiKey(SC_I) then tecla=1
    if MultiKey(SC_J) then tecla=1
    if MultiKey(SC_K) then tecla=1
    if MultiKey(SC_L) then tecla=1
    if MultiKey(SC_M) then tecla=1
    if MultiKey(SC_N) then tecla=1
    if MultiKey(SC_O) then tecla=1
    if MultiKey(SC_P) then tecla=1
    if MultiKey(SC_Q) then tecla=1
    if MultiKey(SC_R) then tecla=1
    if MultiKey(SC_S) then tecla=1
    if MultiKey(SC_T) then tecla=1
    if MultiKey(SC_U) then tecla=1
    if MultiKey(SC_V) then tecla=1
    if MultiKey(SC_W) then tecla=1
    if MultiKey(SC_X) then tecla=1
    if MultiKey(SC_Y) then tecla=1
    if MultiKey(SC_Z) then tecla=1
  
    ' numeros
    if MultiKey(SC_0) then tecla=1
    if MultiKey(SC_1) then tecla=1
    if MultiKey(SC_2) then tecla=1
    if MultiKey(SC_3) then tecla=1
    if MultiKey(SC_4) then tecla=1
    if MultiKey(SC_5) then tecla=1
    if MultiKey(SC_6) then tecla=1
    if MultiKey(SC_7) then tecla=1
    if MultiKey(SC_8) then tecla=1
    if MultiKey(SC_9) then tecla=1
                                                 
    ' cursores
    If MultiKey(SC_LEFT) Then tecla=1 
    If MultiKey(SC_UP)   Then tecla=1
    If MultiKey(SC_RIGHT)Then tecla=1
    If MultiKey(SC_DOWN) Then tecla=1

   Return tecla
End function


sub sprites()

	dim inigraf as integer
	dim as integer a,b,c,d,e,f,g,h,i,j
	dim as integer simx, simy
	dim as integer x,y,ff,cc,hh,ii	' ff caracter, cc color
	
	'  sprites van de 9880 a 989f, 8 grupos de 4 bytes=32bytes
	'  9880h-989fh ;Sprite controls. 8 groups of 4 bytes:
	'  1st byte; code/attribute.
	'            Bits 0-5: sprite code.
	'            Bit    6: x invert.
	'            Bit    7: y invert.
	'  2nd byte ;color.
	'            Bits 0-3: colour. (palette scheme 0-15)
	'            Bit    4: 0=charset1, 1 =charset 2.
	'  3rd byte ;y position 
	'  4th byte ;x position

	for j=0 to 31 step 4' 31 bytes para sprites, 8 de golpe * 4bytes
			h=0 ' contadores de columnas y filas de pixeles dibujados, para hacer 16x16
			i=0
			inigraf=&h9880+j ' en CCLIMBER al menos
			
			' 1 de 4
			ff=ram(inigraf+0) and &b00111111 ' numero de sprite de 64 posibles
			simx=iif (ram(inigraf+0) and &b01000000,1,0) ' simx
			simy=iif (ram(inigraf+0) and &b10000000,1,0) ' simy (parece que  el cclimber no lo usa)
			
			' 2 de 4
			cc=ram(inigraf+1)
		   e=(cc and &b00010000) shr 4 ' bit 4 banco de caracteres: 0 o 1 no hay mas (tile0 o tile1)
		   cc=cc and 15 ' los primeros 4 (xxxx1111) bits son el color de 0 a 15
			
			' 3 de 4
			y=256-ram(inigraf+2) ' la y va invertida!!!!
			
			' 4 de 4
			x=ram(inigraf+3)+400 ' los 400 es cosa mia, para centraje en pantalla solo
		
		  
		 ii=0:hh=0:h=0
		   'inicio MIO en la 16384 (ojo) de los graficos sprites en MI LECTURA de ROM
		   ' mas el banco de SPRITE segun sea 0 o 1 
			inigraf=&h000+(ff*32)+(e*&h1000)
			for f=0 to 31
				a=graf(inigraf+f)
				b=graf(inigraf+f+&h2000)
				d=128
				for g=0 to 7
					c=(iif((a and d),1,0)*2)+ iif((b and d),1,0)
					d=d shr 1
					c=c*cc ' color obtenido de 2bits, por color de 4 bits CC=16 colores, que viene de la lectura de pantalla
					if c<>0 then 
						if simx then
							pset (x+(15-(h+g)),y+i),c ' SIMX
						end if
						if simy then
							print "sim y no probada":screencopy:sleep:pset (x+h,y+(16-i)),c ' no probado aun, por que cclimber no tiene en Y
						end if
						if simx+simy=0 then
							pset (x+h+g,y+i),c ' SIN SIM
						end if
					endif
					'h+=1
				next
				i+=1
				if f= 7 then h=8:i=0
				if f=15 then h=0:i=8
				if f=23 then h=8:i=8
			next
	next
	
	
	''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	
	
	
	' ademas, UN SOLO BIG SPRITE.... (por ejemplo, el logo, el pajarraco, el KINGKONG, etc.
	' 98ddh ;Bigsprite colour/attribute.
	'            Bit 0-2: Big sprite colour.
	'            Bit   4: x invert.
	'            Bit   5: y invert. (not used by CC)
	' 98deh ;Bigsprite y position.
	' 98dfh ;Bigsprite x position.
	
	dim as integer xbig,ybig
	dim as integer bigcaracter ' hh e ii son cada casilla de 16x16 del big
	
	' direccion de inicio de parametros de bigsprite
	inigraf=&h98dd ' en CCLIMBER al menos
	
	' 1 de 3
	cc=ram(inigraf+0) ' color y simetrias
	simx=iif (ram(inigraf+0) and &b00001000,1,0) ' simx
	simy=iif (ram(inigraf+0) and &b00010000,1,0) ' simy
   cc=cc and 7 ' los primeros 3 (xxxxx111) bits son el color de 0 a 7
	
	' 2 de 3
	ybig=128-ram(inigraf+1) ' la y va invertida!!!! 128 pixeles?? porque....
	'locate 1,1:print ram(inigraf+1) ,ram(inigraf+2) 
	
	' 3 de 3
	xbig=(136-ram(inigraf+2))+400 ' los 400 es cosa mia, para centraje en pantalla solo
	' el logo nichibutsu del hotel marquesina necesita 32 pixeles de mas en x!!!!!

   'big sprite, complicado:
   ' parece que es de 16*16 caracteres(128X128 pixeles)
   ' y lo dibuja en la ram primero como caracteres, entre la 8800 y 88ff (256 bytes)
   ' organizado como 16 columnas, por 16 filas
   ' leemos cada caracter, y con el cogemos el grafico desde la zona del GRAF2 (en mi caso, desde la 16384)
   ' y lo vamos dibujando (sus 128x128 pixels) en la direccion cogida de las dir 98dd (ver arriba)

	hh=0:ii=0:i=0
	for j=0 to 255
		for f=0 to 7
			bigcaracter=ram(&h8800+j)
			inigraf=&h4000+(bigcaracter*8)
			a=graf(inigraf+f)
			b=graf(inigraf+f+&h0800) ' 2048 bytes cada plano de color (4096 para cada rom)
			d=128
			for g=0 to 7
				c=(iif((a and d),1,0)*2)+ iif((b and d),1,0)
				d=d shr 1
				c=c*cc ' color obtenido de 2bits, por color de 4 bits CC=16 colores, que viene de la lectura de pantalla
				if c<>0 then 
						if simx then
							pset (xbig+(127-(g+hh)),ybig+i+ii),c ' SIMX
						end if
						if simy then
							print "sim y no probada":screencopy:sleep ' no probado aun, por que cclimber no tiene en Y
						end if
						if simx+simy=0 then
							pset (xbig+g+hh,ybig+i+ii),c ' SIN SIM
						end if
				endif
			next
			i+=1
			' menudo cristo, en cada casilla de 16x16 ponemos 256 pixeles (8x8)
			' incrementamos horizontalamnte 8 pixeles, y al llegar a 128, bajamos 8 pixeles a la siguiente fila
			if i=8 then i=0:hh+=8:if hh>127 then hh=0:ii+=8
		next
	next

	
	
end sub


sub pongraf(x as integer, y as integer, ff as integer, cc as integer)
	' ff caracter, cc color
	dim inigraf as integer
	dim as integer a,b,c,d,e,f,g,h,i
	dim as integer simx, simy
	
	x=(x-1)*8
	y=(y-1)*8

   
   ' analizamos el datos de color
   ' el bit 7 (128) es invert Y
   ' el bit 6 (64) es invert X
   ' el bit 5 (32)  no se usa
   ' el bit 4 (16) es  0=charset1, 1=charset2.
   
   ' primero, banco grafico
   e=(cc and &b00010000) shr 4 ' bit 4 banco de caracteres: 0 o 1 no hay mas (tile0 o tile1)
   ' segundo, color
   cc=cc and 15 ' los primeros 4 (xxxx1111) bits son el color de 0 a 15
	' tercero, simetria X
	' cuarto, simetria Y (no usada en cclimber, pero si en ckong????)


   
	' caracteres de 8x8 pixeles, 2 bits de color por pixel (4 colores)
	' eso significa 8 bytes por caracter, seguidos, del 0 al 7
	' los bits van de 0 a 7 (primer byte), luego, bajamos fial, segundo byte, etc hasta el 8
	' osea, la forma mas natural de pixelado.
	inigraf=(ff*8)+(e*&h1000)' 8 bytes cada caracter, y seguido, el banco de 4k: 0 o 1
	for f=0 to 7
		a=graf(inigraf+f)
		b=graf(inigraf+f+&h2000) ' 8192 bytes cada plano de color, peeeero.... 
		                         ' teniendo en cuenta, que llevan 2k huecos cada 2k, o sea
		                         ' que en realidad, son 4096 de un plano a otro (ver carga-roms para direcciones)
		d=128
		for g=0 to 7
			c=(iif((a and d),1,0)*2)+ iif((b and d),1,0)
			d=d shr 1
			c=c*cc ' color obtenido de 2bits, por color de 4 bits CC=16 colores, que viene de la lectura de pantalla
			pset (y+g,x+f),c
		next
	next
	
end sub


Sub ponpantalla()

    DIM I as integer, m as Integer, a as ubyte, b as ubyte
    Dim X As integer, y As integer,f as integer, h as integer,g as integer
    dim FF AS integer, GG as integer


 ' la pantalla es de 32x32 = 1024 bytes de la &h9000 a &h93ff &h400=1024
 ' los colores, van en una zona MUY diferente, en concreto, de &h9C00 a &h9FFF, tambien 1024
 ' 256×256 pincheles, 96 colores?? (32x32 caracteres)
 ' el alto VISIBLE es de 30 filas (240 pincheles), por que las filas 0 y 31 son para el scroll que dibuja en oculto.
 ' al mostrarse solo de la 1 a la 30, no se nota el dibujado brusco en las filas 0 y 31
 
 'exit sub
'goto nosalir
cls


    x=0:y=0
    For i = 0 To 31'(32*32)-1 step 32
    	for f= 0 to 31
    		' cada fila son 32 caracteres, mas 32 de colores en la dir VERRAM+&H0C00
           gg = RAM(verram+(i*32)+f+(&h0c00))'colores y bancos graficos DIR &H9C00
           ff = RAM(verram+(i*32)+f)' caracter
           
           ' para modo texto
           'if ff<32 then ff=ff+64
           'print #1, bin(gg,8)
           'color gg:locate y+1,x+1:print chr(ff); 
           
           ' para modo grafico
           pongraf(y+1,x+1,ff,gg)
           x=x+1:if x=32 then x=0:y=y+1
    	next f
    Next i
    
    
         
     
         
    ' seguido, sprites, en medio de los dos, antes del scroll
    'sprites()
    
    
    
    
     ' "scroll" por columnas. se muestra en pantalla rotando el valor indicado
     ' ejemplo, si es 128, se muestra de 128 a 255 y seguido de 0 a 127
     ' si el valor es 2, se muestra de 2 a 255 y seguido de 0 a 1, de modo que siempre se muestran 256
     ' pero la primera y ultima fila de 16 pixeles (32 al total) no se ven solo son visibles 228 pincheles
     ' asi no se aprecia como se dibuja arriba o abajo
     
     h=&h9800
     for f=0 to 31
     	b=RAM(h+f) ' cogemos el valor a desplazar (0 es sin nada, o sea, se coge todo sin movimiento)
     	i=0' contador fijo de 0 a 255 para dibujar en pantalla lo que se desplaza
     	' primer grupo, desde el valor indicado hasta el final (255)
     	for g=b to 255
     		for m=0 to 7
     		 a=point(f*8+m,g)
     		 'pset(f*8+m,g),25 'cls
     		 ' solo se muetran las filas entre la 2 y la 30 (2 filas por arriba y dos por abajo no se ven)
     		 ' en pincheles son 16 por arriba y 16 por abajo
     		 if i>15 and i<240 then pset (f*8+400+m,i),a
     		next
     		i+=1
     	next
     	' segundo grupo, desde el inicio (0) hasta el valor-1
     	for g=0 to b-1
     		for m=0 to 7
     		 a=point(f*8+m,g)
     		 'pset(f*8+m,g),25 'cls
     		 ' solo se muetran las filas entre la 2 y la 30
     		 if i>15 and i<240 then pset (f*8+400+m,i),a
     		next
     		i+=1
     	next
     next

sprites()
    
     ' vemos en modo hexadecimal la salida de pantalla
goto salir
nosalir:
verram=&h9880
    If MultiKey(SC_PAGEDOWN) then verram+=40
    if MultiKey(SC_PAGEUP) then verram-=40
    if multikey(SC_SPACE) then sleep
    ' ponemos una zona de RAM primero en HEXA
    ' la pantalla es de 32x32=1024 bytes de la &h9000 a &h93ff &h400=1024
    ' los colres, van en una zona MUY diferente, en concreto, de &h9C00 a &h9FFF, tambien 1024
    FF=verram'+(32*32)
    For i=0 To 16
    	For m=0 To 47 Step 3 '96 caracteres, entre 3=32 bytes de ancho de pantalla
    		Locate i+1,m+1
    		If ff<&h10000 Then Print Hex(RAM(FF),2):FF+=1 Else Print "#"
    	Next
    Next
    FF=verram
    ' y luego en ASCII
    For i=0 To 9
    	For m=0 To 95
    		Locate i+17,m+1
    		a=RAM(FF)
    		'If ff<&h10000 Then Print IIf(a<32 Or a=255,".",Chr(a)):FF+=1 Else Print "#"
    	Next
    Next
    Locate 29,33:Print "Estamos viendo en la:";Hex(verram,4)

    'sleep
salir:


    ' una vez dibujada oculta, la volcamos a la visible
    ScreenCopy
End Sub


' control de puertos I/O
Function inb(port As integer) as Integer

  Print #1,"   inp:";Hex(port,2)
    If (port And &HFF) = &H92 Then
      'fC=0
      'regAF_ and=(F_C Xor 255)
      ' quizas, las dir &hff2c hasta 40 bytes, sean buffer pantalla 20+20c.
      'For f As integer=0 To 40:ram(&hff2c+f)=Int(Rnd(1)*5):next
		'Return LeeTeclado()
    End If
    'fC=-1
    Return 255
    
End Function

Sub outb(port As integer, outbyte As Integer)
	
  Print #1,"out:";Hex(port,2);" - ";Hex(outbyte,2)
    If (port And 1) = 0 Then
   
    End If
    
End Sub






''''''''''''''''''''''''''''''''''''
'''''''  MANEJO DE LAS IRQ '''''''''
''''''''''''''''''''''''''''''''''''
Function interrupt() As Integer
	Dim lcounter as integer ' tiempo a dormir el PC dentro de INTERRUPT para frenarlo a la velocidad Z80 real
	Dim lsleep as integer ' recoge el valor del TIMER(), tiempo trascurrido entre dos llamadas a INTERRUPT
   Dim tecla As Integer
     
   interruptCounter = interruptCounter + 1  
   If (interruptCounter Mod 16)=0 Then
        ' ejecuciones cada 16 veces (1/2 segundo) de cada IRQ
        ' para elementos que no requieren tanta actualizacion
        ' como un parpadeo en pantalla de un caracter, por ejemplo
        ' o la lectura de un teclado
   End If

    ' If it's a maskable interrupt
    If intIFF1 = False Then
        interrupt = 0
    Else
        Select Case intIM
        Case 0, 1
            pushpc
            intIFF1 = False
            intIFF2 = False
            regPC = 56
            interrupt = 13
        Case 2
            pushpc
            intIFF1 = False
            intIFF2 = False
            regPC = peekw((intI * 256) Or &HFF)
            interrupt = 19
        End Select
    End If
   
   ' miramos el tiempo transcurrido entre dos IRQ
    lSleep = int(Timer()*1000) - TiempoReal
    
    If lSleep = 0 Then lSleep = 1
    
    If lSleep < InterruptDelay + 1 Then     
        lCounter = InterruptDelay - lSleep - DelayOverage
        If lCounter < 0 Then
            DelayOverage = -lCounter
        Else
            DelayOverage = DelayOverage - ((InterruptDelay - lSleep) - lCounter)
            ' este sleep causa una pausa superalta y detiene el emul. estudiarlo
            ' EL SEGUNDO PARAMETRO DE SLEEP, IMPIDE QUE SE INTERRUMPA CON UNA TECLA
            If VelocidadReal then Sleep lCounter,1
        End If
    Else
        DelayOverage = DelayOverage + lSleep - InterruptDelay
        'Locate 20,20:Print delayoverage,InterruptDelay
        If DelayOverage > 48 Then DelayOverage = 48
    End If

    TiempoReal = int(Timer()*1000)
    
End Function

