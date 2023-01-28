
select case galax_version

	case 1: '( galaxian )
	   directorio="galaxian"
		Carga_ROM( "galmidw.u",    &h0000 )
		Carga_ROM( "galmidw.v",    &h0800 )
		Carga_ROM( "galmidw.w",    &h1000 )
		Carga_ROM( "galmidw.y",    &h1800 )
		Carga_ROM( "7l",           &h2000 )
      ' graficos
		Carga_ROM( "1h.bin",       &h0000+rom_graf_ini )
		Carga_ROM( "1k.bin",       &h0800+rom_graf_ini )

	case 2: '( galmidw )
		directorio="galmidw"
		Carga_ROM( "galmidw.u",    &h0000 )
		Carga_ROM( "galmidw.v",    &h0800 )
		Carga_ROM( "galmidw.w",    &h1000 )
		Carga_ROM( "galmidw.y",    &h1800 )
		Carga_ROM( "galmidw.z",    &h2000 )
      ' graficos
		Carga_ROM( "galmidw.1j",   &h0000+rom_graf_ini )
		Carga_ROM( "galmidw.1k",   &h0800+rom_graf_ini )

	case 3: '( superg )
		directorio="superg"
		'Carga_ROM( "superg.u",     &h0000 )
		'Carga_ROM( "superg.v",     &h0800 )
		Carga_ROM( "7f.bin",       &h0000 ) ' mi version lleva una de 4k en lugar de 2 de 2k
		Carga_ROM( "superg.w",     &h1000 )
		Carga_ROM( "superg.y",     &h1800 )
		Carga_ROM( "superg.z",     &h2000 )
      ' graficos
		Carga_ROM( "galmidw.1j",   &h0000+rom_graf_ini )
		Carga_ROM( "galmidw.1k",   &h0800+rom_graf_ini )

	case 4: '( galapx )
		directorio="galapx"
		Carga_ROM( "galx.u",       &h0000 )
		Carga_ROM( "galx.v",       &h0800 )
		Carga_ROM( "galx.w",       &h1000 )
		Carga_ROM( "galx.y",       &h1800 )
		Carga_ROM( "galx.z",       &h2000 )
      ' graficos
		Carga_ROM( "galx.1h",      &h0000+rom_graf_ini )
		Carga_ROM( "galx.1k",      &h0800+rom_graf_ini )

	case 5:'( galap1 )
		directorio="galap1"
		Carga_ROM( "superg.u",     &h0000 )
		Carga_ROM( "superg.v",     &h0800 )
		Carga_ROM( "cp3",          &h1000 )
		Carga_ROM( "galx_1_4.rom", &h1800 )
		Carga_ROM( "galx_1_5.rom", &h2000 )
      ' graficos
		Carga_ROM( "galmidw.1j",   &h0000+rom_graf_ini )
		Carga_ROM( "galmidw.1k",   &h0800+rom_graf_ini )

	case 6: '( galap4 )
		directorio="galap4"
		Carga_ROM( "galnamco.u",   &h0000 )
		Carga_ROM( "galnamco.v",   &h0800 )
		Carga_ROM( "galnamco.w",   &h1000 )
		Carga_ROM( "galnamco.y",   &h1800 )
		Carga_ROM( "galnamco.z",   &h2000 )
      ' graficos
		Carga_ROM( "galx_4c1.rom", &h0000+rom_graf_ini )
		Carga_ROM( "galx_4c2.rom", &h0800+rom_graf_ini )

	case 7: '( galturbo )
		directorio="galturbo"
		Carga_ROM( "superg.u",     &h0000 )
		Carga_ROM( "galx.v",       &h0800 )
		Carga_ROM( "superg.w",     &h1000 )
		Carga_ROM( "galturbo.y",   &h1800 )
		Carga_ROM( "galturbo.z",   &h2000 )
      ' graficos
		Carga_ROM( "galturbo.1h",  &h0000+rom_graf_ini )
		Carga_ROM( "galturbo.1k",  &h0800+rom_graf_ini )
	Case 8: ' ( ROMTEST )
		directorio="romtest"
		Carga_ROM( "romtest.bin",     &h0000 )	
      ' graficos
		Carga_ROM( "1h.bin",       &h0000+rom_graf_ini )
		Carga_ROM( "1k.bin",       &h0800+rom_graf_ini )
				
end Select

