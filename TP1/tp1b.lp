max Ingresos
subject to
DefIngresos)	Ingresos - 2000000 Trenes - 10000 ViasCubiertas = 0
DefTrenes)		Trenes - TrenesComprados = 1000
DefVias)		Vias - ViasCompradas = 48000
CnsTrenesMin)	TrenesComprados >= 40
CnsPresupuesto)	20000000 TrenesComprados + 50000 ViasCompradas <= 1000000000
CnsCobertura1)	50 Trenes - ViasCubiertas >= 0
CnsCobertura2)	Vias - ViasCubiertas >= 0
end
gin Trenes
gin TrenesComprados
