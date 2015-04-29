max Ingresos
subject to
DefIngresos)	Ingresos - 2 Trenes - 0.010 ViasCubiertas = 0
DefTrenes)		Trenes - TrenesComprados = 1000
DefVias)		Vias - ViasCompradas = 48000
CnsTrenesMin)	TrenesComprados >= 40
CnsPresupuesto)	20 TrenesComprados + 0.050 ViasCompradas <= 1000
CnsCobertura1)	50 Trenes - ViasCubiertas >= 0
CnsCobertura2)	Vias - ViasCubiertas >= 0
end
gin Trenes
gin TrenesComprados
