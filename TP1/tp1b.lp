max Ingresos
subject to
DefIngresos)    Ingresos - 2 Trenes - 10 ViasCubiertas = 0
DefTrenes)      Trenes - TrenesComprados = 1000
DefVias)        Vias - ViasCompradas = 48
CnsTrenesMin)   TrenesComprados >= 40
CnsPresupuesto) 20 TrenesComprados + 50 ViasCompradas <= 1000
CnsCobertura1)  0.050 Trenes - ViasCubiertas >= 0
CnsCobertura2)  Vias - ViasCubiertas >= 0
end
