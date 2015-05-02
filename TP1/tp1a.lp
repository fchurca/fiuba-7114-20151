max Xh + Xj + Xe + Xs

subject to
! Vicepresidencia mutuamente excluyente
Yvh + Yvj + Yve + Yvs < 1
! Sectores
 99 Yvh + 3 Xh -   Yc -   Yd > 0 ! Corriente histórica
-99 Yvj + 1 Xj -   Yd -   Yd < 0 ! Juventud
 99 Yve + 1 Xe +   Yc +   Yi > 2 ! Empresarios
 99 Yvs + 1 Xs - 2 Yc - 2 Yi > 0 ! Sindicato
end
! Apoyo de los distintos sectores
int Xh ! Corriente histórica
int Xj ! Juventud
int Xe ! Empresarios
int Xs ! Sindicatos

! Asuntos
int Yc  ! Huelga en Campana: A favor del sector empresario
int Yd  ! Lista de diputados: Otorgar vacante a juventud
int Yi  ! Apertura de importaciones
int Yvh ! Vicepresidencia: Corriente Histórica
int Yvj ! Vicepresidencia: Juventud
int Yve ! Vicepresidencia: Empresarios
int Yvs ! Vicepresidencia: Sindicatos

