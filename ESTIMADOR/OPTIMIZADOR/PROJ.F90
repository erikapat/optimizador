  SUBROUTINE proj(n,x,inform,lv,uv,lh,uh,ni)
    IMPLICIT NONE
    INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
    INTEGER, INTENT(IN)        :: n
    REAL (dp), INTENT(IN OUT)  :: x(:)
    INTEGER, INTENT(OUT)       :: inform
	!AGREGADO:
	integer,intent(in) :: ni
	real (dp), intent(in):: lv,uv,lh,uh

	integer :: i
	
    inform = 0

!Se establecen cotas superiores e inferiores para los vectores velocidad V
!y desplazamientos verticales H para reducir el conjunto solución
!           lv: cota inferior del vector velocidad V
!           lh: cota inferior del vector desplazamientos verticales H
!			uv: cota superior del vector velocidad V
!			uh: cota superior del vector desplazamientos verticales H
	do i=1,n
		if(i<=2*ni) then
			if (x(i) < lv) then
				x(i) = lv
			else if (x(i) > uv) then
				x(i) = uv
			end if
		else if(i>2*ni) then
			if (x(i) < lh) then
				x(i) =0.0d0
			else if (x(i) > uh) then
				x(i) = uh
			end if
		end if
	end do
	


  END SUBROUTINE proj