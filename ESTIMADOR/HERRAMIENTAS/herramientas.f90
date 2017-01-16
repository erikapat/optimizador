	module herramientas

	implicit none

	contains
!========================   SUBRUTINA ACTUALIZAR   =============================	
	subroutine actualizar(n,x,ni,zextz1,zextz2,zextz3)
	
	implicit none
	integer, parameter  :: dp = selected_real_kind(12, 60)
    integer, intent(in)     :: n, ni !dimension de x(n) y H(ni+1) respectivamente
    real (dp), intent(in)   :: x(n) !vector con los elementos del vector H Y V
    real (dp), intent(in out):: zextz1(10),zextz2(10),zextz3(10) !coordenadas z de
	!las interfaces
	real (dp):: h(ni+1) !vector de desplazamientos verticales
    integer:: i,j

!Esta subrutina se encarga de trasladar verticalmente, segun sea el valor de H,
!las interfaces
!================================================================================
!H:PROFUNDIDADES
	j=1
	h(1:ni+1)=0.0D0 !se obtienen los h del vector x
	do i=2*ni+1,n
		j=j+1
		h(j)=x(i)
	end do

	j=1
	do i=2,ni+1
	!(a1,b1,c1+h1),(a2,b2,c2+h2),(a3,b3,c3+h3)
		j=j+1
		zextz1(j)=zextz1(j)+h(i)
		zextz2(j)=zextz2(j)+h(i)
		zextz3(j)=zextz3(j)+h(i)
	end do
end subroutine actualizar
	
!*****************************  SUBRUTINA COTAS ****************************************
	subroutine cotas(n,x,x2,ni,lv,uv,lh,uh)
	
	implicit none
	integer, parameter  :: dp = selected_real_kind(12, 60)
	integer, intent(in) :: ni, n
	real(dp), intent(in)   :: x(n), x2(2*ni+1)
    real(dp), intent(out):: lv,uv,lh,uh
	integer :: i,j
	real(dp)::hh(ni),veloc(2*ni)
	

	j=0
	do i=1,n
		if (i<=2*ni) then
			veloc(i)=x(i)
		else if (i>2*ni) then
			j=j+1
			hh(j)=x(i)
		end if
	end do
	

	!el valor lv tiene que ser mayor estricto a cero
	lv=0.5D0*(min((minval(veloc(1:2*ni))),(minval(x2(2:2*ni+1)))))  	
	uv=1.2D0*(max(maxval(veloc(1:2*ni)),maxval(x2(2:2*ni+1)))) 
	lh=0.0D0*(minval(hh(1:ni)))
	uh=1.2D0*(maxval(hh(1:ni)))
   
	!print *,'cota inferior de v=',lv
	!print *,'cota superior',uv
	!print *,'min hh',lh
	!print *,'max hh',uh

    end subroutine cotas
 !********************************************************************************************
	
      !***************************** SUBRUTINA ERROR *******************************

      ! DESCRIPCIÓN: si el archivo no existe o esta vacío, se produce un error
	
	subroutine error(ios)
	
	implicit none
	integer, intent(in)::ios
		
		if(ios/=0 .and. ios==29) then !falla de lectura
			print "(/)"
			print "(11x,(a))","___________________ERROR__________________"
			print "(11x,(a))","              FALLA DE LECTURA         "
			print "(11x,(a))","(   Nombre incorrecto o archivo Inexistente )"
			print "(//)"
			stop
				
		else if (ios/=0 .and. ios==24) then
			print "(/)"
			print "(11x,(a))","________________ ___ERROR__________________"
			print "(11x,(a))","                ARCHIVO VACÍO         "
			print "(11x,(a))","(          Final de archivo leido        )"
			print "(//)"
			stop
	    else if (ios/=0) then
			print "(/)"
			print "(11x,(a))","___________________ERROR__________________"
			print "(11x,(a))","              FALLA DE LECTURA         "
			print "(//)"
			stop
				
		end if
	end subroutine error
	
  !********************** FIN SUBRUTINA ERROR ****************************
 
 !********************** SUBRUTINA tiempo_total ****************************
   
   	function tiempo_total(t)result(vet)
	
	implicit none
	
    real (4), intent(in)   :: t
    real (4) :: vet(4), tt
	integer :: centinela
!Indica el tiempo en horas, minutos y segundos que tarda el programa en obtener la solución
	!Inicialización
	vet(1:3)=0.0d0
	centinela=0
	tt=t
	do while (centinela==0)
		if(tt>=3600) then !horas
			vet(1)=int(tt/3600)
			tt=tt-vet(1)*3600
		else if (tt<3600 .and. tt>=60) then !min
			vet(2)=int(tt/60)
			tt=tt-vet(2)*60
			vet(3)=int(tt)
			vet(4)=int((tt-int(tt))*100)
			centinela=1
		else !segundos
			vet(3)=int(tt)
			vet(4)=int((tt-int(tt))*100)
			centinela=1
		end if
	end do

end function tiempo_total
 !********************** FIN SUBRUTINA tiempo_total ****************************

	end module	