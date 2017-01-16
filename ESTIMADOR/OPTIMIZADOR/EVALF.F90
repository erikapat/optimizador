  SUBROUTINE evalf(n,x,f,inform,num,nr,ns,ni,zextx1,zextx2,a,zexty1,zexty2,zextz1, &
                   zextz2,zextz3,zextx3,zexty3,xs,xr,nupo,datx,daty,fupo,vapox,vapoy,T)
    
	use herramientas

	IMPLICIT NONE
    INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)
    INTEGER, INTENT(IN)     :: n
    REAL (dp), INTENT(IN)   :: x(:)
    REAL (dp), INTENT(OUT)  :: f
    INTEGER, INTENT(OUT)    :: inform
	!============================================================================
	!PARAMETROS PARA EL TRAZADOR
	integer,   intent(in):: num,nr,ns,ni
    real (dp), intent(in out):: zextx1(10),zextx2(10),zextx3(10)
	real (dp), intent(in out):: zexty1(10),zexty2(10),zexty3(10)
    real (dp), intent(in out):: zextz1(10),zextz2(10),zextz3(10),xs(20,3),xr(200,3),nupo(10)
	real (dp), intent(in out):: datx(10),daty(10),fupo(20),vapox(20,20),vapoy(20,20) 
	real (dp), intent(in out):: a(20) !VELOCIDAD (OJO)
    !PARAMETROS DE SALIDA DEL TRAZADOR
	real(dp):: tempo(ni*ns*nr)
	real(dp):: ccmatriz(num,3)
    !============================================================================
	!TIEMPOS DE VIAJE REALES
	real (dp), intent(in) :: T(ni*ns*nr)
	!OTRAS VARIABLES
    integer :: rayos,i,nfilas,j
    real(dp) :: vel(20),z1(10),z2(10),z3(10), h(ni+1)

	inform = 0
	nfilas=num
	rayos=ni*ns*nr

!==============================================
!VELOCIDADES
	j=0
	vel(1:20)=0.0D0
	do i=2,2*ni+1
		j=j+1
		vel(i)=x(j)
	end do
!================================================
	a=vel
!====================================================================================    
!ACTUALIZACIÓN DE LAS COORDENADAS QUE DEFINEN LAS INTERFACES (solo z)
!Se parte del vector H perturbado (desplazamiento vertical), el cual es inicialmente 
!sumado a la coordenada z de las interfaces reales, obteniendo así las interfaces
!pertubadas.El vector H, por medio del método gradiente espectral proyectado
!decrece hasta ser igual a cero, obteniendose las interfaces originales 
	z1=zextz1
	z2=zextz2
	z3=zextz3

	call actualizar(n,x,ni,z1,z2,z3)
!==================================================================================
	!TRAZADOR DE RAYOS
	call rayisoconv3d(tempo,ccmatriz,nfilas,nr,ns,ni,zextx1,zextx2,a,zexty1,zexty2,z1,&
	                  z2,z3,zextx3,zexty3,xs,xr,nupo,datx,daty,fupo,vapox,vapoy)
!====================================================================================
	
	!EVALUCIÓN DEL FUNCIONAL	
	f=0.0D0
    do i=1,rayos
       f = f + (T(i)-tempo(i))**2
	end do
!===================================================================================

  END SUBROUTINE evalf