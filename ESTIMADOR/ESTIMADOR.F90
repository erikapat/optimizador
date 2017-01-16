
!Programa principal
program estimador

	use dfport !para el uso de la función de fortran RTC()
	use herramientas
	use fun_jac !conjunto de funciones y subrutinas para calcular el jacobiano
	use Spectral_Projected_Grad !gradiente espectral proyectado
	
	implicit none
	
	!!!!!!!!!!!!!!!!!!!!DECLARACIÓN DE VARIABLES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer, parameter  :: dp = selected_real_kind(12, 60)

	integer :: nr,ns,ni,rayos,num
	integer :: n, m, maxit, maxfc,iter,fcnt,gcnt,flag
	logical :: output
	real (dp) :: f,eps,pginfn,pgtwon,eps2, porcv, porch
	real (4) :: ta(2), dtim, vet(4)
	real (8) :: s,s1, total_time
	real (dp), dimension(:), allocatable :: x,x2
	real(dp),dimension(:),allocatable:: T, v, h
	real(dp),dimension(:,:),allocatable:: cmatriz
	character(len=25):: centinela="0" !,arch2
	integer :: i, ios, j

	!PARA LA FUNCIÓN LECTURA
    real (dp):: zextx1(10),zextx2(10),zextx3(10),zexty1(10),zexty2(10),zexty3(10)
    real (dp):: zextz1(10),zextz2(10),zextz3(10),xs(20,3),xr(200,3),nupo(10)
	real (dp):: datx(10),daty(10),fupo(20),vapox(20,20),vapoy(20,20) 
	real (dp):: a(20) !VELOCIDAD (OJO)
	real (dp):: lv,uv,lh,uh

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !inicio del ciclo while (centinela)
	do while (centinela=="0")
		print "(/)"! ESPACIO
		print "(//,12x,(a))", "*******************************************************"
		print "(12x,(a))", "*                                                      *"
		print "(12x,(a))", "*                ---  MENU  ---                        *"
		print "(12x,(a))", "* OPERACIONES:                                         *"
		print "(12x,(a))", "*       1.- Perturbar unicamente las velocidades (V)   *"
		print "(12x,(a))", "*       2.- Perturbar unicamente las despl. vert.(H)   *"
		print "(12x,(a))", "*       3.- Perturbar velocidades (V) y despl. vert.(H)*"
		print "(12x,(a))", "*       4.- SALIR                                      *"
		print "(12x,(a))", "*                                                      *"
		print "(12x,(a))", "********************************************************"
		print "(/)"! ESPACIO
		print "(16x,(a),\)", "SELECIONE UNA OPCION: "
		read *,centinela
		print "(//)"! ESPACIO
		
		if (centinela=="4") then
			print "(16x,(a),\)", "FIN DE PROGRAMA " !!------------->fin de programa
			print "(//)"! ESPACIO
			stop !---->salir
		else if (centinela=="1" .or. centinela=="2" .or. centinela=="3") then
			print "(16x,(a),\)", "INTRODUZCA PORCENTAJE ENTRE 0 Y 1: "
		    read *,porcv
			if(porcv<=0 .or. porcv>=1) then !falla de lectura
				print "(/)"
				print "(11x,(a))","___________________ERROR__________________"
				print "(11x,(a))","              FALLA DE USUARIO         "
				print "(11x,(a))","(   El porcentaje debe estar entre 0 y 1 )"
				print "(//)"
				stop
			end if
			print "(//)"! ESPACIO

			if (centinela=="1") then
				porch=0.0d0 !porcentaje de h
			else if (centinela=="2") then
				porch=porcv
				porcv=0.0d0
			else if (centinela=="3") then
				porch=porcv
			end if
		else 
			print "(16x,(a),\)", "OPCION NO VALIDA " !--------------->opción no válida
			print "(/)"
			centinela="0"
			cycle !---------------------> no se ejecutan las instrucciones siguientes
	  !se imprime nuevamente el menu para que el usuario teclee la opción correcta
		end if
	enddo 
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
	dtim = dtime(ta)
	!s=rtc() !hora en segundos al inicio del cálculo
	
	!archivo de entrada con los datos que caracterizan la estructura del subsuelo
	call lectura(nr,ns,ni,zextx1,zextx2,a,zexty1,zexty2,zextz1,zextz2,zextz3,zextx3, &
	             zexty3,xs,xr,nupo,datx,daty,fupo,vapox,vapoy)
	num=numero_filas(ni, nr, ns)
	!write(6,*) 'NOMBRE DEL ARCHIVO DE ENTRADA: DATOS PERTURBADOS'
    !read(5,'(a)') arch2 

	!PARÀMETROS PARA EL GEP
	 
	 m=21 !OJO M no puede ser igual a cero!!!!
	 n=3*ni
	 maxit = 200000
	 maxfc = 200000
	 output =.true. !escribe los iterados en caso de ser TRUE
	 eps = 1.0D-011   !criterio de parada
	 eps2 = 1.0D-011

	!DETERMINO RAYOS (numero de rayos)
	 rayos=ni*nr*ns
	!--------------------------------------------------
	!HABILITAR ESPACIO EN MEMORIA PARA LAS VARIABLES
	allocate(x(n))
	allocate(T(rayos))
	allocate(cmatriz(num,3))
	allocate(x2(2*ni+1))	
	!--------------------------------------------------

	!TRAZADOR DE RAYOS
    call rayisoconv3d(T,cmatriz,num,nr,ns,ni,zextx1,zextx2,a,zexty1,zexty2,zextz1, &
	                  zextz2,zextz3,zextx3,zexty3,xs,xr,nupo,datx,daty,fupo,vapox,vapoy)

	num=numero_filas(ni, nr, ns)
   !--------------------------------------------------
	!LIBERAR ESPACIO EN MEMORIA PARA LAS VARIABLES
	deallocate(cmatriz)
   !--------------------------------------------------
	
	x2(1:2*ni+1)=a(1:2*ni+1)

	!------------ PERTURBAR LOS DATOS -------------------!
	j=2
	do i=1,n
		if (i<2*ni+1) then
			x(i)=x2(i+1)*(1+porcv)
		else if (i>=2*ni+1) then
			x(i)=zextz1(j)*porch
			j=j+1
		end if
	end do
	!______---------------------------------------________
	
	call cotas(n,x,x2,ni,lv,uv,lh,uh)

   !--------------------------------------------------
	!LIBERAR ESPACIO EN MEMORIA PARA LAS VARIABLES
	deallocate(x2)
   !--------------------------------------------------
		

    call spg(n,x,m,eps,eps2,maxit,maxfc,output,f,pginfn,pgtwon,iter,fcnt,gcnt, &
             flag,num,nr,ns,ni,zextx1,zextx2,a,zexty1,zexty2,zextz1,zextz2,zextz3, &
		     zextx3,zexty3,xs,xr,nupo,datx,daty,fupo,vapox,vapoy,T,lv,uv,lh,uh)

	!--------------------------------------------------
	!HABILITAR ESPACIO EN MEMORIA PARA LAS VARIABLES:
	! velocidad y desplazamientos verticales
	allocate(v(2*ni+1))
	allocate(h(ni+1))
	!--------------------------------------------------
	!s1=rtc() !hora en segundos al final del cálculo

	print*,'______________________________________________________________________________'
	print*,"iter = ",iter
	print*,"fcnt (evaluaciones de la funcion)= ",fcnt
	print*,"gcnt (evaluaciones del gradiente)= ",gcnt
	print*,"flag = ",flag
	!print *,"X:"
	!print *,"x = ",x

	j=1
	v(1:2*ni+1)=0.0D0
	h(1:ni+1)=0.0D0
	do i=1,n
		if (i<2*ni+1) then
			v(i+1)=x(i)
		else if (i>=2*ni+1) then
			j=j+1
			h(j)=x(i)
		end if
	end do
	
	print *, '' 
	print *,'VELOCIDADES:'
	print 200,v(1:2*ni+1)
	200 format(10x,F21.14)
	print *, '' 
	print *,'DESPLAZAMIENTOS VERTICALES:'
	print 100,h(1:ni+1)
	100 format(10x,F16.14)
	
	print *, '' 
	print *, '' 

    !tiempo cpu
	dtim = dtime(ta)
	print *, 'TIEMPO CPU EN SEGUNDOS:',ta(1), 'seg.'
	vet=tiempo_total(ta(1))
	print *, '' 
	print *,'TIEMPO DE CPU EN HORAS:'
	print *, '' 
	print *,'         HORAS', '        MIN', '          SEG', '       CENT SEG'
	print *,int(vet(1)),':',int(vet(2)),':',int(vet(3)),':',int(vet(4))
	
	!tiempo en segundos que tarda el programa en dar respuesta
	!total_time=s1-s 
	!print *, '' 
	!vet=tiempo_total(total_time)
	!print *, 'tiempo en h:',vet(1),':',vet(2),':',vet(3)
	print *, '' 
	print *, '' 

   !--------------------------------------------------
	!LIBERAR ESPACIO EN MEMORIA PARA LAS VARIABLES
	deallocate(x)
	deallocate(T)
	deallocate(v)
	deallocate(h)
   !--------------------------------------------------


end program estimador