	subroutine lectura(nr,ns,ni,zextx1,zextx2,a,zexty1,zexty2,zextz1,
     +zextz2,zextz3,zextx3,zexty3,xs,xr,nupo,datx,daty,fupo,vapox,vapoy)
	!lectura del archivo de datos, proveniente de el programa trazado de rayos
	!Esta subrutina devuelve, un vector de tiempos de viaje y una matriz con
	!las coordenadas de los rayos.
	
	use herramientas !funciones y subrutinas

	implicit double precision(a-h,o-z)
	integer ns,nr,ni
      integer ::i,k,ios,j
	character(len=25)::arch 
	integer, parameter :: n=3, e=5
      dimension zextx1(10),zextx2(10),a(20),zexty1(10),zexty2(10)
	dimension zextz1(10),zextz2(10),zextz3(10),zextx3(10),zexty3(10)
      dimension xs(20,3),xr(200,3),nupo(10),datx(10),daty(10),fupo(20)
	dimension vapox(20,20),vapoy(20,20)
	dimension kod(10),p_col(20,3) !puntos colineales (para la verificación)
	

	write(6,*) 'NOMBRE DEL ARCHIVO DE ENTRADA: DATA REAL'
      read(5,'(a)') arch 
      open(unit=1,file=arch,status='old',iostat=ios)
	print *, '' 
	print *, '' 

	call error(ios)		
	!if(ios/=0) then !falla de lectura
	!  print "(/)"
	!  print "(11x,(a))","___________________ERROR__________________"
	!  print "(11x,(a))","              FALLA DE LECTURA         "
	!  print "(11x,(a))","( Nombre de archivo incorrecto o Inexistente )"
	!  print "(//)"
	!  stop
	!end if

c---------------------------------------------------------
cLectura del número de capas, receptores y fuentes en 
c                  el modelo

c--------------------------------------------------------
      read(1,*,iostat=ios) ni
	call error(ios)		
      read(1,*,iostat=ios) ns
	call error(ios)		
      read(1,*,iostat=ios) nr
	call error(ios)		
	!estos valores deben ser mayores a cero
	if(ni<=0 .or. ns<=0 .or. nr<=0 .or. ns>20 .or. nr>200 .or. ni>9) then 
	  print "(/)"
	  print "(11x,(a))","___________________ERROR_______________________"
	  print "(11x,(a))","              FALLA DE USUARIO         "
		if (ni<=0) then
			print "(12x,(a))","( Numero de capas menor o igual a cero )"
			print "(//)"
			stop
		else if (ns<=0) then
			print "(12x,(a))","( Numero de fuentes menor o igual a cero )"
			print "(//)"
			stop
		else if (nr<=0) then
			print "(12x,(a))","( Numero de receptores menor o igual a cero )"
			print "(//)"
			stop
		else if (ns>20) then
		    print "(10x,(a))","( Numero de fuentes supera el 
     +limite establecido )"
			print "(//)"
			stop
		else if (nr>200) then
		    print "(10x,(a))","( Numero de receptores supera el 
     +limite establecido )"
		    print "(//)"
		    stop
		else if (ni>9) then
		    print "(10x,(a))","( Numero de capas supera el 
     +limite establecido )"
		    print "(//)"
		    stop
		end if
	end if

c-----------------------------------------------------------
cLectura del vector de velocidades
c
c-----------------------------------------------------------

      a(1) = 0.0d0
      do i=2,2*ni+1
         read(1,*,iostat=ios) a(i)
	   call error(ios)		
		if(a(i)<=0) then !falla de usuario
			print "(/)"
			print "(11x,(a))","___________________ERROR______________________"
			print "(11x,(a))","              FALLA DE USUARIO         "
			print "(14x,(a))","( Velocidades menores o iguales a cero )"
			print "(//)"
			stop
		end if
      enddo
	
	j=2*(ni-1)+1
	do i=2,ni+1
		if ( a(i)/=a(i+j) ) then
			print "(/)"
			print "(11x,(a))","___________________ERROR______________________"
			print "(11x,(a))","              FALLA DE USUARIO         "
			print "(12x,(a))","( Velocidades de subida y bajada diferentes )"
			print "(//)"
			stop
		end if
		j=j-2
	end do
c------------------------------------------------------------
c Readind the coordenates of the source and the receiver
c-----------------------------------------------------------
      do k=1,ns
         read(1,*,iostat=ios)xs(k,1),xs(k,2),xs(k,3)
	   call error(ios)		
      enddo
      do k=1,nr
         read(1,*,iostat=ios)xr(k,1),xr(k,2),xr(k,3)
	   call error(ios)		
      enddo  	
c---------------------------------------------------------
c  Reading the points that define the interface zi
	do i=1,ni+1
         read(1,*,iostat=ios) kod(i)
	   call error(ios)		
         if (kod(i) .eq. 0) then
           read(1,*) zextx1(i),zexty1(i),zextz1(i)
           read(1,*) zextx2(i),zexty2(i),zextz2(i)
           read(1,*) zextx3(i),zexty3(i),zextz3(i)
	!Verificación de que los puntos que definen una interface sean no colineales
	     p_col(i,1)=(zexty2(i)-zexty1(i))*(zextz3(i)-zextz1(i))-
     +     (zextz2(i)-zextz1(i))*(zexty3(i)-zexty1(i))
           p_col(i,2)=(zextz2(i)-zextz1(i))*(zextx3(i)-zextx1(i))-
     +     (zextx2(i)-zextx1(i))*(zextz3(i)-zextz1(i))
           p_col(i,3)=(zextx2(i)-zextx1(i))*(zexty3(i)-zexty1(i))-
     +     (zextx3(i)-zextx1(i))*(zexty2(i)-zexty1(i)) 
	  
		 if(p_col(i,1)==0 .and. p_col(i,2)==0 .and. p_col(i,3)==0) then 
			print "(/)"
			print "(11x,(a))","___________________ERROR_______________________"
			print "(11x,(a))","              FALLA DE USUARIO         "
			print "(12x,(a))","( Interfaces definidas con Puntos Colineales )"
			print "(//)"
			stop
		 end if
		 !if(zextz1(i)/=zextz2(i) .or. zextz1(i)/=zextz3(i) 
     +	!	 .or. zextz2(i)/=zextz3(i)) then 
		!	print "(/)"
		!	print "(11x,(a))","___________________ERROR_______________________"
		!	print "(11x,(a))","              FALLA DE USUARIO         "
		!	print *, "               Coordenadas z de la interface numero" 
	    !   print *,"                     ", i
     		!	print *,"                       diferentes entre si "
		!	print "(//)"
		!	stop
		! end if
	   else
           read(1,*) nupo(i)
           do k=1,nupo(i)
             read(1,*) datx(k),daty(k),fupo(k)
             vapox(i,k)=datx(k)
             vapoy(i,k)=daty(k)
           enddo
	   endif
	enddo  

	do i=1,ni
		do j=i+1,ni+1
			if( zextz1(i) >= zextz1(j) ) then 
				!print *, i,zextz1(i),j,zextz1(j)
				print "(/)"
				print "(11x,(a))","________________________ERROR___
     +_________________________"
				print "(15x,(a))","              FALLA DE USUARIO         "
				print "(13x,(a))","( Valores de z deben ser diferentes 
     +entre interfaces )"
				print "(//)"
				stop
			end if
		end do
	end do



	close(unit=1) !cerrar archivo de lectura

	end subroutine lectura
     
	!*************************** FIN SUBRUTINA LECTURA **************************