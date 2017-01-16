	!             *** CALCULO DE LA MATRIZ JACOBIANA ***
	program jacobiano
      
      implicit none

      !****************** DECLARACIONES ******************************
	!
		
	real(kind=8), dimension(:,:) , allocatable:: matriz,l,J,zz
	integer ni,nfilas,ns,nr,ios, errores
	integer, parameter :: n=3
	character(len=25)::arch_matriz 
	real(kind=8), dimension(:), allocatable :: v, h

      !*****************************************************************
	!                         DATOS INICIALES                         !
		
      !ni=número de capas
	ni=3
	!ns=fuentes
	ns=2
	!nr=receptores
	nr=2
      !vector velocidades
	allocate(v(2*ni+1))
	v=(/0,2,3,4,4,3,2/)
	!vector de h
	allocate(h(ni+1))
	h=(/0,1,2,3/)
    
      !end program P_Jacobiano


	!*****************************************************************
	print '(16x,(a),\)', 'NOMBRE DEL ARCHIVO:'
	read *, arch_matriz
	print '(/)' !espacio

	!sencilla función que cálcula el número de líneas que contiene el
	!archivo de lectura
      nfilas=numero_filas(ni,nr,ns) 

      allocate(matriz(nfilas,n))
	allocate(l(ni*nr*ns,2*ni+1))
	allocate(zz(ni*nr*ns,2*ni+1))
	allocate(J(ni*nr*ns,ni*nr*ns+ni))
      
	!lectura del archivo de datos
	matriz=lectura(arch_matriz, nfilas,nr,ns)
      
      !cálculo de la matriz de longitud
	call longitud(matriz, nfilas, ni,nr,ns,l,zz,errores)
	
	!se verifica que el vector de velocidades y la matriz de longitud
	!no posean ceros en lugares estrategicos
	call verificar(ni,nr,ns,l,v,errores)
	
	!FINALMENTE: cálculo de la matriz Jacobiana
	!TAMAÑO: ni*ns*nr X 3*ni
	J=matriz_jacobiana(l,zz,v,nfilas,ni,nr,ns)

	contains 

	!******************* FUNCION NUMERO_FILAS **************************
	!sencilla función que cálcula el número de líneas que contiene el
	!archivo de entrada

	function numero_filas(ni, nr, ns)result(nfilas)
	
	implicit none

	integer :: nfilas, i,j
      integer, intent(in) :: ni, nr,ns
      
      !cálculo del número de filas
	nfilas=0
	do i=1,ni
		do j=1,ns*nr !número de receptores por número de fuentes
			nfilas=nfilas+2*i+1
		end do
	end do

	end function numero_filas    



      !*********************** FIN FUNCION NUMERO_FILAS ****************************

	!*************************** FUNCION LECTURA **************************

	function lectura(arch_matriz, nfilas,nr,ns)result(matriz)
	!lectura del archivo de datos, proveniente de el programa trazado de rayos

	integer, intent(in):: nfilas,ns,nr
	character(len=*), intent(in)::arch_matriz 
	character(len=100)::a 
	integer ::i,j,k,ios,m
	integer, parameter :: n=3
      real(kind=8), dimension(nfilas,n) :: matriz
	
	!se abre el archivo con el nombre especificado
	open(unit=1, file=arch_matriz, status="old",iostat=ios)
	!función que verifica si se produjo un falla en el momento de
	!abrir el archivo
	call error(ios)

	
	m=0
	read(1,*)a !la primera línea del archivo es un string
      !lectura por filas
	do j=1,ni
		do k=1,ns*nr
			do i=1,2*j+1
				m=m+1
		        read(1,*,iostat=ios)matriz(m,1:n)
				call error(ios)	 
			end do
			if (m<nfilas) then 
				read(1,*)a !string
			end if
			read(1,*)a !última línea -> string
		end do
	end do
	
	
	end function lectura
     
     
	!*************************** FIN FUNCION LECTURA **************************
     
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
      
	!***************************** SUBRUTINA LONGITUD *******************************

	subroutine longitud(matriz, nfilas, ni,nr,ns,l,zz,errores)
	!cálculo de la matriz de longitud: longitud de rayo por cada capa, las
	!filas son cada una de las capas (ni capas) y las columnas (2*n+1) representan
	!los valores de la longitud, la primera columna que representa a l1 (longitud 1)
	!es igual a cero
	!Cada capa( cada filas) tiene un número determinado de longitudes (columnas diferentes
	!de cero)
	!ENTRADA:
	!matriz-> contiene los valores leidos en la función lectura
	!nfilas-> número de filas de matriz
	!ni,nr,ns->número de capas, receptores y fuentes respectivamente
	!SALIDA:
	!l-> matriz con los valores de las longitudes para cada capa
	!zz->matriz con las diferecias de la tercera columna de matriz, es decir, z(i)-z(i-1)
	!errores->si este valor es igual a cero al finalizar la función, todo OK, de lo 
	!contrario se produce un error (valor igual a 1)
	implicit none

      integer, intent(in):: nfilas, ni,nr,ns
	integer, parameter :: n=3
      real(kind=8), dimension(nfilas,n), intent(in) :: matriz
	real(kind=8), dimension(ni*nr*ns,2*ni+1), intent(out) :: l, zz
	integer, intent(out):: errores
	integer ::i,j,k,h,m,z,w
	real(kind=8) :: sum
	
	!INICIALIZACIÓN
	do i=1,ni*ns*nr
		do j=1,2*ni+1
			l(i,j)=0
			zz(i,j)=0
		end do
	end do
	
	errores=0
	h=0
	w=0
	do i=1,ni
		do z=1,ns*nr !esto tiene un verdadero efecto cuando ns*nr>1
			w=w+1
			m=1
			do j=h+2,h+2*i+1
				m=m+1
				sum=0
				do k=1,n !fórmula de longitud
					sum=sum+(matriz(j,k)-matriz(j-1,k))**2
				end do
			l(w,m)=sqrt(sum) !valor de la longitud
			zz(w,m)=matriz(j,3)-matriz(j-1,3) !diferencia de los z
	
	!Para evitar división por cero, verificamos que no haya valores iguales de cero
	!en caso de que haya alguno retornaremos al programa principal y la función verificar
	!dara un mensaje de error
			if (l(w,m)==0) then
				errores=1
				return
			end if

			end do
			h=h+2*i+1
		end do
	end do
			
	!imprimir
	!do i=1,ni*ns*nr
	!	print *, l(i,1:2*ni+1)
	!end do

	end subroutine longitud
	!***************************** FIN SUBRUTINA LONGITUD *******************************


	!***********************FUNCION MATRIZ_JACOBIANA****************************************
	!cálculo de la matriz Jacobiana. TAMAÑO: ni*ns*nr X 3*ni
	function matriz_jacobiana(l,zz,v,nfilas,ni,nr,ns)result(Jacobiano)
	!ENTRADA:
	!l-> matriz con los valores de las longitudes para cada capa
	!zz->matriz con las diferecias de la tercera columna de matriz, es decir, z(i)-z(i-1)
	!v-> vector de velocidades
	!nfilas-> número de filas de matriz
	!ni,nr,ns->número de capas, receptores y fuentes respectivamente
	!SALIDA:
	!Jacobiano: matriz Jacobiana

	implicit none

	integer, intent(in) :: nfilas, ni,nr,ns
	real(kind=8), dimension(2*ni+1), intent(in) :: v
	real(kind=8), dimension(ni*nr*ns,2*ni+1), intent(in) :: l,zz
	real(kind=8), dimension(ni*nr*ns,3*ni) :: Jacobiano
	real(kind=8), dimension(2*ni+1):: ver
	integer :: i,j,m,n,salto,k,rayo,b,w,f,g1,g2,kk,ii,aux
	real(kind=8):: sum1,sum2,sum3,sum4
	
	salto=ns*nr
	!numero de filas
	n=ni*nr*ns 
	!numero de columnas
	m=3*ni

	!***************************************
	!Para el caso ni=3, tenemos h2=h7, h3=h6, h4=h5, 
	!como z5-z4=(f5-h5)-(f4-h4)=f5-f4 que al derivar con
	!respecto a h5 o h4 da cero (esto sucede para la formula de l5). 
	!El vector ver guarda la posición en la que esto ocurre, es decir 
	!en la posición 5 ponemos un cero (ver màs abajo)
	aux=2*ni+1
	!inicialización
	ver(1:2*ni+1)=1
	ver(ni+2)=0
	!print *, "ver=", ver(1:2*ni+1)
	!****************************************


	!INICIALIZACIÓN
	!do i=1,n
			Jacobiano(1:n,1:m)=0
	!end do

	!el cálculo de la matriz jacobiana se divide en tres partes
	! se empieza calculando desde la última fila, tomando
	!1ero : columnas desde 1 a ni
	!2do  : columnas desde ni+1 a 2*ni
	!3ero : columnas desde 2*ni+1 a 3*ni
	!cada división implica diferentes cálculos 

	k=1
	kk=1
	w=n
	do j=1,m
		rayo=n+1
		if (j<ni+1 .and. k<n+1) then !1ero : columnas desde 1 a ni
			do i=n,k,-salto
				do b=1,salto
					rayo=rayo-1
					Jacobiano(rayo,j)=-l(rayo,j+1)/v(j+1)**2
				end do
			end do
		k=k+salto
		else if (ni<j<2*ni+1 .and. w>0) then !2do : columnas desde ni+1 a 2*ni
			f=0
			do i=n,w,-salto
				do b=1,salto
					rayo=rayo-1
					Jacobiano(rayo,j)=-l(rayo,j+1-2*f)/v(j+1)**2
				end do
				f=f+1
			end do
		w=w-salto
		else if(2*ni<j<m .and. kk<n+1) then !3ero:columnas desde 2*ni+1 a 3*ni
			rayo=n+1


			ii=j-2*ni

			do i=n,kk,-salto
			

				if (ii==i/salto) then !diagonal
					do b=1,salto
					   rayo=rayo-1
			!note que si ver==0, en algunas de las dos operaciones sum=0
					  sum1=ver(ii+1)*zz(rayo,ii+1)/(l(rayo,ii+1)*v(ii+1))
			          sum2=ver(ii+2)*zz(rayo,ii+2)/(l(rayo,ii+2)*v(2*ni+2-ii))

			
					   Jacobiano(rayo,j)=sum1-sum2
					

					end do
				else  !no diagonal
					do b=1,salto
						rayo=rayo-1
						
						g1=2*(i/salto)-j+2*ni+1
						g2=2*(i/salto)-j+2*ni+2
						
						sum1=ver(ii+1)*zz(rayo,ii+1)/(l(rayo,ii+1)*v(ii+1))
					    sum2=ver(ii+2)*zz(rayo,ii+2)/(l(rayo,ii+2)*v(ii+2))
						sum3=ver(g1)*zz(rayo,g1)/(l(rayo,g1)*v(2*ni+1-ii))
					    sum4=ver(g2)*zz(rayo,g2)/(l(rayo,g2)*v(2*ni+2-ii))	
	

						Jacobiano(rayo,j)=sum1-sum2+sum3-sum4


					end do
				end if
			end do
			kk=kk+salto
		end if
	end do	
	!guardamos la matriz en un archivo, para acceder a ella cuando sea necesario
	open(unit=1, file="salida.txt", status="unknown",action="write")
	do i=1,n
			write(1,*), Jacobiano(i,1:m)
	end do

	end function matriz_jacobiana
	!*********************** FIN FUNCION MATRIZ_JACOBIANA ****************************************
	
	!***************************** FUNCION VERIFICAR ****************************************
	!se verifica que el vector de velocidades y la matriz de longitud
	!no posean ceros en lugares estrategicos

	subroutine verificar(ni,nr,ns,l,v,errores)
	!ENTRADA:
	!ni->número de capas
	!l-> matriz con los valores de las longitudes para cada capa
	!zz->matriz con las diferecias de la tercera columna de matriz, es decir, z(i)-z(i-1)
	!v-> vector de velocidades
	!errores->si este valor es igual a cero al finalizar la función, todo OK, de lo 
	!contrario se produce un error (valor igual a 1)
	!SALIDA:
	!mensaje de error y fin de programa en caso de presentarse algo que no corresponda

	implicit none
	integer, intent(in) :: ni,nr,ns, errores
	real(kind=8), dimension(2*ni+1), intent(in) :: v
	real(kind=8), dimension(ni*nr*ns,2*ni+1), intent(in) :: l
	integer :: i, j, aux

	do i=2,2*ni+1
		if (v(i)==0) then
			print "(/)"
			print "(11x,(a))","___________________ERROR__________________"
			print "(11x,(a))","              VECTOR DE VELOCIDADES"
			print "(11x,(a))","(El vector de velocidades a partir del segundo"
			print "(11x,(a))","       indice debe ser distinto de cero       )"
			print "(//)"
			stop
		end if
	end do
		
	if (errores==1) then
		print "(/)"
		print "(11x,(a))","___________________ERROR__________________"
		print "(11x,(a))","              MATRIZ DE LONGITUD"
		print "(11x,(a))","(Valor o valores de longitud iguales a cero)"
		print "(//)"
		stop
	end if

	end subroutine verificar
	!***************************** FIN FUNCION VERIFICAR ****************************************

      
	end program jacobiano

     
