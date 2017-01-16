      subroutine rayisoconv3d(tempo,cmatriz,j,nr,ns,ni,zextx1,zextx2,a,
     +zexty1,zexty2,zextz1,zextz2,zextz3,zextx3,zexty3,xs,xr,nupo,datx,
     +daty,fupo,vapox,vapoy)
c      double precision xs(10,2),xr(100,2)
c      double precision xgrid(2000),zgrid(2000)
c      double precision x(31),z(31),zong(31)
c      double precision hx,hz,v(31),zext1(16),zext2(16)
c      double precision time,g(29)
c      double precision d(29),c(29),e(29)   
c      double precision deltax(29)
      implicit double precision(a-h,o-z)
      parameter (tole=0.0000001,miter=130,m=8)

	integer ldx,ni,mr,nn,i,j,rayo,itera,ilstotal
      integer io,is,k,kod,eka,eg

      real tt1(2),tt2(2),tiempo1,tiempo2,etime
      dimension xs(20,3),xr(200,3),x(20),y(20),z(20),xy(20)
      dimension zong(20),v(20),zextx1(10),zextx2(10),g(20),a(20)
      dimension gplus(20),w(0:m-1),ybar(20),zexty1(10),zexty2(10)
      dimension fupo(20),copol(20,20),kod(10),vapox(20,20),vapoy(20,20)
      dimension zextx3(10),zexty3(10),pend(20,3),datx(10),daty(10)
      dimension zextz1(10),zextz2(10),zextz3(10),xytemp(20),nupo(10)
	!=====================variables de salida=========================
	dimension cmatriz(j,3)!agregado, matriz con las coordenadas x y z
	dimension tempo(ni*ns*nr) !agregado, vector de tiempos de viajes
	!==================================================================
      !character*80 input
	j=0
c-----------------------------------------------------------
c This program computes the rays going from the
c source (xs) to the receiver (xr) that
c reflects on a reflector Zi(x). The reflector could be a
c straight line or could be a cubic  spline  that
c interpolates the points :
c
c   (xgrid(i),z(i)) , i=1,..,xdim
c
c where z(i) is given by the user.
c
c The problem we solve to compute one ray from
c a given xs to a given xr is
c
c       Min T(x,z)
c       
c 
c-----------------------------------------------------------
c      open(unit=2,file='pend.in',status='unknown')
c      open(unit=3,file='vapox.in',status='unknown')
c      open(unit=4,file='vapoy.in',status='unknown')
c      open(unit=11,file='copol.in',status='unknown')

c    

	!print *, 'nr=', nr,'ni=', ni,'ns=', ns
	!do i=1,2*ni+1
	!   print *, 'a(i)=', a(i)
      !enddo

	!do i=1,ns
      !print *, 'xs=',xs(i,1),xs(i,2),xs(i,3)
	!enddo
	!do i=1,ns
	!print *, 'xr=',xr(i,1),xr(i,2),xr(i,3)
	!enddo
	
	!do i=1,ni+1
	!print *, zextx1(i),zexty1(i),zextz1(i)
      !print *, zextx2(i),zexty2(i),zextz2(i)
      !print *, zextx3(i),zexty3(i),zextz3(i)
	!enddo

c      write(6,*) 'NOMBRE DEL ARCHIVO DE ENTRADA'
c      read(5,'(a)') input
c      open(unit=1,file=input,status='old')
c---------------------------------------------------------
c Reading the number of interfaces, number of shots per
c interface, number of receptores in the model
c--------------------------------------------------------
c      read(1,*) ni
c      read(1,*) ns
c      read(1,*) nr

	
c-----------------------------------------------------------
c Reading the velocity vector (down going and up going
c velocities
c-----------------------------------------------------------
c      a(1) = 0.0d0
c      do i=2,2*ni+1
c         read(1,*) a(i)
c      enddo
c      print *,'a=',a(1:20)

c------------------------------------------------------------
c Readind the coordenates of the source and the receiver
c-----------------------------------------------------------
c      do k=1,ns
c         read(1,*)xs(k,1),xs(k,2),xs(k,3)
c      enddo
c      do k=1,nr
c         read(1,*)xr(k,1),xr(k,2),xr(k,3)
c      enddo  
c-------------------------------------------------------
c The leading dimension for the xgrid and zgrid
c-------------------------------------------------------
c      cpu = ctime()   
      ldx =2*ni +1   

c-----------------------------------------------------------
c      INICIALIZACION
c-----------------------------------------------------------
      do i=1,ni+1 
         do j=1,20
           copol(i,j)=0.0d0
           vapox(i,j)=0.0d0
           vapoy(i,j)=0.0d0

         enddo  
      enddo 
      do i=1,20
       do j=1,3
         pend(i,j)=0.0d0
       enddo
      enddo
      do i=1,10
      nupo(i)=0
      enddo          
c---------------------------------------------------------
c  Reading the points that define the interface zi
c  Calculando las pendientes correspondientes a las rectas
c  que describen algunas de las interfaces y calculando
c  los coeficientes de los polinomios de las interfaces 
c  restantes 
c---------------------------------------------------------

      do 1 i=1,ni+1
c         read(1,*) kod(i)
c         if (kod(i) .eq. 0) then
c           read(1,*) zextx1(i),zexty1(i),zextz1(i)
c           read(1,*) zextx2(i),zexty2(i),zextz2(i)
c           read(1,*) zextx3(i),zexty3(i),zextz3(i)
           pend(i,1)=(zexty2(i)-zexty1(i))*(zextz3(i)-zextz1(i))-
     +     (zextz2(i)-zextz1(i))*(zexty3(i)-zexty1(i))
           pend(i,2)=(zextz2(i)-zextz1(i))*(zextx3(i)-zextx1(i))-
     +     (zextx2(i)-zextx1(i))*(zextz3(i)-zextz1(i))
           pend(i,3)=(zextx2(i)-zextx1(i))*(zexty3(i)-zexty1(i))-
     +     (zextx3(i)-zextx1(i))*(zexty2(i)-zexty1(i)) 
		!print *, pend(i,3)

c         else
c           read(1,*) nupo(i)
c           do k=1,nupo(i)
c             read(1,*) datx(k),daty(k),fupo(k)
c             vapox(i,k)=datx(k)
c             vapoy(i,k)=daty(k)
c           enddo 
         if (nupo(i) .ne. 0) then
           call incoef3(nupo(i),datx,daty,fupo)
           do k=1,nupo(i)
             copol(i,k)=fupo(k)
           enddo 
         endif       
    1 continue
      call mayor(nupo,ni,mmax)
c      write(11,*) mmax
c      write(2,*) 'pend'
      !do i=1,ni+1
c         write(2,*) pend(i,1),pend(i,2),pend(i,3)  
      !enddo   
c      write(3,*)'vapox'
      !do i=1,ni+1
c         write(3,*)(vapox(i,j),j=1,mmax)
      !enddo
c      write(4,*) 'vapoy'
      !do i=1,ni+1
c         write(4,*) (vapoy(i,j),j=1,mmax)
      !enddo
c      write(11,*) 'copol'
      !do i=1,ni+1
c         write(11,*) (copol(i,j),j=1,mmax)
      !enddo
       
c-------------------------------------------------------
c Defining the number of rays
c-------------------------------------------------------
       mr = nr*ni*ns
c----------------------------------------------------------
c Creating the velocity matrix for the initial model. 
c nn is the number of points in the raypath.
c---------------------------------------------------------
      tiempo1=etime(tt1)
c      write(6,*) 'tiempo1=',tiempo1
      rayo = 0
      itera=0
      ilstotal = 0
	eka=0
      do is=1,ni
         nn = 2*(2*is-1)
         call velocity(a,v,is,ldx,ni)
c---------------------------------------------------------
c defining the end x-coordinates of the ray from xs to xr
c---------------------------------------------------------
         do 2 i=1,ns
            do 3 j=1,nr 
               x(1) = xs(i,1) 
               x(2*is+1) = xr(j,1)
               y(1)=  xs(i,2)
               y(2*is+1) = xr(j,2)
               do io=2,2*is
                  x(io) = 0.0d0
                  y(io) = 0.0d0
               enddo
               ldn=2*is+1       
c----------------------------------------------------------
c Calling the Minimization subroutine
c-----------------------------------------------------------
         call mintime(f,is,v,x,y,xy,xytemp,z,g,zong,ldn,m,nupo,
     +   tole,gplus,ybar,w,miter,iter,ils,ievalf,iflag,nn,pend,
     +   zextx1,zexty1,zextz1,kod,copol,vapox,vapoy,rayo)
!====================MODIFICADO POR EG=============================
		!SALIDA: MATRIZ CON LAS COORDENADAS X Y Z
	  do eg=1,2*is+1
	!   print*, 'COORDENADAS: '
	!   print*, x(i),y(i),z(i)

		eka=eka+1
          cmatriz(eka,1)=x(eg) !matriz con coordenadas  X Y Z
          cmatriz(eka,2)=y(eg)
          cmatriz(eka,3)=z(eg)
	  enddo
	  tempo(rayo)=f !vector tiempos de viaje
	!print*, 'eka', eka

      !print*, 'MATRIZ'
	!do eg=1,eka
      !  print*, cmatriz(eg,1),cmatriz(eg,2),cmatriz(eg,3)
	!enddo


!=================================================
         itera= itera + iter
         ilstotal = ilstotal + ils
   3        continue
   2     continue
      enddo
      !write(6,*)'el numero total de iteraciones es',itera
      !write(6,*) 'el numero total de line search es',ilstotal
c      cpu = ctime()
c      write(6,*) cpu 
      tiempo2 =  etime(tt2)
      ttt1= tt2(1)-tt1(1)
      ttt2 = tt2(2)-tt1(2)
      ttt3 = ttt2+ttt1
      !write(6,50) ttt1,ttt2,ttt3
      !50 format('usertime',d14.6,'systemtime',d14.6,'elapsetime',
      !* d14.6)
      ! write(6,*) 'tiempo2=',tiempo2
      ! write(6,*) 'elapsed time=',tiempo2 - tiempo1
c      stop
      end
  
      subroutine mintime(f,ni,v,x,y,xy,xytemp,z,g,zong,ldn,m,nupo,
     +tol,gplus,ybar,w,maxit,iter,ils,ievalf,iflag,nn,pend,zextx1,
     +zexty1,zextz1,kod,copol,vapox,vapoy,rayo)
                  
c      double precision time,v(ldx),x(ldx) 
c      double precision g(ldnn),d(ldnn),e(ldnn),c(ldnn)
c      double precision z(ldx),zong(ldx),deltax(ldnn)
      implicit double precision (a-h,o-z)
      dimension v(ldn),x(ldn)
	dimension g(nn),xy(nn),z(ldn),y(ldn)
      dimension zong(ldn),gplus(nn),ybar(nn),w(0:m-1),pend(20,3)
      dimension xtemp(20),ytemp(20)
      dimension zextx1(10),zexty1(10),kod(10),copol(20,20),vapox(20,20)
      dimension vapoy(20,20),nupo(10)
      dimension xytemp(2*ldn),zextz1(10)
      integer ni,ldn,nn,iter,maxit,m,ils,ievalf,iflag,i,kod,nupo
      integer rayo
      !open(unit=8,file='ray2.out',status='unknown')
      call iniguess(x,y,ni,ldn)

c----------------------------------------------------------------------------
c Ajustando el vector xr
c----------------------------------------------------------------------------      
   
      do i=2,2*ni
      xy(i-1)=x(i)
      xy(2*ni-2+i)=y(i)
      end do
      call gbb(nn,m,xy,xytemp,x,y,g,tol,gplus,ybar,maxit,w,xtemp,ytemp,
     +iter,ils,ievalf,iflag,z,v,zong,ldn,ni,pend,zextx1,zexty1,
     +zextz1,kod,copol,vapox,vapoy,nupo)
      rayo = rayo +1
      call interface(pend,zextx1,zexty1,zextz1,x,y,z,ni,ldn,kod,copol,
     +               vapox,vapoy,nupo)
      call dist(x,y,z,zong,ni,ldn)
      call traveltime(v,zong,ni,ldn,f) 
      !write(8,*) 'the optimal values of x,y and z  para rayo',rayo
      do 1 i=1,2*ni+1
         
	   !write(8,10) x(i),y(i),z(i)

   10    format(3x,f14.7,5x,f14.7,7x,f14.7)
   12    format(f10.7,f10.7,f10.7,f10.7,f10.7,f10.7,f10.7)
    1 continue
      !write(8,*) 'the optimal travel time is',f 
      return
      end
      subroutine gbb(n,m,xy,xytemp,x,y,g,tol,gplus,ybar,maxit,w,xtemp,
     +              ytemp,iter,ils,ievalf,iflag,z,v,zong,ldn,ni,pend,
     +            zextx1,zexty1,zextz1,kod,copol,vapox,vapoy,
     +             nupo)
c
c     subroutine gbb implements the global Barzilai and 
c     Borwein method, to find local minimizers of a given 
c     function,  described in the paper "The Barzilai and 
c     Borwein gradient method for the large scale 
c     unconstrained minimization problem" to appear in 
c     SIAM J. Opt., by M. Raydan. 
c
c     subroutines evalf and grad must be supplied by the 
c     user to evaluate  the function f and the gradient 
c     vector g, respectively. 
c
      implicit double precision (a-h,o-z)
      integer n,iter,k,maxit,m,ils,ievalf,iflag,ldn,ni,kod,nupo
      dimension xy(n),g(n),gplus(n),ybar(n),w(0:m-1),x(ldn)
      dimension z(ldn),v(ldn),zong(ldn),pend(20,3),y(ldn)
      dimension zextx1(10),zexty1(10),kod(10),copol(20,20),vapox(20,20)
      dimension vapoy(20,20)
      dimension xtemp(ldn),ytemp(ldn),xytemp(n),zextz1(10),nupo(10)
c      double precision x(n),g(n),a,tol,f,r1
c      double precision gplus(n),y(n),w(0:m-1)
c      double precision tempn,tempd,dnrm2,alpha,xtemp(n)
c
c     input variables
c
c        n        size of the problem 
c        m        number of previous function values 
c                 to be considered in the nonmonotone 
c                 line search. 
c        xy       initial guess in Rn
c        tol      stopping criteria: 
c                 2-norm(g)< tol*(1+|f|)  
c        maxit    maximum number of iterations
c
c      output variables 
c
c        xy       approximation to the local minimizer
c        g        gradient at xy
c        iter     number of iterations required to 
c                 stop the process. Also the number of
c                 gradient evaluations during the process
c        ils      number of line searches required 
c                 during the process
c        ievalf   number of function evaluations 
c                 required during the process 
c        iflag    termination parameter, iflag=0 (convergence),
c                 iflag =1 (too many iterations)
c
!      open(unit=7,file='barz.out',status='unknown')
!      write(7,10)
! 10   format(//,10x,'Global Barzilai and Borwein ',//)
!      write(7,20)
! 20   format(1x,'iteration',7x,'f(x)',9x,'2-norm(g(x))',
!     1       8x,'alpha',//)
c
c        evaluate f and g at initial  xy
c
      iter=1
      ils=0
      iflag=0
c      call evalf(n,x,f)
      call interface(pend,zextx1,zexty1,zextz1,x,y,z,ni,ldn,kod,copol,
     +vapox,vapoy,nupo)
      call dist(x,y,z,zong,ni,ldn)
      call traveltime(v,zong,ni,ldn,f)
      ievalf=1
      w(0)=f 
      call grad(x,y,v,zong,ni,ldn,n,g,z,pend,kod,
     +          copol,vapox,vapoy,nupo)
      !do i=1,n
      !enddo
      r1=dnrm2(n,g,1)
 40   a=max(1.0d0,r1)
 
c
c        stop after too many iterations
c
 30   if (iter .gt. maxit) go to 180
c
!      write(7,50) iter,f,r1,a
! 50   format(3x,i3,7x,d12.6,5x,d12.6,7x,d12.6)
c
c        on convergence stop
c
      if (r1 .le. tol*(1.0d0+dabs(f))) go to 200
c
      alpha=1.0d0/a
	!========================================================================= 
	if (a==0.0D0) then
		print "(/)"
		print "(11x,(a))","___________________ERROR__________________"
		print "(11x,(a))","              DIVISION POR CERO           "
		print "(11x,(a))","                  LINEA 398               "
		print "(11x,(a))","               RAYISOCONV3D.F             "
		print "(//)"
		stop
	end if
	!============================================================================
c
c      nonmonotone line search
c
      call nonmlines(n,m,iter,xy,xytemp,x,y,w,g,r1,f,alpha,xtemp,ytemp,
     +             ils,ievalf,z,v,zong,ldn,ni,pend,zextx1,zexty1,
     +              zextz1,
     +               kod,copol,vapox,vapoy,nupo)
      w(mod(iter,m))=f 
c
c        compute  vector ybar (as in secant equation)
c
      call interface(pend,zextx1,zexty1,zextz1,x,y,z,ni,ldn,kod,copol,
     +               vapox,vapoy,nupo)
      call dist(x,y,z,zong,ni,ldn)
	!print *, 'x=', x
	!print *, 'y=', y
	!print *, 'z=', z

	!print *, 'zong=', zong
      call grad(x,y,v,zong,ni,ldn,n,gplus,z,pend,kod,
     +          copol,vapox,vapoy,nupo)
      r1=dnrm2(n,gplus,1) 
      do 80 k=1,n
	  ybar(k) = gplus(k) - g(k)
 80   continue
c
c       compute alpha for next iteration
c       (Barzilai and Borwein choice)
c
      tempn=0.0d0
      tempd=0.0d0
c
      do 90 k=1,n
	  tempn=tempn + g(k)*ybar(k)
	  tempd=tempd + g(k)*g(k)
 90   continue 
c
      a =-tempn/(tempd*alpha)
	!========================================================================= 
	if (tempd*alpha==0.0D0) then
		print "(/)"
		print "(11x,(a))","___________________ERROR__________________"
		print "(11x,(a))","              DIVISION POR CERO           "
		print "(11x,(a))","                  LINEA 447               "
		print "(11x,(a))","               RAYISOCONV3D.F             "
		print "(//)"
		stop
	end if
	!============================================================================

c
c        set up for next iteration
c
      do 100 k=1,n
	  g(k) = gplus(k)
 100  continue
      iter = iter + 1
c     
c     parameter epsilon, in the paper, is set to 1.d-10
c
      if (a .le. 1.0d-10) then
       write (7,*) 'change direction'
       go to 40
      else
       go to 30
      endif
c
 180  iflag=1
 200  return
      end
       function dnrm2(n,sx,incx)
       double precision sx(n),u,zal,dnrm2
       integer n,incx,k
c
c      very simple version of the 2-norm described
c      in the library LINPACK
c
	!print *, 'n=',n
	!print *, 'sx=',sx(1:n)
	!print *, 'incx=',incx

       dnrm2=0.0d0
       zal=1.0d-15
	k=1
       do 30 i=1,n
	   u=dabs(sx(k))
	    if (u .gt. zal) then
		dnrm2=dnrm2*(zal/u)*(zal/u)
	!========================================================================= 
	       if (u==0.0D0) then
		     print "(/)"
		     print "(11x,(a))","___________________ERROR__________________"
		     print "(11x,(a))","              DIVISION POR CERO           "
		     print "(11x,(a))","                  LINEA 497               "
		     print "(11x,(a))","               RAYISOCONV3D.F             "
		     print "(//)"
		     stop
	      end if
	!============================================================================
		zal=u
		dnrm2=dnrm2+1.0d0
	      else
		dnrm2=dnrm2 + (u/zal)*(u/zal)
	     end if
c
	  k=k+incx
 30    continue
c	
	dnrm2=zal*dsqrt(dnrm2)
c

       return
       end
      subroutine nonmlines(n,m,iter,xy,xytemp,x,y,w,g,r1,f,alpha,xtemp,
     +      ytemp,ils,ievalf,z,v,zong,ldn,ni,pend,zextx1,zexty1,
     +       zextz1,
     +           kod,copol,vapox,vapoy,nupo)
      implicit double precision (a-h,o-z)
      integer n,m,iter,ils,ievalf,ktemp,ldn,ni,i,j,kod,nupo
      dimension xy(n),w(0:m-1),g(n),xtemp(ldn),x(ldn),y(ldn),ytemp(ldn)
      dimension z(ldn),v(ldn),zong(ldn),zextx1(10),zexty1(10)
      dimension kod(10),xytemp(n),zextz1(10),nupo(10),pend(20,3)
      dimension copol(20,20),vapox(20,20),vapoy(20,20)
      
c      double precision x(n),w(0:m-1),r1,r2,f,alpha,r3
c      double precision g(n),xtemp(n),evalf,fold,atemp
c           
c      r2 is the maximum of the last m values of f
c      r3 =  gkTgk  times 10**-4
c
      ktemp=ievalf+1
      r3=r1*r1*1.0d-4
      fold=f
c
      r2=w(0)
      do 20 i=1,min(iter,m-1)
      if (w(i) .gt. r2) then
	 r2=w(i)
       else
      endif
 20   continue
c
 30   do  j=1,n
      xytemp(j)=xy(j)-alpha*g(j)
      enddo
      xtemp(1)=x(1)
      xtemp(ldn)=x(ldn)
      ytemp(1)=y(1)
      ytemp(ldn)=y(ldn) 
      do i=2,ldn-1
      xtemp(i)=xytemp(i-1)
      ytemp(i)=xytemp(i+ldn-3)
      enddo
      !do i=1,ldn
      !enddo
      call interface(pend,zextx1,zexty1,zextz1,xtemp,ytemp,z,ni,ldn,kod,
     +              copol,vapox,vapoy,nupo)
      call dist(xtemp,ytemp,z,zong,ni,ldn)
      call traveltime(v,zong,ni,ldn,f)
c
c      call evalf(n,xtemp,f)
      ievalf=ievalf + 1
c
      if (f .gt. (r2-alpha*r3)) then 
	 atemp=(r1*r1*alpha*alpha)/(2.0d0*(f+r1*r1*alpha -fold))
!	 write (7,*) 'backtracking, quadratic fit'
	   if (atemp .lt. 0.1d0*alpha) then 
	     alpha=0.1d0*alpha
	   else
	     alpha=atemp
	   endif
	   if (1.0d-010 .gt. alpha) then 
	     go to 15
	   else
	   endif    
	   go to 30
      else
  15    endif
c
      do 50 j=1,n
      xy(j)=xytemp(j)
 50   continue
      do j=1,n/2
      x(j+1)=xy(j)
      y(j+1)=xy((n/2)+j)
      enddo
c
      if (ievalf .gt. ktemp) ils=ils+1
      return
      end



      



      subroutine interface(pend,zextx1,zexty1,zextz1,x,y,z,ni,ldx,kod,
     +                     copol,vapox,vapoy,nupo)
c      double precision zext1(ldl),zext2(ldl),z(ldx)
      implicit double precision(a-h,o-z)
      dimension z(ldx),pend(20,3),zextx1(10),zexty1(10),x(ldx),y(ldx)
      dimension kod(10),copol(20,20),vapox(20,20),vapoy(20,20)
      dimension zextz1(10),nupo(10)
      integer ni,ldx,kod,i,j,i2,nupo

c-----------------------------------------------------------
c This subroutine defines each interface as a function
c of the variable x.
c-----------------------------------------------------------
      !========================================================================= 
	do i=1,ni  
	  if (pend(i,3)==0.0D0) then
		print "(/)"
		print "(11x,(a))","___________________ERROR__________________"
		print "(11x,(a))","              DIVISION POR CERO           "
		print "(11x,(a))","                  LINEA 609               "
		print "(11x,(a))","               RAYISOCONV3D.F             "
		print "(//)"
		stop
	  end if
	end do
	!============================================================================
	
	do i=1,ni
         if (kod(i) .eq. 0) then
            z(i) = zextz1(i)-(pend(i,1)/pend(i,3))*(x(i)-zextx1(i))
     +             -(pend(i,2)/pend(i,3))*(y(i)-zexty1(i))
            i2=2*ni+2-i
            z(i2) = zextz1(i)-(pend(i,1)/pend(i,3))*(x(i2)-zextx1(i))
     +             -(pend(i,2)/pend(i,3))*(y(i2)-zexty1(i))
         else
            i2=2*ni+2-i
            z(i)=0
            z(i2)=0
            do j=1,nupo(i)
            bnorma1=((x(i)-vapox(i,j))**2+(y(i)-vapoy(i,j))**2)**(0.5d0)
            bnorma2=((x(i2)-vapox(i,j))**2+(y(i2)-vapoy(i,j))**2)
            bnorma2=bnorma2**(0.5d0)
              if (bnorma1 .eq. 0.0d0) then
                 z(i)=z(i)+0.0d0
              else
               z(i)=z(i)+copol(i,j)*((bnorma1)**2.0d0)*(dlog(bnorma1)-1)
              endif   
              if (bnorma2 .eq. 0.0d0) then
                 z(i2)=z(i)+0.0d0
              else
                 aux=((bnorma2)**2.0d0)*(dlog(bnorma2)-1)
                 z(i2)=z(i2)+copol(i,j)*aux
              endif     
            enddo   
         endif
      enddo
      if (kod(ni+1) .eq. 0) then
         i=ni+1
         z(i) = zextz1(i)-(pend(i,1)/pend(i,3))*(x(i)-zextx1(i))
     +             -(pend(i,2)/pend(i,3))*(y(i)-zexty1(i))
      else
         i=ni+1 
         z(i)=0
         do j=1,nupo(i)
           bnorma1=((x(i)-vapox(i,j))**2+(y(i)-vapoy(i,j))**2)**(0.5d0) 
           if (bnorma1 .eq. 0.0d0) then
                 z(i)=z(i)+0.0d0
              else
               z(i)=z(i)+copol(i,j)*((bnorma1)**2.0d0)*(dlog(bnorma1)-1)
           endif 
         enddo
      endif
      
      return 
      end


      subroutine velocity(a,v,ni,ldx,ncapa)
      implicit double precision(a-h,o-z)
      dimension v(ldx),a(ldx)
c      double precision v(ldx),a(31)
      integer ni,ldx,i,ncapa

c----------------------------------------------------------
c This subroutine defines the velocity as a function of x.
c here we consider the simplest case vi=constant, where
c i indicates the ith-interface.
c----------------------------------------------------------
      !print *, 'trazador'
      !print *,'a=',a(1:2*ni+1)
	!print *,'ni=',ni
      !========================================================================= 
	do i=2,2*ni+1  
	  if (a(i)==0.0D0) then
		print "(/)"
		print "(11x,(a))","___________________ERROR__________________"
		print "(11x,(a))","              DIVISION POR CERO           "
		print "(11x,(a))","                  LINEA 686               "
		print "(11x,(a))","               RAYISOCONV3D.F             "
		print "(//)"
		stop
	  end if
	end do
	!============================================================================

	v(1) = 0.0d0
      do i=2,ni+1
         v(i) = 1.0d0/a(i)
      enddo
      do 1 i=2,ni+1
         v(i+ni) = 1.0d0/a(2*ncapa+i -ni)
    1 continue
      return
      end
     

      
            
      subroutine iniguess(x,y,ni,ldx)
      implicit double precision(a-h,o-z)
      dimension x(ldx),y(ldx)
c      double precision x(ldx),z(ldx)
c      double precision haux
      integer ni,nni,ldx,i

c-----------------------------------------------------------
c This subroutine computes an initial guess for the
c ray going from xs to xr. This initial guess will be used
c by the Newton method to obtain the ray with minimun
c travel time.
c-----------------------------------------------------------
      nni = 2*ni
      ri=0.0d0
      do i=1,nni
      ri=ri+1.0d0
      enddo
      
      haux = (x(nni+1)-x(1))/ri
      hauy = (y(nni+1)-y(1))/ri
      do 1 i=2,nni
         x(i)=x(1) + (i-1)*haux
         y(i)=y(1) + (i-1)*hauy
    1 continue
      return
      end






      subroutine grad(x,y,v,zong,n,ldn,nn,g,z,pend,kod,
     +   copol,vapox,vapoy,nupo)
      implicit double precision(a-h,o-z)
      dimension x(ldn),y(ldn),v(ldn),zong(ldn),g(nn),z(ldn),pend(20,3)
      dimension kod(20),copol(20,20),nupo(10)
      dimension vapox(20,20),vapoy(20,20)
c      double precision x(ldn),G(ldnn),z(ldn) 
c      double precision x(ldn),G(ldnn),zong(29),z(ldn) 
c      double precision aux1,aux2,v(ldn),zong(ldn)
      integer n,ldn,i,kod,nupo

c------------------------------------------------------------
c This subroutine evaluates the gradient of the travel time
c           A
c function t (x,z).
c           B
c-----------------------------------------------------------
c      aux1 = v(2)*((x(2)-x(1))+(pend(1)*(z(2)-z(1))))/zong(2)
      

	do 1 i=2,n+1 
         if (kod(i) .eq. 0) then 
	!========================================================================= 
	    if (pend(i,3)==0.0D0 .or. zong(i)==0.0D0 . or. 
     +		zong(i+1)==0.0D0) then
		  print "(/)"
		  print "(11x,(a))","___________________ERROR__________________"
		  print "(11x,(a))","              DIVISION POR CERO           "
		  print "(11x,(a))","                  LINEA 798               "
		  print "(11x,(a))","               RAYISOCONV3D.F             "
		  print "(//)"
	      stop
	    end if
	!============================================================================

            aux1 =v(i)*((x(i)-x(i-1))+(-1.0d0*(pend(i,1)/pend(i,3)))
     +             *(z(i)-z(i-1)))/zong(i)

            aux2 = v(i+1)*((x(i+1)-x(i))+(-1.0d0*(pend(i,1)/pend(i,3)))
     +             *(z(i+1)-z(i)))/zong(i+1) 
			!print*,'zong=',zong(i+1)
		
         else
            der=0.0d0
            do j=1,nupo(i)
         bnorma=((x(i)-vapox(i,j))**2+(y(i)-vapoy(i,j))**2)**(0.5d0) 
               if (bnorma .eq. 0.0d0)  then
                  der=der+0.0d0
               else
                  der=der+copol(i,j)*
     +            ((x(i)-vapox(i,j))*(2*dlog(bnorma)-1))
               endif
            enddo
	!========================================================================= 
	    if (zong(i)==0.0D0 .or. zong(i+1)==0.0D0) then
		  print "(/)"
		  print "(11x,(a))","___________________ERROR__________________"
		  print "(11x,(a))","              DIVISION POR CERO           "
		  print "(11x,(a))","                  LINEA 835               "
		  print "(11x,(a))","               RAYISOCONV3D.F             "
		  print "(//)"
	      stop
	    end if
	!============================================================================
            aux1 = v(i)*((x(i)-x(i-1))+(der*(z(i)-z(i-1))))/zong(i)
            aux2 = v(i+1)*((x(i+1)-x(i))+(der*(z(i+1)-z(i))))/zong(i+1)

		endif        
            g(i-1) = aux1 - aux2
c        aux1=aux2
    1 continue
	          

      do 2 i=n+2,2*n
         i2=2*n+2-i  
         if (kod(i2) .eq. 0) then 
	!========================================================================= 
	     if (zong(i)==0.0D0 .or. zong(i+1)==0.0D0 .or. 
     +		 pend(i2,3)==0.0D0) then
		   print "(/)"
		   print "(11x,(a))","___________________ERROR__________________"
		   print "(11x,(a))","              DIVISION POR CERO           "
		   print "(11x,(a))","                  LINEA 852               "
		   print "(11x,(a))","               RAYISOCONV3D.F             "
		   print "(//)"
	       stop
	     end if
	!============================================================================

            aux1 =v(i)*((x(i)-x(i-1))+(-1.0d0*(pend(i2,1)/pend(i2,3)))
     +             *(z(i)-z(i-1)))/zong(i)
          aux2 = v(i+1)*((x(i+1)-x(i))+(-1.0d0*(pend(i2,1)/pend(i2,3)))
     +             *(z(i+1)-z(i)))/zong(i+1) 
          !print *, 'zong=',zong(i+1)
		!print *, 'aux2=',aux2

         else
            der=0.0d0
            do j=1,nupo(i2)
         bnorma=((x(i)-vapox(i2,j))**2+(y(i)-vapoy(i2,j))**2)**(0.5d0) 
               if (bnorma .eq. 0.0d0)  then
                  der=der+0.0d0
               else
                  der=der+copol(i2,j)*
     +            ((x(i)-vapox(i2,j))*(2*dlog(bnorma)-1))
               endif
            enddo
	!========================================================================= 
	     if (zong(i)==0.0D0 .or. zong(i+1)==0.0D0) then
		   print "(/)"
		   print "(11x,(a))","___________________ERROR__________________"
		   print "(11x,(a))","              DIVISION POR CERO           "
		   print "(11x,(a))","                  LINEA 900               "
		   print "(11x,(a))","               RAYISOCONV3D.F             "
		   print "(//)"
	       stop
	     end if
	!============================================================================

            aux1 = v(i)*((x(i)-x(i-1))+(der*(z(i)-z(i-1))))/zong(i)
            aux2 = v(i+1)*((x(i+1)-x(i))+(der*(z(i+1)-z(i))))/zong(i+1)
          endif        
            g(i-1) = aux1 - aux2
c        aux1=aux2
    2 continue
    
      do 3 i=2,n+1 
         if (kod(i) .eq. 0) then
	!========================================================================= 
	     if (zong(i)==0.0D0 .or. zong(i+1)==0.0D0 .or. 
     +		 pend(i,3)==0.0D0) then
		   print "(/)"
		   print "(11x,(a))","___________________ERROR__________________"
		   print "(11x,(a))","              DIVISION POR CERO           "
		   print "(11x,(a))","                  LINEA 904               "
		   print "(11x,(a))","               RAYISOCONV3D.F             "
		   print "(//)"
	       stop
	     end if
	!============================================================================

           aux3 = v(i)*((y(i)-y(i-1))+(-1.0d0*(pend(i,2)/pend(i,3)))
     +      *(z(i)-z(i-1)))/zong(i)
           aux4 = v(i+1)*((y(i+1)-y(i))+(-1.0d0*(pend(i,2)/pend(i,3)))
     +      *(z(i+1)-z(i)))/zong(i+1)
         else
            der=0.0d0
            do j=1,nupo(i)
         bnorma=((x(i)-vapox(i,j))**2+(y(i)-vapoy(i,j))**2)**(0.5d0) 
               if (bnorma .eq. 0.0d0) then
                  der=der+0.0d0
               else
                  der=der+copol(i,j)*
     +            ((y(i)-vapoy(i,j))*(2*dlog(bnorma)-1))
               endif 
            enddo
	!========================================================================= 
	     if (zong(i)==0.0D0 .or. zong(i+1)==0.0D0) then
		   print "(/)"
		   print "(11x,(a))","___________________ERROR__________________"
		   print "(11x,(a))","              DIVISION POR CERO           "
		   print "(11x,(a))","                  LINEA 936               "
		   print "(11x,(a))","               RAYISOCONV3D.F             "
		   print "(//)"
	       stop
	     end if
	!============================================================================

            aux3 = v(i)*((y(i)-y(i-1))+(der*(z(i)-z(i-1))))/zong(i)
            aux4 = v(i+1)*((y(i+1)-y(i))+(der*(z(i+1)-z(i))))/zong(i+1)
          endif        
            g(2*n+i-2) = aux3 - aux4
c        aux1=aux2
            
    3 continue 
      do 4 i=n+2,2*n
         i2=2*n+2-i 
         if (kod(i2) .eq. 0) then
	!========================================================================= 
	     if (zong(i)==0.0D0 .or. zong(i+1)==0.0D0 .or. 
     +		 pend(i2,3)==0.0D0) then
		   print "(/)"
		   print "(11x,(a))","___________________ERROR__________________"
		   print "(11x,(a))","              DIVISION POR CERO           "
		   print "(11x,(a))","                  LINEA 954               "
		   print "(11x,(a))","               RAYISOCONV3D.F             "
		   print "(//)"
	       stop
	     end if
	!============================================================================

           aux3 = v(i)*((y(i)-y(i-1))+(-1.0d0*(pend(i2,2)/pend(i2,3)))
     +      *(z(i)-z(i-1)))/zong(i)
           aux4 = v(i+1)*((y(i+1)-y(i))+(-1.0d0*(pend(i2,2)/pend(i2,3)))
     +      *(z(i+1)-z(i)))/zong(i+1)
         else
            der=0.0d0
            do j=1,nupo(i2)
         bnorma=((x(i)-vapox(i2,j))**2+(y(i)-vapoy(i2,j))**2)**(0.5d0) 
               if (bnorma .eq. 0.0d0) then
                  der=der+0.0d0
               else
                  der=der+copol(i2,j)*
     +            ((y(i)-vapoy(i2,j))*(2*dlog(bnorma)-1))
               endif 
            enddo
	!========================================================================= 
	     if (zong(i)==0.0D0 .or. zong(i+1)==0.0D0) then
		   print "(/)"
		   print "(11x,(a))","___________________ERROR__________________"
		   print "(11x,(a))","              DIVISION POR CERO           "
		   print "(11x,(a))","                  LINEA 973               "
		   print "(11x,(a))","               RAYISOCONV3D.F             "
		   print "(//)"
	       stop
	     end if
	!============================================================================

            aux3 = v(i)*((y(i)-y(i-1))+(der*(z(i)-z(i-1))))/zong(i)
            aux4 = v(i+1)*((y(i+1)-y(i))+(der*(z(i+1)-z(i))))/zong(i+1)
          endif        
            g(2*n+i-2) = aux3 - aux4
c        aux1=aux2
            
    4 continue
      return
      end     





      


      subroutine dist(x,y,z,zong,n,ldn)
      implicit double precision(a-h,o-z)
      dimension x(ldn),y(ldn),z(ldn),zong(ldn)
c      double precision x(ldn),z(ldn),zong(ldn)
      integer n,ldn,i

c-------------------------------------------------------------
c This subroutine computes the arc length between two 
c points of the two consecutives interfaces.
c In this case the arc length is given by the euclidean
c distance between two points.
c-----------------------------------------------------------
      zong(1) = 0.0d0
      do 1 i=2,2*n+1 
        zong(i)=sqrt((x(i)-x(i-1))**2+(y(i)-y(i-1))**2+(z(i)-z(i-1))**2)
    1 continue
      return
      end





      subroutine traveltime(v,zong,ni,ldx,f)
      implicit double precision(a-h,o-z)
      dimension v(ldx),zong(ldx)
c      double precision v(ldx),zong(ldx)
c      double precision time
      integer ni,ldx,i

c-------------------------------------------------------------
c This subroutine evaluates the time function we
c want to minimize, at the optimal set of points 
c (xi,zi),i=1,2*ni+1  where, xi is in the interface zi.
c----------------------------------------------------------
      f = 0.0d0
      do 1 i=2,2*ni+1
         f = f + v(i)*zong(i)
    1 continue
      return
      end
      
      
     



      subroutine mayor(nupo,ni,mmax)
      implicit double precision(a-h,o-z)
      dimension nupo(ni+1)
      integer ni,mmax

c------------------------------------------------------------
c This subroutine computes de maximun of a vector nupo
c-------------------------------------------------------------
      mmax=nupo(1)
      do i=2,ni+1
         if (nupo(i) .gt. mmax) then
             mmax= nupo(i)
         endif
      enddo
      return
      end 


