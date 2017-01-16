      subroutine incoef3(nupo,datx,daty,fupo)
      implicit double precision(a-h,o-z)
      dimension datx(60),daty(60),fupo(60),A(60,60),ipvt(60),vt(60)
      integer nupo,info,i,j
      call calmat(A,datx,daty,nupo)
      do j=1,nupo
       vt(j)=a(nupo,j)
      enddo
       vf=fupo(nupo)
      do i=nupo,2,-1
        fupo(i)=fupo(i-1)
        do j=1,nupo
          A(i,j)=A(i-1,j)
        enddo
      enddo
      do j=1,nupo
        A(1,j)=vt(j)
      enddo
      fupo(1)=vf 
           
      call dgefa(A,60,nupo,ipvt,info)
      call dgesl(A,60,nupo,ipvt,fupo,0)
      
      
      return
      end
c-----------------------------------------------------------------------
c rutina que calcula los coeficientes de la matriz A
c-----------------------------------------------------------------------
      subroutine calmat(A,datx,daty,nupo)
      implicit double precision(a-h,o-z)
      integer nupo,i,j
      dimension datx(60),daty(60),A(60,60)
      do i=1,nupo
          do j=1,nupo
             bnorma=((datx(i)-datx(j))**2+(daty(i)-daty(j))**2)**(0.5d0)
             if (bnorma .eq. 0) then
                A(i,j)=0.0d0
             else
                A(i,j)=((bnorma)**2.0d0)*(dlog(bnorma)-1)
             endif      
          enddo
      enddo
      return
      end
          
      
