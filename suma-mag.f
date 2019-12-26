      PROGRAM SUMA_MAG
      IMPLICIT NONE
      INTEGER I,J,L,K,IO,HEAD_LINES,COL1,COL2,jmaxp,jmaxs
      INTEGER IND_MIN1,IND_MIN2,MAX1,MAX2
      REAL*4 MP(0:5000,5),MS(0:5000,5),M(0:5000,5)
      REAL*4 AGEP(0:5000), AGES(0:5000),junk,DELTAT,MIN,DENO
      REAL*4 M_int
      CHARACTER*60 NAME1,NAME2,FMT_SALIDA,FMT_HEADER,FMT_SALIDA2
      CHARACTER*90 BARRA
      
      OPEN(20,FILE="PARAM.DAT",STATUS="OLD") !LECTURA DE PARAMETROS
      READ(20,*) HEAD_LINES
!NOMBRE DE LOS ARCHIVOS A LEER
      READ(20,*) NAME1          ! PRIMARIA
      READ(20,*) NAME2          ! SECUNDARIA
C      
      OPEN(10,FILE=NAME1, STATUS="OLD") !LECTURA DE DATOS
      OPEN(11,FILE=NAME2, STATUS="OLD")
      OPEN(30,FILE="SALIDA.DAT")
      DO I=0,HEAD_LINES-1 !HEAD_LINES : LINEAS DEL ENCABEZADO
         READ(10,*)
         READ(11,*)
      ENDDO

      !Componente primaria
      IO=0
      J=0
C     COL1=8 !Columna que quiero que lea
      COL1=4
      COL2=8
      DO WHILE(IO.EQ.0)
         READ(10,*,IOSTAT=IO) (JUNK, i=1,col1-1), AGEP(J)
     &        , (JUNK, i=COL1+1,COL2-1),(MP(J,I),I=1,5)
c        MP(j,1),MP(j,2),MP(j,3) !lee la edad y U,B,V,R,I
         J=J+1
      ENDDO
      jmaxp=j-2
      write(*,*) "Cantidad de lineas -- primaria:", jmaxp, "IO=",IO

      !Componente secundaria
      IO=0
      COL1=4
      COL2=8
      J=0
      DO WHILE(IO.EQ.0)
         READ(11,*,IOSTAT=IO) (JUNK, i=1,col1-1),AGES(J),(JUNK, i=COL1+1
     &        ,COL2-1),(MS(J,I),I=1,5)
c        ,MS(j,1),MS(j,2),MS(j,3) !Lee la edad, y U,B,V,R,I
         J=J+1
      ENDDO
      jmaxs=j-2
      write(*,*) "Cantidad de lineas -- secundaria:", jmaxs, "IO=",IO

      IF(AGES(JMAXP) .LT. AGES(JMAXS)) THEN ! Lo hago hasta la mas joven
         MAX1= JMAXP
      ELSE
         MAX1= JMAXS
      ENDIF

!     Condicion de igual edad
      OPEN(UNIT=40,FILE="TEST") !En 40 escribo los puntos que se
c      utilizan de la secundaria para combinar las mag.
      write(*,*) max1,agep(jmaxp), ages(jmaxs) !control
c     REVISADO
      DO J=0,MAX1               !Lo hago hasta la mas joven
         MIN=ABS(AGEP(J)-AGES(J))
         if(MIN > 0.01) then
            I=0
            ind_min1=0
            ind_min2=0
            DO WHILE(I<=JMAXS)
               DELTAT=ABS(AGEP(J)-AGES(I)) !ESTE RECORRE LA SECUNDARIA
               if (deltat < min) then ! Busco el mas cercano en edad del otro track
                  min=deltat
                  if (agep(j)-ages(i) < 0) then
                     ind_min1=i-1
                  else
                     ind_min1=i
                  endif
                  ind_min2=ind_min1+1
               endif
               I=I+1
            ENDDO
            if (ind_min1==0 .or. ind_min2==0) then !esto es porque si alguna no cumple la tolerancia
               if (agep(j)-ages(j) < 0) then
                  ind_min1=j-1
               else
                  ind_min1=j
               endif
               ind_min2=ind_min1+1
            endif
c     Verificacion
c            write(*,*) ages(j), agep(j), ages(ind_min1),ages(ind_min2)
c            write(*,*) ind_min1, ind_min2
c            write(*,*) agep(j)-ages(ind_min1), agep(j)-ages(ind_min2)
c            write(*,*) "=============================="
c
c            if ( agep(j)-ages(ind_min1) < 0 ) then
c               write(*,*) "ERROR", agep(j)-ages(ind_min2)
c               pause
c            endif
C     Interpolo en magnitud a la edad del track de la primaria
            DO L=1,5
               DENO=AGES(IND_MIN2)-AGES(IND_MIN1)
               if(DABS(DENO-0.D0).LT.1.D-6) THEN ! Por si tienen la misma edad
                  M(J,L)=-2.5*LOG10(10**(-0.4*MP(J,L))
     &                 +10**(-0.4*MS(IND_MIN1,L)))
c                  write(*,*) ind_min1 !control
               else
               M_INT=MS(IND_MIN1,L)+(MS(IND_MIN2,L)-MS(IND_MIN1,L))
     &              /DENO*(AGEP(J)-AGES(IND_MIN1)) 
C
                  M(J,L)=-2.5*LOG10(10**(-0.4*MP(J,L))+10**(-0.4*M_INT))
               endif
            ENDDO
!     Control
            WRITE(40,*) (MS(IND_MIN1,K),K=1,3), ind_min1, ind_min2
     &           ,AGEP(J), ages(ind_min1),min
     &       , AGES(IND_MIN2)-AGES(IND_MIN1)

         else
            DO L=1,5
               M(J,L)=-2.5*LOG10(10**(-0.4*MP(J,L))+10**(-0.4*MS(J,L)))
               WRITE(40,*) (MS(J,K),K=1,3)
            ENDDO
         endif
      ENDDO
C      
      !Formatos de salida
      FMT_SALIDA="(17(F8.5,1X))"
      FMT_HEADER="(A,1x,9(A4,5X),A8)"
      FMT_SALIDA2="(5(F8.5,1x),5(A8,1x),6(F8.5,1x),A8)"
      BARRA="#==========================================================
     &======================"
      WRITE(30,FMT_HEADER) "#","U_p","B_p","V_p","U_s","B_s","V_s",
     &     "U_ps","B_ps","V_ps", "AGE[Myr]"
      WRITE(30,"(A90)") BARRA
      DO I=0,MAX1
         if (I > jmaxs ) then
            write(30,FMT_SALIDA2) (MP(I,L), L=1,5), "########",
     &           "########","########", "########", "########"
     &           , (M(I,L), L=1,5), AGEP(i), "########"
         else
            write(30,FMT_SALIDA) (MP(I,L), L=1,5), (MS(I,L), L=1,5)
     &    , (M(I,L), L=1,5), AGEP(i), ages(i) !Escribo las 5 magnitudes
         endif
       enddo
c
      close(10)
      close(11)
      close(20)
      close(30)
      END 
