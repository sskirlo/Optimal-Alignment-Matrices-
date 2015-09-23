
	PROGRAM modSABAH65

c integer*8 are new 64 bit words which will be able to handle 7 by 7 matrices, not available when code was originally written

	IMPLICIT INTEGER(KIND=8) (A-Z)
        INTEGER*8 I, II, III, IIII, G, F, FF, GG, C, PHI, THE, Mask1
     &  KK1, KK2, TP(0:127), TPX(0:127), COUNT1, SUM1
	DIMENSION COR(0:127,0:127),IWT(0:127),A(20,-7:7),JJ(7),
     &  X(0:127),H(4),T(4),R(0:7,-6:6)
	M=1
	COUNT1=0
c if want program to end after executing a particular matrix, set Swit to 5555, else use 2
	OPEN (M,FILE='RESULT.TXT')

	
	CORMAX=3
	LMAX=4
	N=7
        IN=(2**N)-1
        IM=42/(2**(7-N))
        Base=2**8

	WRITE (M,677) LMAX,N,CORMAX
677	FORMAT (2X,'ROWS=',I2,3X,'COLUMNS=',I2,3X,'CORMAX=',I2)
	WRITE (M,678)
678	FORMAT (4X,'ROW1',3X,'ROW2',3X,'ROW3',3X,'ROW4')


C
C COMPUTATION OF CORRELATION MATRIX
C	
	IWT(0)=0
	IWT(1)=1
	IWT(2)=1
	IWT(3)=2
	LL=2
103	DO 104 II=0,((2**LL)-1)
	IWT((2**LL)+II)=IWT(II)+1
104	CONTINUE
	LL=LL+1
	IF (LL.LE.(N-1)) GO TO 103

	II=0
102	J=0
101	K=XOR(II,J)
	COR(II,J)=2*(N-IWT(K))
	J=J+1
	IF (J.LE.(2**N)-1) GO TO 101
	II=II+1
	IF (II.LE.(2**N)-1) GO TO 102
C
C GENERATING FLIPS OF (2**N) ROWS ABOUT THEIR V. AXIS
C
C	

c generates lookup table of horizontal flips of 7 bit words
	SS=0
	DO 55 I=1,N
	JJ(I)=0
55	CONTINUE
999	X(SS)=0
	DO 68 J=1,N
	X(SS)=X(SS)+(2**(J-1))*JJ(J)
68	CONTINUE
	SS=SS+1
	K=N
888	JJ(K)=JJ(K)+1
	IF (JJ(K).EQ.1) GO TO 999
	JJ(K)=0
	K=K-1
	IF (K.GE.1) GO TO 888

c calculate lookup for transpose operator

        DO 205 II=0,((2**N)-1)
c initialize to zero
	TP(II)=0
        VAR=II
	DO 206 III=0,N-1
	IF((VAR-2**(N-1-III))>=0) GO TO 207	
	GO TO 206
207     TP(II)=TP(II)+BASE**(7-III)
        VAR=VAR-2**(N-III-1)
206     CONTINUE	
205	CONTINUE
c        write(*,*) 'TP(5)', TP(5)


c find operator which includes horizontal flip followed by transpose
        DO 208 II=0,((2**N)-1)
c initialize to zero
	TPX(II)=0
        VAR=X(II)
	DO 209 III=0,N-1
	IF((VAR-2**(N-1-III))>=0) GO TO 210	
	GO TO 209
210     TPX(II)=TPX(II)+BASE**(7-III)
        VAR=VAR-2**(N-III-1)
209     CONTINUE	
208	CONTINUE
        write(*,*) 'TPX(5)', TPX(5)
	write(*,*) 'X(1)', X(1)

C
C	EXTRACTION AND TEST WORDS
C




c       words will be formatted as follows, 56, 48, 40, 32, 24, 16, 8, 0, will start from largest down

c       these words extract 1st and 2nd row so can identify shifts to make lexiographically least
c       need to define base=2**8 b/c otherwise compiler won't except large numbers, even though word size is big enough (64 bits)
        
	III=(Base**7)*IN
	IIII=(Base**6)*IN
	FF=(Base**7)*((2**(N-1))+(2**(N-2)))
	G=(Base**7)*(2**(N-1))
	F=(Base**7)*(2**(N-2))
	GG=(Base**6)*(2**(N-1))	
C	
C	INITIALIZING THE FIRST HEAD AND TAIL
C
	NH=1
	NT=1
	H(NH)=0
	T(NT)=0
8	W=NH+NT   

c        W=4
c        H(1)=2
c        H(2)=10
c	T(2)=28
c	T(1)=75


c   switches based on width, need to specify switch for all possiblities
	GO TO (14,14,15,16,10001,10002,10003),W
C
C PERFORMING V.FLIPS,H.FLIPS AND VH.FLIPS.
C



c these create word I, and generate lexiographically least of v,vh,and h, by making result lex least by going to 113 and testing for all 3 operations
14	K=0
	A(1,0)=H(1)
	A(2,0)=T(1)
	C=(Base**7)*IN+(Base**6)*IN
	PHI=(Base**7)*IM+(Base**6)*IM
	THE=(Base**6)*IN
	I=(Base**7)*A(1,0)+(Base**6)*A(2,0)
	II=(Base**7)*A(2,0)+(Base**6)*A(1,0)
	GO TO 113
1123	II=(Base**7)*X(A(1,0))+(Base**6)*X(A(2,0))
	GO TO 113
1143	II=(Base**7)*X(A(2,0))+(Base**6)*X(A(1,0))
	K=-1
	GO TO 113

15	K=2
	A(1,0)=H(1)
	A(2,0)=H(2)
	A(3,0)=T(1)
	C=(Base**7)*IN+(Base**6)*IN+(Base**5)*IN
	PHI=(Base**7)*IM+(Base**6)*IM+(Base**5)*IM
	THE=(Base**6)*IN
	I=(Base**7)*A(1,0)+(Base**6)*A(2,0)+(Base**5)*A(3,0)
	II=(Base**7)*A(3,0)+(Base**6)*A(2,0)+(Base**5)*A(1,0)
c        WRITE(*,*) 'V',II
	GO TO 113
1122	II=(Base**7)*X(A(1,0))+(Base**6)*X(A(2,0))+(Base**5)*X(A(3,0))
c        WRITE(*,*) 'H',II
	GO TO 113
1142	II=(Base**7)*X(A(3,0))+(Base**6)*X(A(2,0))+(Base**5)*X(A(1,0))
c        WRITE(*,*) 'VH',II
	K=-1
	GO TO 113

16	K=4
	A(1,0)=H(1)
	A(2,0)=H(2)
	A(3,0)=T(2)
	A(4,0)=T(1)
	C=(Base**7)*IN+(Base**6)*IN+(Base**5)*IN+(Base**4)*IN
	PHI=(Base**7)*IM+(Base**6)*IM+(Base**5)*IM+(Base**4)*IM
	THE=(Base**6)*IN+(Base**4)*IN
	I=(Base**7)*A(1,0)+(Base**6)*A(2,0)+(Base**5)*A(3,0) 
     &  +(Base**4)*A(4,0)
	II=(Base**7)*A(4,0)+(Base**6)*A(3,0)+(Base**5)*A(2,0) 
     &  +(Base**4)*A(1,0)
	GO TO 113
1121	II=(Base**7)*X(A(1,0))+(Base**6)*X(A(2,0))+(Base**5)*X(A(3,0))
     &  +(Base**4)*X(A(4,0))
	GO TO 113
1141	II=(Base**7)*X(A(4,0))+(Base**6)*X(A(3,0))+(Base**5)*X(A(2,0))
     &  +(Base**4)*X(A(1,0))
	K=-1
	GO TO 113

c  need to be careful with k and the, unclear from pattern how these should be modified exactly
10001	K=6
c        write(*,*) 'W', W  
c        write(*,*) '',''
c end testing code
	A(1,0)=H(1)
	A(2,0)=H(2)
        A(3,0)=H(3)
	A(4,0)=T(2)
	A(5,0)=T(1)
	C=(Base**7)*IN+(Base**6)*IN+(Base**5)*IN+(Base**4)*IN+(Base**3)*IN
	PHI=(Base**7)*IM+(Base**6)*IM+(Base**5)*IM+(Base**4)*IM
     &  +(Base**3)*IM
	THE=(Base**6)*IN+(Base**4)*IN
	I=(Base**7)*A(1,0)+(Base**6)*A(2,0)+(Base**5)*A(3,0)
     &  +(Base**4)*A(4,0)+(Base**3)*A(5,0)
	II=(Base**7)*A(5,0)+(Base**6)*A(4,0)+(Base**5)*A(3,0)
     &  +(Base**4)*A(2,0)+(Base**3)*A(1,0)
	GO TO 113
1120	II=(Base**7)*X(A(1,0))+(Base**6)*X(A(2,0))+(Base**5)*X(A(3,0))
     &  +(Base**4)*X(A(4,0))+(Base**3)*X(A(5,0))
	GO TO 113
1140	II=(Base**7)*X(A(5,0))+(Base**6)*X(A(4,0))+(Base**5)*X(A(3,0))
     &  +(Base**4)*X(A(2,0))+(Base**3)*X(A(1,0))
	K=-1
        GO TO 113

c  need to be careful with k and the, unclear from pattern how these should be modified exactly
10002	K=8
c        write(*,*) 'W', W  
c        write(*,*) '',''
c end testing code
	A(1,0)=H(1)
	A(2,0)=H(2)
        A(3,0)=H(3)
	A(4,0)=T(3)
	A(5,0)=T(2)
        A(6,0)=T(1)
	C=(Base**7)*IN+(Base**6)*IN+(Base**5)*IN+(Base**4)*IN+(Base**3)*IN
     &  +(Base**2)*IN
	PHI=(Base**7)*IM+(Base**6)*IM+(Base**5)*IM+(Base**4)*IM
     &  +(Base**3)*IM+(Base**2)*IM
	THE=(Base**6)*IN+(Base**4)*IN+(Base**2)*IN
	I=(Base**7)*A(1,0)+(Base**6)*A(2,0)+(Base**5)*A(3,0)
     &  +(Base**4)*A(4,0)+(Base**3)*A(5,0)+(Base**2)*A(6,0)
	II=(Base**7)*A(6,0)+(Base**6)*A(5,0)+(Base**5)*A(4,0)
     &  +(Base**4)*A(3,0)+(Base**3)*A(2,0)+(Base**2)*A(1,0)
	GO TO 113
1119	II=(Base**7)*X(A(1,0))+(Base**6)*X(A(2,0))+(Base**5)*X(A(3,0))
     &  +(Base**4)*X(A(4,0))+(Base**3)*X(A(5,0))+(Base**2)*X(A(6,0))
	GO TO 113
1139	II=(Base**7)*X(A(6,0))+(Base**6)*X(A(5,0))+(Base**5)*X(A(4,0))
     &  +(Base**4)*X(A(3,0))+(Base**3)*X(A(2,0))+(Base**2)*X(A(1,0))
	K=-1
	GO TO 113

10003	K=10
c        write(*,*) 'W', W  
c        write(*,*) '',''
c end testing code
	A(1,0)=H(1)
	A(2,0)=H(2)
        A(3,0)=H(3)
	A(4,0)=H(4)
	A(5,0)=T(3)
	A(6,0)=T(2)
        A(7,0)=T(1)
	C=(Base**7)*IN+(Base**6)*IN+(Base**5)*IN+(Base**4)*IN+(Base**3)*IN
     &  +(Base**2)*IN+(Base**1)*IN
	PHI=(Base**7)*IM+(Base**6)*IM+(Base**5)*IM+(Base**4)*IM
     &  +(Base**3)*IM+(Base**2)*IM+(Base**1)*IM
	THE=(Base**6)*IN+(Base**4)*IN+(Base**2)*IN+(Base**0)*IN
	I=(Base**7)*A(1,0)+(Base**6)*A(2,0)+(Base**5)*A(3,0)
     &  +(Base**4)*A(4,0)+(Base**3)*A(5,0)+(Base**2)*A(6,0)+Base*A(7,0)
	II=(Base**7)*A(7,0)+(Base**6)*A(6,0)+(Base**5)*A(5,0)
     &  +(Base**4)*A(4,0)+(Base**3)*A(3,0)+(Base**2)*A(2,0)+Base*A(1,0)
	GO TO 113
1118	II=(Base**7)*X(A(1,0))+(Base**6)*X(A(2,0))+(Base**5)*X(A(3,0))
     &  +(Base**4)*X(A(4,0))+(Base**3)*X(A(5,0))+(Base**2)*X(A(6,0))
     &  +(Base**1)*X(A(7,0))	
	GO TO 113
1138	II=(Base**7)*X(A(7,0))+(Base**6)*X(A(6,0))+(Base**5)*X(A(5,0))
     &  +(Base**4)*X(A(4,0))+(Base**3)*X(A(3,0))+(Base**2)*X(A(2,0))
     &  +(Base**1)*X(A(1,0))
	K=-1
	GO TO 113

C
C END OF V.FLIPS,H.FLIPS AND VH.FLIPS
C
C
C MAKING THE ARRAY S-TYPE
C

c this section mnakes array lexiographically least given previous transformations, with C, phi, theta operations, gets applied after v, h, vh operations
113	Q=AND(II,III)
	P=AND(II,IIII)
	XX=4
	IF (Q.GE.F) XX=3
	IF (Q.GE.G) XX=2
	IF (Q.GE.FF) XX=1
	Y=4
	IF (P.GE.GG) Y=0
	Z=XX+Y
	GO TO (10,20,30,40,50,60,70,80),Z
10	J=XOR(II,C)
	GO TO 800
20	NN=XOR(C,PHI)
	J=XOR(II,NN)
	GO TO 800
30	NN=XOR(THE,PHI)
	J=XOR(II,NN)
	GO TO 800
40	J=XOR(II,THE)
	GO TO 800
50	NN=XOR(C,THE)
	J=XOR(II,NN)
	GO TO 800
60	NN=XOR(C,THE)
	NNN=XOR(NN,PHI)
	J=XOR(II,NNN)
	GO TO 800
70	J=XOR(II,PHI)
	GO TO 800

80	J=II
	RU3=R3+U1
	RC3=AND(RU3,MASK1)
	IF (RC3.GT.0)	GO TO 2

c compares if transformed J is less than I
c code should only go to change the array if not lex least if at the maximum width, otherwise should test if current numbers satisfy sidelobe condition and keep building, 
c criteria that lex least can only be used on final set of arrays, because applying this condition to smaller matrices can cut off lex least matrices at the full size

c800     write(*,*) 'I', I
c	write(*,*) 'J', J

800	IF ((I.GT.J).AND.(LMAX.EQ.W)) GO TO 2
	K=K+1
c only sets k=-1 if has gone through all possibilities, then k will equal 0 at this point if finds lexographically least, so can go on to autocorrelation test
	IF(K.EQ.0)	GO TO 777  
	GO TO (1123,1143,1122,1142,1121,1141,1120,1140,
     &  1119,1139,1118,1138),K
c111	GO TO 2
C
C ARRAY GENERATED IS LEX. LEAST IN ITS EQUIVALENCE CLASS.
C GENERATION OF VALUES OF ALL SHIFTS OF ROWS
C

c generate shifts for different tau for tail check, either shift bits left or right, and then feed into correlation function in next section      

777	DO 1000 TAU=1,N-1
	DO 2000 J=1,W
	A(J,TAU)=(A(J,TAU-1)-1)/2
	B=A(J,TAU-1)/2
	IF (2*B.EQ.A(J,TAU-1))	A(J,TAU)=B
	A(J,-TAU)=A(J,-TAU+1)-(2**(N-TAU))
	IF (A(J,-TAU).LT.0) A(J,-TAU)=A(J,-TAU+1)
2000	CONTINUE
1000	CONTINUE

C
C BEGIN CORRELATION TESTING
C
335     RC1=0

c correlation is maximum of 49*2=98, so need 7 bits to store information
c Cor is pregenerated function which takes shifted rows and determines the correlation from these, the total overlap is determined by looping over overlap rows. 
	D=W/2

c select based on word size, autocorrelation scaled from 0 to 2*N*W, add V, and subtract VV to see if has reached maximum sidelobe specified
3	V=(2**7-1)-((W-D)*N+CORMAX)
	VV=((W-D)*N-CORMAX)
	S=1
        Base2=2**7


c code will keep modifying D as goes through sections, only when D have gone to -1 will it be able to break out and print the value of the array which satisfies the conditions
c need S=0 when D=0 so will not include 1st byte of mask1, so will not misdetect main peak as something exceeding the sidelobe
338	IF (D.EQ.0)	S=0
c        write(*,*) 'Dimension', D

c mask is used to pick up overflow bits, place so that at end of 8 bit allocation, so 7, 15, 23, etc. 
	MASK1=(Base**6)*S*Base2+(Base**5)*Base2
     &  +(Base**4)*Base2+(Base**3)*Base2
     &  +(Base**2)*Base2+(Base**1)*Base2+Base2

	R1=0
c	write(*,*) 'hello',''
	DO 3000 Z=0,N-1
	SUM1=0
	DO 2999 J=1,W-D
	KK1=(Base**(6-Z))*(COR(A(J,Z),A(J+D,-Z))-Z)
c	SUM1=COR(A(J,Z),A(J+D,-Z))-Z+SUM1
c	write(*,*) 'Cor',COR(A(J,Z),A(J+D,-Z))-Z
c	write(*,*) J,Z
c	write(*,*) 'A',A(J,0)
	R1=R1+KK1
2999    CONTINUE
c	write(*,*) Z,SUM1
3000	CONTINUE
c	write(*,*) 'out',''

c define the word this way so we don't have to worry about changing when we change matrix parameters
	U1=0
	DO 2998 Z=0,N-1
	U1=(Base**(6-Z))*V+U1
2998    CONTINUE
	RU1=R1+U1
	RC1=AND(RU1,MASK1)

c        write (*,*) 'RC1', RC1
c        write (*,*) 'R1', R1
c	write (*,*) 'D', D

c       write (*,*) 'G1', G1
c        write (*,*) 'G2', G2
c        write (*,*) 'G3', G3


334	IF (RC1.GT.0)	GO TO 2

	L1=0
	DO 3998 Z=0,N-1
	L1=(Base**(6-Z))*VV+L1
3998    CONTINUE
	RL1=R1-L1
	RC3=AND(RL1,MASK1)

c        write (*,*) 'RL3', RC3

336	IF (RC3.GT.0)	GO TO 2
c        PRINT *,"LINE224: R13=",R13,"D=",D
c     only goes to 707 after tail check and middle check are complete, i.e. d=0, each time goes through decrements D, starts with D equal to W/2
c     we don't need to go past this point because at D=0, second operation should be equivalent anyways
	IF (D.EQ.0)	GO TO 707
	R2=0
	DO 4000	J=1,W-D
	DO 3999 Z=0,N-1
	KK2=(Base**(6-Z))*(COR(A(J,-Z),A(J+D,Z))-Z)
	R2=R2+KK2
3999    CONTINUE
4000	CONTINUE
	U2=U1
	RU2=R2+U2
	RC2=AND(RU2,MASK1)
       
c        write (*,*) 'RC2', RC3
	
	IF (RC2.GT.0)	GO TO 2
	L2=L1
	RL2=R2-L2
	RC2=AND(RL2,MASK1)

c        write (*,*) 'RL2', RC2

	IF (RC2.GT.0)	GO TO 2
	D=D-1
c    only print out if 1st entry is given by 1

c       

c    D gets decremented every pass, after tail check and middle check is completed, breaks out and goes to 707 after checking with D=0 above
340	IF (D.GE.0)	GO TO 3

c2221    write(*,*) 'Output rejected', '' 



c will never pass through here, exits midway through this section through a goto to 707

c we want to isolate problem values

C END CORRELETION TESTING.
C
C
C COMPUTATION OF DOUBLY-APERIODIC AUTOCORRELATION
C OF ARRAY TO BE PRINTED.
C
c707	
C
C WRITE ROUTINE OF ARRAY AND ITS AUTOCORRELATION.
C

707	IF (W.NE.LMAX) GO TO 2
	COUNT1=COUNT1+1
	GO TO (17,17,18,19, 9000,9001),W

9001    WRITE (M,1109) (A(I,0),I=1,W)
1109	FORMAT (5X,I3,5X,I3,5X,I3,5X,I3,5X,I3,5X,I3)
	GO TO 2
9000    WRITE (M,1110) (A(I,0),I=1,W)
1110	FORMAT (5X,I3,5X,I3,5X,I3,5X,I3,5X,I3)
	GO TO 2
19	WRITE (M,1111) (A(I,0),I=1,W)
1111	FORMAT (5X,I3,5X,I3,5X,I3,5X,I3)
	GO TO 2
18	WRITE (M,1112) (A(I,0),I=1,W)
1112	FORMAT (5X,I3,5X,I3,5X,I3)
	GO TO 2
17      WRITE (M,1113) (A(I,0),I=1,W)
1113	FORMAT (5X,I3,5X,I3)
	GO TO 2

c707	DO 4444 D=0,W-1
c	YY=1-N
c	IF (D.EQ.0)	YY=0
c	DO 3333 TAU=YY,N-1
c	R(D,TAU)=0
c	DO 2222 I=1,W-D
c	R(D,TAU)=R(D,TAU)+((COR(A(I,-TAU),A(I+D,TAU))-ABS(TAU))-N)
c2222	CONTINUE
c3333	CONTINUE
c4444	CONTINUE
C
C WRITE ROUTINE OF ARRAY AND ITS AUTOCORRELATION.
C
c	PRINT *,"LINE 277"
c	GO TO (17,17,18,19, 9000),W


c9000    WRITE (M,1110) (A(I,0),I=1,W),(R(0,TAU),TAU=0,N-1),
c     *	((R(D,TAU),TAU=1-N,N-1),D=1,W-1)
c1110	FORMAT (5X,I3,5X,I3,5X,I3,5X,I3,5X,I3,8X,59I3)
c	GO TO 5555
c19	WRITE (M,1111) (A(I,0),I=1,W),(R(0,TAU),TAU=0,N-1),
c     *	((R(D,TAU),TAU=1-N,N-1),D=1,W-1)
c1111	FORMAT (5X,I3,5X,I3,5X,I3,5X,I3,16X,46I3)
c	GO TO 5555
c18	WRITE (M,1112) (A(I,0),I=1,W),(R(0,TAU),TAU=0,N-1),
c     *	((R(D,TAU),TAU=1-N,N-1),D=1,W-1)
c1112	FORMAT (5X,I3,5X,I3,5X,I3,24X,33I3)
c	GO TO 5555
c17	AB=(2**N)-1-A(2,0)	
c	IF (AB.LT.A(2,0)) 	GO TO 2
c	WRITE (M,1113) (A(I,0),I=1,W),(R(0,TAU),TAU=0,N-1),
c     *	((R(D,TAU),TAU=1-N,N-1),D=1,W-1)
c1113	FORMAT (5X,I3,5X,I3,32X,20I3)
C

C END OF WRITE ROUTINE.
C
C
C BACKTACKING	OR FORWARDING STARTS HERE.
C
2	IF ((2*D).EQ.W)	GO TO 4
5	IF (W.EQ.LMAX)	GO TO 6
	IF (NH.EQ.NT)	GO TO 7
	NT=NT+1
	T(NT)=0
	GO TO 8
7	NH=NH+1
	H(NH)=0
	GO TO 8
6	IF (NH.EQ.NT)	GO TO 4
12	X1=(3-NH)
	IF (X1.LT.0)	X1=0
	IF (H(NH).LT.(2**(N-X1))-1)	GO TO 9
	NH=NH-1
4	IF (T(NT).EQ.(2**N)-1)	GO TO 13
	T(NT)=T(NT)+1
	GO TO 8
9	H(NH)=H(NH)+1
	GO TO 8
13	IF (NT.EQ.1)	GO TO 11
	NT=NT-1
	GO TO 12
11	X2=(3-NH)
	IF (X2.LT.0)	X2=0
	IF (H(NH).EQ.(2**(N-X2))-1)	GO TO 5555
	H(NH)=H(NH)+1
	T(NT)=H(NH)
	GO TO 8
C
C BACKTRACKING 	OR FORWARDING ENDS HERES.
C
5555    WRITE(*,*) Base**(2),'base'    
	WRITE(*,*) 'Final count',COUNT1	
	CLOSE(M)
	STOP
	END PROGRAM
