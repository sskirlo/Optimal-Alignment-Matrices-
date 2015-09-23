
	PROGRAM modSABAHGen2

c integer*8 are new 64 bit words which will be able to handle 7 by 7 matrices, not available when code was originally written

	IMPLICIT INTEGER(KIND=8) (A-Z)
        INTEGER*8 I, II, Mask1, KK1, KK2, COUNT1, SWIT
	DIMENSION COR(0:127,0:127),A(20,-7:7),JJ(7),
     &  X(0:127),H(4),T(4),R(0:7,-6:6)
	M=1
	COUNT1=0
c if want program to end after executing a particular matrix, set Swit to 5555, else use 2
	OPEN (M,FILE='RESULT.TXT')

c for 5*5 search range should be 13,19, mindistance guess is 8

c need to use following three criteria to weed out matrices in the back track search, along with the symmetry considerations
	MAXONES=13
	MINONES=8
	MINDISTANCE=4
	CORMAX=6

c need to initialize all initial test words to zero
	DO 683 K=1,4
	H(K)=0
	T(K)=0
683	CONTINUE


	WRITE(*,*) 'Starting Optimal Distance',MINDISTANCE
	WRITE(*,*) 'Starting Maximum Sidelobe',CORMAX

	LMAX=4
	N=4
        Base=2**8
	Base2=2**7

c mask anded with autocorrelation word to extract whether or not exceeded peak in 8th bit of each part
	MASK1=(Base**6)*1*Base2+(Base**5)*Base2
     &  +(Base**4)*Base2+(Base**3)*Base2
     &  +(Base**2)*Base2+(Base**1)*Base2+Base2
c mask2 is used for testing with no vertical shift, so don't want autocorrelation peak to trigger sidelobe backtrack
	MASK2=(Base**6)*0*Base2+(Base**5)*Base2
     &  +(Base**4)*Base2+(Base**3)*Base2
     &  +(Base**2)*Base2+(Base**1)*Base2+Base2
c V is added to see if exceeds maximum autocorrelation
	V=(2**7-1)-CORMAX
       

	WRITE (M,677) LMAX,N,CORMAX
677	FORMAT (2X,'ROWS=',I2,3X,'COLUMNS=',I2,3X,'CORMAX=',I2)
	WRITE (M,678)
678	FORMAT (4X,'ROW1',3X,'ROW2',3X,'ROW3',3X,'ROW4')


c a highly ineffecient way to compute correlation matrix
c each time have incidence, add one to correlation sum
	
	DO 679 I=0,((2**7)-1)
	DO 680 J=0,((2**7)-1)
c	DO 680 J=30
	I1=I
	J1=J
	COR(I,J)=0
	DO 681 K=0,7
	IN1=0
	IN2=0
	IF((I1-2**(7-K)).LT.0) GOTO 682
	IN1=1
	I1=I1-2**(7-K) 
682	IF((J1-2**(7-K)).LT.0) GOTO 681
	IN2=1
	J1=J1-2**(7-K) 
	IF((IN1.NE.IN2).OR.(IN2.NE.(1))) GOTO 681
	COR(I,J)=COR(I,J)+1
681     CONTINUE
680	CONTINUE
679	CONTINUE

c testing
c	write(*,*) COR(0,0),COR(127,127)
c	write(*,*) COR(127,8),COR(8,127)
c	write(*,*) COR(9,63),COR(63,9)
c	GOTO 5555

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


C
C	EXTRACTION AND TEST WORDS
C	
C	INITIALIZING THE FIRST HEAD AND TAIL
C
	NH=1
	NT=1
	H(NH)=0
	T(NT)=0
8	W=NH+NT 

c	write(*,*) T(2),''
c	IF(NT.GT.(NH+1)) GOTO 5555
c	GO TO 2

c        W=3
c        H(1)=2
c        H(2)=20
c	T(1)=19
c	T(1)=15

c	SWIT=0
c	if(H(1).EQ.(2)) GOTO 11111
c	GOTO 11110
c11111	if(H(2).EQ.(17)) GOTO 11112
c	GOTO 11110
c11112	if(T(1).EQ.(22)) GOTO 11113
c	GOTO 11110
c11113	if(T(1).EQ.(15)) GOTO 11114
c	GOTO 11110
c11114   SWIT=1
c	write(*,*) 'Have match'
c11114	if(T(1).EQ.(31)) GOTO 11115
c	GOTO 11110
c11113   write(*,*) 'Have match'
c	SWIT=1

c   switches based on width, need to specify switch for all possiblities
11110	GO TO (14,14,15,16,10001,10002,10003),LMAX

c these create word I, and generate lexiographically least of v,vh,and h, by making result lex least by going to 113 and testing for all 3 operations
14	K=0
	A(1,0)=H(1)
	A(2,0)=T(1)
c we only need to test full lex least at end of the code
	IF(W.NE.LMAX) GOTO 777
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
	IF(W.NE.LMAX) GOTO 777
	I=(Base**7)*A(1,0)+(Base**6)*A(2,0)+(Base**5)*A(3,0)
	II=(Base**7)*A(3,0)+(Base**6)*A(2,0)+(Base**5)*A(1,0)
	GO TO 113
1122	II=(Base**7)*X(A(1,0))+(Base**6)*X(A(2,0))+(Base**5)*X(A(3,0))
	GO TO 113
1142	II=(Base**7)*X(A(3,0))+(Base**6)*X(A(2,0))+(Base**5)*X(A(1,0))
	K=-1
	GO TO 113

16	K=4
	A(1,0)=H(1)
	A(2,0)=H(2)
	A(3,0)=T(2)
	A(4,0)=T(1)
	IF(W.NE.LMAX) GOTO 777
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
	IF(W.NE.LMAX) GOTO 777
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
	IF(W.NE.LMAX) GOTO 777
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
	IF(W.NE.LMAX) GOTO 777
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

c compares if transformed J is less than I
c criteria that lex least can only be used on final set of arrays, because applying this condition to smaller matrices can cut off lex least matrices at the full size

113	IF ((I.GT.II).AND.(LMAX.EQ.W)) GO TO 2
	K=K+1
	IF(K.EQ.0)	GO TO 777  
	GO TO (1123,1143,1122,1142,1121,1141,1120,1140,
     &  1119,1139,1118,1138),K

c need to determine number of ones (autocorrelation peak) in matrix so can calculate maxdistance and see if the number of ones has exceeded max ones
c can also do a minimum check and make sure matrix being built up is on track to be in optimal range

777	R1=0
        DO 706 J=1,W
	KK1=COR(A(J,0),A(J,0))
	R1=R1+KK1
706     CONTINUE
	ONES=R1

c generate shifts for different tau for tail check, either shift bits left or right, and then feed into correlation function in next section      

	DO 1000 TAU=1,N-1
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
	D=W/2
	MASK=MASK1
	PASS=1

c	write(*,*) 'hello',''
c	IF (ONES.GT.MAXONES) GOTO 2
c	IF (ONES.LT.(MINONES-N*(LMAX-W))) GOTO 2
c	write(*,*) 'peak ones',ONES
	
3	IF (D.EQ.0)  MASK=MASK2

c mask is used to pick up overflow bits, place so that at end of 8 bit allocation, so 7, 15, 23, etc. 

	R1=0
	DO 3000 Z=0,N-1
	DO 2999 J=1,LMAX-D
	KK1=(Base**(6-Z))*(COR(A(J,Z),A(J+D,-Z)))
	R1=R1+KK1
2999    CONTINUE
3000	CONTINUE

	U1=0
	DO 2998 Z=0,N-1
	U1=(Base**(6-Z))*V+U1
2998    CONTINUE
	RU1=R1+U1
	RC1=AND(RU1,MASK)

	IF (RC1.GT.0)	GO TO 5000

c     only goes to 707 after tail check and middle check are complete, i.e. d=0, each time goes through decrements D, starts with D equal to W/2
c     we don't need to go past this point because at D=0, second operation should be equivalent anyways, here we are testing with horizontal shifts in opposite direction
	IF ((D.EQ.(0)).AND.(W.EQ.LMAX))	GO TO 707
	IF (D.EQ.(0)) GO TO 2
	R2=0
	DO 4000	J=1,LMAX-D
	DO 3999 Z=0,N-1
	KK2=(Base**(6-Z))*(COR(A(J,-Z),A(J+D,Z)))
	R2=R2+KK2
3999    CONTINUE
4000	CONTINUE
	U2=U1
	RU2=R2+U2
	RC2=AND(RU2,MASK)
     
	IF (RC2.GT.0)	GO TO 5000

c D gets decremented every pass, after tail check and middle check is completed, breaks out and goes to 707 after checking with D=0 above	
c it is pointless to do a middle check on matrix before fully formed because correlation here does not represent correlation of final matrix unlike tailcheck, 
c if we pass the tailcheck all we know that we can keep building
	D=D-1
	IF (D.GE.0)	GO TO 3

5000	PASS=1
	GO TO 2


C END CORRELETION TESTING.
C

c707	write(*,*) 'Printing'


c computes matrix autocorrelation, need to determine here whether matrix has sufficient distance to be included in output file, otherwise will get swamped

707	MAXSIDE=0
	DO 4444 D=0,W-1
	YY=1-N
	IF (D.EQ.0)	YY=0
	DO 3333 TAU=YY,N-1
	R(D,TAU)=0
	DO 2222 I=1,W-D
	R(D,TAU)=R(D,TAU)+COR(A(I,-TAU),A(I+D,TAU))
2222	CONTINUE
	IF((D.EQ.(0)).AND.(TAU.EQ.(0))) GOTO 3333
	IF(R(D,TAU).GT.MAXSIDE) MAXSIDE=R(D,TAU)
3333	CONTINUE
C	IF(SWIT.EQ.0) GOTO 4444
C	write(*,*) '',R(D,TAU)
4444	CONTINUE
c	write(*,*) 'Maximum sidelobe',MAXSIDE

c here we have conditions which will determine whether matrix will get printed out, make bounds tighter if these are better than 1st guess, want to only print out optimal matrices
c do not need to set Pass=0 here because already at maximum size, this will automatically cause backtracking condition
c	write(*,*) 'hello',''
c	write(*,*) 'ones',ONES
c	write(*,*) 'MAXSIDE',MAXSIDE
c	IF(SWIT.EQ.(1)) GOTO 9002
	IF((ONES-MAXSIDE).LT.MINDISTANCE) GOTO 2
	IF(ONES.LT.MINONES) GOTO 2
	IF(ONES.GT.MAXONES) GOTO 2
	IF((ONES.EQ.MAXONES).AND.(MAXSIDE.LT.CORMAX)) CORMAX=MAXSIDE
        IF((ONES-MAXSIDE).EQ.MINDISTANCE) GOTO 9003
c if greater than mindistance, create new mindistance
	write(*,*) 'New mindistance',ONES-MAXSIDE
	MINDISTANCE=(ONES-MAXSIDE)	
	COUNT1=0
	GOTO 9003	

c9002	WRITE(*,*) 'Ones',ONES
c	WRITE(*,*) 'MAXSIDE',MAXSIDE
	

C
c WRITE ROUTINE OF ARRAY AND ITS AUTOCORRELATION.
C
c	PRINT *,"LINE 277"
c9001	COUNT1=COUNT1+1	
c	GO TO (17,17,18,19, 9000),W
c
c9000    WRITE (M,1110) (A(I,0),I=1,W),(R(0,TAU),TAU=0,N-1),
c     *	((R(D,TAU),TAU=1-N,N-1),D=1,W-1)
c1110	FORMAT (5X,I3,5X,I3,5X,I3,5X,I3,5X,I3,8X,59I3)
c	GO TO 2
c19	WRITE (M,1111) (A(I,0),I=1,W),(R(0,TAU),TAU=0,N-1),
c     *	((R(D,TAU),TAU=1-N,N-1),D=1,W-1)
c1111	FORMAT (5X,I3,5X,I3,5X,I3,5X,I3,16X,46I3)
c	GO TO 2
c18	WRITE (M,1112) (A(I,0),I=1,W),(R(0,TAU),TAU=0,N-1),
c     *	((R(D,TAU),TAU=1-N,N-1),D=1,W-1)
c1112	FORMAT (5X,I3,5X,I3,5X,I3,24X,33I3)
c	GO TO 2
c17	AB=(2**N)-1-A(2,0)	
c	IF (AB.LT.A(2,0)) 	GO TO 2
c	WRITE (M,1113) (A(I,0),I=1,W),(R(0,TAU),TAU=0,N-1),
c     *	((R(D,TAU),TAU=1-N,N-1),D=1,W-1)
c1113	FORMAT (5X,I3,5X,I3,32X,20I3)
C
9003	COUNT1=COUNT1+1
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


C END OF WRITE ROUTINE.
C
C
C BACKTACKING	OR FORWARDING STARTS HERE.
C

c2       write(*,*) 'output rejected', ''
c	GO TO 5555

cc if val=1 didn't satisfy sidelobe condition, we need to stop going down, and keep incrementing at current level if possible
2	write(*,*) '',''  
	write(*,*) NH,NT
	write(*,*) H(1),''
	write(*,*) T(1),''
	write(*,*) H(2),''
	write(*,*) T(2),''
	write(*,*) 'Pass',PASS
	IF(H(1).EQ.(2)) GO TO 5555

	IF (PASS.EQ.(0)) GO TO 6
	IF (W.EQ.LMAX)	GO TO 6
	IF (NH.EQ.NT)	GO TO 7
	NT=NT+1
	T(NT)=0
	GO TO 8
7	NH=NH+1
	H(NH)=0
	GO TO 8
c need to decide what last one added was, if odd, last row is NH, if even NT, 
6	IF (NH.NE.NT) GO TO 13
c condition right below here is NT last row added, if (NH+NT) even
	IF (T(NT).EQ.((2**N)-1)) GO TO 12
	T(NT)=T(NT)+1
c if last row already full, go back up one level, need to increment next level up
	GO TO 8	
12	T(NT)=0
	NT=NT-1
	GO TO 6
13	IF (H(NH).EQ.((2**N)-1)) GO TO 11
	H(NH)=H(NH)+1
c we can't test NH=1 condition, need at least 2
	GO TO 8	
11	H(NH)=0		
	NH=NH-1
	IF(NH.EQ.(1)) GOTO 6
c if tries to go back b/c 1st row is already full, after filling up all previous rows, has finished going through all possiblities
c	write(*,*) '','bye'
	IF(NH.EQ.(0)) GOTO 5555
	GO TO 6

C
C BACKTRACKING 	OR FORWARDING ENDS HERES.
C
5555    WRITE(*,*) 'Final count',COUNT1	
	WRITE(*,*) 'Optimal Distance',MINDISTANCE
	WRITE(*,*) 'Maximum Sidelobe',CORMAX
	CLOSE(M)
	STOP
	END PROGRAM
