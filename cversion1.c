
#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
#include "mpi.h"

#define	MAXONES 20
#define MINONES 13
#define MINDISTANCE 4
#define LMAX 5
#define N 5
#define MID N-1
#define BASE 256
#define BASE2 128

//prints out binary representation of word
int printlong(unsigned long long x)
{
	//unsigned int test=31; 	
	//printf("%d \n",test<<=1); 

	int n; 
	int bits[64]; 
	for(n=0; n<64; n++)
	{
		bits[n]=x&1; //and with 1
		x>>=1;        //keep shifting left
	}
	for(n=63; n>=0; n--)
	{
		if((n+1)%8==0)
			printf(" "); 
		printf("%d",bits[n]); 	
	}
	printf("\n"); 
}
int printshort(unsigned short int x)
{
	int n; 
	int bits[16]; 
	for(n=0; n<16; n++)
	{
		bits[n]=x&1; //and with 1
		x>>=1;        //keep shifting left
	}
	for(n=15; n>=0; n--)
	{
		printf("%d",bits[n]); 	
	}
	printf("\n"); 
}
//should be able to print array with limited dimensions so clearer
int printarray(unsigned short int array[7][13],int shift)
{
	int n,n1;
	int bits[LMAX]; 
	unsigned short int x; 
	for(n=0; n<N; n++)
		printf("%d ",array[n][shift]); 
	printf("\n"); 
	//print out section of matrix that isn't zero padded
	for(n1=0; n1<N; n1++)
	{
		x=array[n1][shift]; 
		for(n=0; n<LMAX; n++)
		{
			bits[n]=0; 
			bits[n]=x&1; //and with 1
			x>>=1;        //keep shifting left
		}
		for(n=LMAX-1; n>=0; n--)
		{
			printf("%d",bits[n]); 	
		}
		printf("\n"); 
	}
	/*for(n1=0; n1<N; n1++)
	{
		printshort(array[n1][shift]); 
	}*/
}
int main(int argc, char *argv[])
{
	int numprocs, myrank;
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &myrank); 


	unsigned long long mask1,mask2,mask,testword,result1,result2;
	unsigned long long corr[8][8];//have different depth, but also different vertical shifts 
	unsigned long long corl[8][8]; 	
	unsigned int optnum,total,total2; 
	short int n,n2,n3,level;  
	unsigned short int part,res,sum,max,pass,sum2,sidelobe,ones,startx,starty,swit,ref,inc,start,end; 
	unsigned short int cor[128][128]; 
	unsigned short int peak[128]; //peak stores number of ones in given word, instant lookup
	unsigned short int flips[128]; 
	unsigned short int shift[128][13]; 
	unsigned short int array[7][13]; 
	unsigned short int onels[8];
	unsigned short int lexleast[8]; 
	unsigned short int stepx[(2*N-1)*LMAX-1];  
	unsigned short int stepy[(2*N-1)*LMAX-1];  
	unsigned long long base8s [7]; 
	unsigned short int optdist=MINDISTANCE; //set as variable so can be tightened as search progresses 
	FILE * fp; 
	FILE * outfile; //print all information to this file about run statistics, etc
	FILE * opt; 
	char output[40]; 
	sprintf(output,"result%d.txt",myrank); 	
	if(fopen("outfile.txt","r")==NULL)
	{
		outfile=fopen("outfile.txt","w"); 
	}
	else
	{
		outfile=fopen("outfile.txt","a"); 
	}	
	if(fopen("optfile.txt","r")==NULL)
	{
		opt=fopen("optfile.txt","w");
		fprintf(opt,"%d \n",optdist); 
		fclose(opt);  
	}
	

	fp=fopen(output,"w");
	//printf("start optimal distance %d \n",optdist); 
	//printf("maxsidelobe is %d \n",MAXONES-optdist); 
	
	//define a lookup table for base shifts
	base8s[0]=1; 
	total=0; 
	total2=0; 
	optnum=0; //variable which counts number of (unsorted) optimal matrices
	sum=127;
	ones=sum>>=(7-N);
	max=ones; 
	//printf("max per row %d \n",ones);
	//printlong(base8s[0]);   
	for(n=1; n<=7; n++)
	{
		base8s[n]=base8s[n-1]*BASE; 
		//printlong(base8s[n]); 	
	}
	
	//create sequence of steps to search autocorrelation 1st for entries which are most likely to cause failure
	
	n3=0; 
	for(n=1;n<=(N-1);n++) //n is the 'radius'	
	{
		for(n2=0; n2<(2*n+(2*n+1)); n2++) //increment over primeter
		{
			//determine difference from start point and add along in direction
			//can mod out parts of perimeter to determine what leg along
			if(n2/(n+1)<1) //1st vertical leg
			{
				startx=n+MID; 
				starty=0; 
				stepx[n3]=startx; 
				stepy[n3]=starty+n2; 
			}
			else if(n2/(n+1)>=1 && n2/(n+2*n)<1) //2nd horizontal leg
			{
				startx=n+MID; 
				starty=n; 
				stepx[n3]=startx-(n2-n); 
				stepy[n3]=starty; 
			}
			else //last leg
			{
				startx=-n+MID; 
				starty=n; 
				stepx[n3]=startx; 
				stepy[n3]=starty-(n2-n-(2*n)); 
			}
			n3++; 
		}
	}
	/*for(n=0; n<((2*N-1)*LMAX-1); n++)
	{
		printf("%d %d \n",stepx[n],stepy[n]); 
	}*/

	//printf("flips \n"); 
	//generate flip lookup tables
	for(inc=0; inc<=max; inc++)
	{
		flips[inc]=0; 
		ref=1<<(N-1); 
		for(n2=0; n2<N; n2++)
		{
			res=1<<n2; 
			if((res&inc)>0)
			{ 
				res=ref>>(n2); 
				flips[inc]=flips[inc]+res;
			} 
		}
		//printshort(inc);
		//printshort(flips[inc]); 
	}	

	//define masks for testing correlation
	mask1=0; 
	mask2=0; 
	for(n=0;n<N;n++)
	{
		mask1=base8s[7-n]*BASE2+mask1;
		if(n!=6)
			mask2=mask2=base8s[7-n]*BASE2+mask2;  	
	}
	//these are test words for checking autocorrelation, we need to update these every time we change cormax
	part=(BASE2-1)-(MAXONES-optdist); 
	testword=0; 
	for(n=0; n<N; n++)
	{
		testword=base8s[7-n]*part+testword; 
	}
	
	//generate correlation lookup table
	for(n=0; n<128; n++)
	{
		for(n2=0; n2<128; n2++)
		{
			//just and together and count the number of ones
			res=n&n2; 
			sum=0; 
			for(n3=0;n3<7;n3++)
			{
				sum2=res&1; //need to seperate otherwise doesn't work
				sum=sum2+sum;  
				res>>=1;
			}
			cor[n][n2]=sum;
			if(n==n2)
			{
				peak[n]=sum; //lookup table for the number of ones
			}  
		}
	}

	//generate shift lookup table, (need to and with 7 1s to clear out shifts to the left, out of range of correlation result)
	for(n=0; n<128; n++)
	{		
		for(n2=-N+1; n2<N; n2++)
		{
			sum=n; 
			if(n2<0)
				sum>>=(-n2); 
			else
				sum<<=(n2); 		
			sum=sum&ones; //do this to make sure doesn't go outside correlation arguments
			shift[n][n2+N-1]=sum; 
		}
	}
			
	//enter loop, recursive 
	level=0; 
	for(n=0; n<7;n++)
	{
		array[n][MID]=0; //initialize to 0
		onels[n]=0; 	
		lexleast[n]=0; //tells that as default should check horizontal flips 
		for(n2=0; n2<7; n2++)
		{
			corr[n][n2]=0; 
			corl[n][n2]=0;
		} 
	}
	lexleast[0]=1; //set so will check horizontal flip 1st run through, will determine what to do for next cycles

	//can initialize wherever we want for debugging
	/*array[0][MID]=15;
	array[0][0]=8; 
	array[0][1]=12;
	array[0][2]=14;  
	array[0][4]=7; 
	array[0][5]=3; 
	array[0][6]=1; 
	 
	level=1;*/ 

	//printarray(array,MID); //N corresponds to middle shift 

	fprintf(outfile,"starting %d \n",myrank); 	

	//comes here every time goes down level (successively tested previous row)
	start:	
	
	if(level!=0)
	{
		start=0; 
		end=max; 
	}
	else
	{
		start=(myrank)*max/numprocs;
		end=(myrank+1)*max/numprocs-1; 
		if((myrank+1)==numprocs) //make sure end closed 
			end=max; 
		fprintf(outfile,"search range is %d to %d for processor %d \n",start,end,myrank);
	}

	//printf("currently at level %d \n",level); 
	//increment array at given level
	for(array[level][MID]=start; array[level][MID]<=end; array[level][MID]++)
	{
		/*if(array[0][MID]==23 && array[1][MID]==9 && array[2][MID]==20)
		{
			printf("match, level %d \n",level);
			printf("lex least variables %d %d %d %d \n",lexleast[0],lexleast[1],lexleast[2],lexleast[3]); 
		} */

		//check if array lex least if was symmetrical under horizontal flips in prior level
		if(lexleast[level]==1)
		{	
			if(flips[array[level][MID]]>array[level][MID])
			{
				lexleast[level+1]=0; //don't need to check again, lex least
			}
			else if(flips[array[level][MID]]==array[level][MID])
			{
				lexleast[level+1]=1; //need to keep checking if lex least as goes down
			}
			else
			{
				goto back; //failed lex least test
			}
		}
		lexleast[level+1]=0; //need to set so gauranteed not to have problem at next level


		//calculate number of ones and see if satifies minimum, maximum bounds
		onels[level+1]=peak[array[level][MID]]+onels[level]; //ones[0] is always zero
		
		//can determine right here whether or not this matrix is in optimal range or can be in the future, min test, max test
		//levels=size-1, we have LMAX rows possible

		if(onels[level+1]>MAXONES) //if have more ones than max, send back immediately
		{
			total2++; 
			goto back;
		} 

		if((onels[level+1]+(LMAX-1-level)*N)<MINONES ) //if can never have enough ones, even if fill in all rows after send back
		{
			total2++; 
			goto back;                             //also covers case of at last row
		}		 



		//printf("%d level %d array at level \n",level,array[level][MID]); 
		//generate shifts of word at current level (-(N-1) to (N-1) mapped to 0 to (2N-1))
		for(n=0; n<MID; n++)
		{
			array[level][n]=shift[array[level][MID]][n];  
		}
		for(n=MID+1; n<(2*N-1); n++)
		{
			array[level][n]=shift[array[level][MID]][n];  
		}

		//test autocorrelation, if fail pass=0
		//n=0 corresponds to left right shifts without vertical shifts, we only need to get on this side not on other
                //also need to make sure don't misdetect peak as a sidelobe, so use mask2

		//just include checking center row as seperate part
		
		corr[level+1][0]=corr[level][0]; 
		for(n3=0; n3<MID; n3++)
		{
			corr[level+1][0]=base8s[7-n3]*cor[array[level][MID]][array[level][n3]]+corr[level+1][0];
		}
		result1=corr[level+1][0]+testword; 
		result2=result1&mask1; 
		if(result2>0)
		{
			total2++; 
			goto back; 
		}
		
		for(n=1; n<=level; n++)
		{  
			corr[level+1][n]=corr[level][n]; 
			for(n3=0; n3<MID; n3++)
			{
				corr[level+1][n]=base8s[7-n3]*cor[array[level][MID]][array[level-n][n3]]+corr[level+1][n];
			}
			result1=corr[level+1][n]+testword; 
			result2=result1&mask1;    
			if(result2>0)
			{
				total2++; 
				goto back;  //keeps looping at current level
			}
			

			corl[level+1][n]=corl[level][n]; 
			for(n3=MID; n3<(2*N-1); n3++)
			{
				corl[level+1][n]=base8s[7-n3+MID]*cor[array[level][n3]][array[level-n][MID]]+corl[level+1][n];
			}
			result1=corl[level+1][n]+testword; 
			result2=result1&mask1;

			if(result2>0)
			{
				total2++; 
				goto back;  //keeps looping at current level
			}		
		}
		

			//printf("passed \n"); 
		if(level<(LMAX-1))
		{
			level=level+1; //only increment level if going down deeper, otherwise should stay at level until runs out of possiblities
			goto start; //go down deeper
		}
		else
		{
			//need to determine peak sidelobe, start in middle first and work outwards
			sidelobe=0;
			//swit=0;
			total++;  
			for(n=0; n<(((2*N-1)*LMAX)-1); n++)
			{ 
				sum=0; 
				for(n2=stepy[n]; n2<=level; n2++)
				{
					sum=cor[array[n2-stepy[n]][stepx[n]]][array[n2][MID]]+sum;
				}
				if(sum>sidelobe)
					sidelobe=sum;
						 
				if((onels[level+1]-sidelobe)<optdist) //once fails stop immediately
					goto back;

				/*if(n>1 && swit==0)
				{	
					swit=1; 
					//total++;
				} */
			}

			//printf("passed sidelobe test \n"); 
			//if gets passed here wasn't rejected because didn't have small enough sidelobes or large enough ones
			
			//check if optdist is less than or better than global optdist and act accordingly
			/*opt=fopen("optfile.txt","r"); 
			fscanf(opt,"%hd",&n); 
			fclose(opt); 
			if(n>=optdist)
			{
				optdist=n; //global variable is better or equivalent
			}
			else  //our variable is better, so we update file
			{
				fprintf(outfile,"updated global variable to %d , procs %d \n",optdist,myrank);
				opt=fopen("optfile.txt","w"); 
				fprintf(opt,"%d \n",optdist); 				
				fclose(opt); 
			}*/

			if(optdist<(onels[level+1]-sidelobe))
			{
				optnum=0; //reset optnum count
				total=0; 
				optdist=onels[level+1]-sidelobe;
				//also reset sidelobe if necessary
				part=(BASE2-1)-(MAXONES-optdist); 
				testword=0; 
				for(n=0; n<N; n++)
				{	
					testword=base8s[7-n]*part+testword; 
				}
			} 
			
			optnum++; 
			//print out and just keep going through current loop
			for(n=0; n<LMAX; n++)
				fprintf(fp,"%d ",array[n][MID]);
			fprintf(fp,"\n ");  
				
		}				
		
		//after goes to previous levels and starts new loops, after these all exhausted comes back to this location
		back:
		;
		
	}
	//if reaches this point exhausted all possibilities at a given level
	
	level=level-1; 
	if(level!=-1)
		goto back; //keep incrementing at previous level
	
	//printf("exiting \n"); 			
	fprintf(outfile,"optimal distance is %d rank %d \n",optdist,myrank); 
	fprintf(outfile,"maxsidelobe at maxones of %d is %d rank %d \n",MAXONES,MAXONES-optdist,myrank);
	fprintf(outfile,"number of optimal matrices (unsorted) %d rank %d \n",optnum,myrank); 
	fprintf(outfile,"total matrices tested with second autocorrelation %d rank %d \n",total,myrank); 
	fprintf(outfile,"total matrices tested rejected earlier %d rank %d \n",total2,myrank); 

	end: 

	fclose(fp);

	MPI_Finalize();
	return 0;  
}
