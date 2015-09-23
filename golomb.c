
#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>

#define SIZE1 5
#define SIZE2 5
#define MOD SIZE2%2
#define MINONES 13
#define MAXONES 20
#define MAXSIDELOBE 10
#define SIZESPEC MAXONES
#define MINDISTANCE 10
#define INFILE "result.txt"

//results that this should work for
// 5*7, 9, 116
//3*5, 6, 14
//6*6, 9, 347
//2*5, 5,  5
//5*5, 8, 4
//4*4, 6, 36
//9*5, 10, 457
//6*7, 10, 50
//7*7, 11, 1

struct coordinate //coordinates of ones
{
	int x; 
	int y; 
};
struct entry 
{
	int rows[SIZE1]; 
}; 
struct fullentry
{
	struct entry intrep; 
	int peaksidelobe;
	int spectrum [SIZESPEC]; 
	//int peakdisp; 
	//int dispspectrum[SIZESPEC];  
	int ones; 	
}; 
int displaylist(struct coordinate * ones, int lenones)
{
	int n; 
	for(n=0; n<lenones; n++)
	{
		printf("%d : %d %d  ",n,ones[n].x,ones[n].y); 
	}
	printf("\n"); 
	return 0; 
}
//displays matrix form of golomb rectangle
int displaymatrix(int mat[SIZE1][SIZE2], FILE * fp ) 
{
	int n1,n2; 
	for(n2=SIZE2-1; n2>=0; n2--)
	{
		for(n1=0; n1<SIZE1; n1++)
		{ 
			if(fp!=NULL)
				fprintf(fp," %d ",mat[n1][n2]);	
			else
				printf(" %d ",mat[n1][n2]);	
		}
		if(fp!=NULL)	
			fprintf(fp,"\n"); 
		else
			printf("\n"); 
	}
	if(fp!=NULL)
		fprintf(fp,"\n");
	else
		printf("\n");  
}
int displayentry(struct entry bob, FILE *fp)
{
	int n; 
	for(n=0; n<SIZE1; n++)
	{
		if(fp!=NULL)
			fprintf(fp,"%d ",bob.rows[n]); 
		else
			printf("%d ",bob.rows[n]); 
	}
	if(fp!=NULL)
		fprintf(fp,"\n"); 
	else
		printf("\n"); 
	return 0; 
}
int displaygeneral(int * autocor,FILE * fp,int size1, int size2) //display autocorrelation
{
	int n1,n2; 
	for(n1=0; n1<size1; n1++)
	{
		for(n2=0; n2<size2; n2++)
		{ 
			if(fp!=NULL)
				fprintf(fp," %d ",autocor[n1*size2+n2]);	
			else
				printf(" %d ",autocor[n1*size2+n2]);	
		}
		if(fp!=NULL)	
			fprintf(fp,"\n"); 
		else
			printf("\n"); 
	}
	if(fp!=NULL)
		fprintf(fp,"\n");
	else
		printf("\n");  
	return 0;  
} 
int calculateMatrix(struct entry bob, int mat[SIZE1][SIZE2])
{	
 	int n1, n2; 
	//displayentry(bob,NULL); 
	for(n1=0; n1<SIZE1; n1++)
	{
		for(n2=SIZE2-1; n2>=0; n2--)
		{
			if( (bob.rows[n1]-pow(2,n2)) >=0)
			{
				bob.rows[n1]=bob.rows[n1]-pow(2,n2);
				mat[n1][n2]=1; 
			} 
			else
			{
				mat[n1][n2]=0; 
			}
		}
	}	
	return 0; 
}
//compute int*size2 integer representation of matrix, most general and fastest way we can form 
int calculateEntry(int mat[SIZE1][SIZE2],struct entry * bob)
{
	int n1,n2; 
	for(n1=0; n1<SIZE1; n1++)
	{
		bob[0].rows[n1]=0; 
		for(n2=0; n2<SIZE2; n2++)
		{
			bob[0].rows[n1]=bob[0].rows[n1]+pow(2,n2)*mat[n1][n2]; 
		}
	}
	return 0; 
}
int calculateSpectrum(int * autocor, int spectrum[SIZESPEC], int ones, int * maxsidelobe,int size1,int size2)
{
	int nx,ny; 
	//displayautocorrelation(autocor,size1,size2);
	(*maxsidelobe)=0; 
        for(nx=0; nx<SIZESPEC; nx++)
	{
		spectrum[nx]=0; 
	}
	for(nx=0; nx<(size1) ;nx++) 
	{
		for(ny=0; ny<(size2); ny++)
		{
			if(autocor[nx*size2+ny]<ones && autocor[nx*size2+ny]<SIZESPEC)
			{	
				if(autocor[nx*size2+ny]>(*maxsidelobe))
					(*maxsidelobe)=autocor[nx*size2+ny]; 	
				spectrum[autocor[nx*size2+ny]]++;
			}
		}
	}
	return 0; 
}
//find the displacement matrix
int calculateAutocor(int * autocor, int mat[SIZE1][SIZE2],int * max)
{
	int lim1=SIZE1-1; 
 	int lim2=SIZE2-1;         
	int nxt,nx,nyt,ny; 
        
	(*max)=0; 
	//initialize autocorrelation matrix
	for(nxt=0; nxt<(2*SIZE1-1); nxt++ )
	{
		for(nyt=0; nyt<(2*SIZE2-1); nyt++ )
		{
			autocor[nxt*(2*SIZE2-1)+nyt]=0; 
		}
	}	
        //displayautocorrelation(autocor,size1,size2); 
 	for(nxt=-lim1;nxt<lim1+1; nxt++)
	{
      		for(nyt=-lim2;nyt<lim2+1; nyt++)
		{
           		for(nx=0; nx<lim1+1; nx++)
			{
                		for(ny=0; ny<lim2+1; ny++)
				{
                     			if( ((nx-nxt)>=0)&& ((ny-nyt)>=0) && ((nx-nxt)<(lim1+1)) && ((ny-nyt)<(lim2+1)) )
					{
                         			autocor[(nxt+lim1)*(2*SIZE2-1)+nyt+lim2]=mat[nx][ny]*mat[nx-nxt][ny-nyt]+autocor[(nxt+lim1)*(2*SIZE2-1)+nyt+lim2]; 
                     			}
                		}
            		}
			if((*max)<autocor[(nxt+lim1)*(2*SIZE2-1)+nyt+lim2])
				(*max)=autocor[(nxt+lim1)*(2*SIZE2-1)+nyt+lim2]; 
      		}
 	}
	//printf("max %d \n",(*max));
        //displayautocorrelation(autocor,NULL); 
	return 0; 
}
int calculateDisplacements(int mat[SIZE1][SIZE2],int * disps)
{
	int n1,n2,x1,y1,count; 
	struct coordinate listones[MAXONES];
	count=0; 
	for(n1=0; n1<SIZE1; n1++)
	{
		for(n2=0; n2<SIZE2; n2++)
		{
			if(mat[n1][n2]==1)
			{
				listones[count].x=n1; 
				listones[count].y=n2; 
				count++;
			} 
		}
	} 
	displaymatrix(mat,NULL); 
	displaylist(listones,count); 
	
	for(n1=0; n1<SIZE1; n1++)
	{
		for(n2=0; n2<(2*SIZE2-1); n2++)
		{
			disps[n1*(2*SIZE1-1)+n2]=0; 
		}
	}	
	
	for(n2=0; n2<count; n2++)
	{
		for(n1=0; n1< n2; n1++)   //entry in counter-1 position is newest 1, we need to verify its displacement with the older ones is not already in the matrix
		{
			y1=listones[n2].y-listones[n1].y; 
			x1=listones[n2].x-listones[n1].x; 
			if(x1<0)  //only half of displacements are unique, we use this convention to pick half
			{
				x1=x1*-1; //just inverting lines, b/c displacements unique up to inversion
				y1=y1*-1; 
			} 
			y1=y1+SIZE2-1; 
		
			//printf("next displacement added %d %d \n",x1,y1); 
			disps[x1*(2*SIZE2-1)+y1]++;
		}
	}
	return 0; 
}
int calculateFullEntry(int mat[SIZE1][SIZE2],struct fullentry * bob)
{
	int n1,n2; 
	int autocor[(2*SIZE1-1)*(2*SIZE2-1)];
	//int disps[SIZE1*(2*SIZE2-1)]; 
	calculateEntry(mat,&(bob[0].intrep)); //calculate entry part of full entry
	calculateAutocor((autocor),mat,&(bob[0].ones));
	calculateSpectrum(autocor,&(bob[0].spectrum[0]),bob[0].ones,&(bob[0].peaksidelobe),2*SIZE1-1,2*SIZE2-1);
	//display info for debugging	
	if(bob[0].ones>=25)
	{
		printf("ones %d \n",bob[0].ones); 
		printf("calculating full entry \n"); 
		displaymatrix(mat,NULL); 		
		printf("calculating autocor \n"); 
		displaygeneral(autocor,NULL,2*SIZE1-1,2*SIZE2-1); 
		printf("peak sidelobe %d \n",bob[0].peaksidelobe);
	}
	/*printf("calculating displacements \n"); 
	calculateDisplacements(mat,disps); 
	displaygeneral(disps,NULL,SIZE1,2*SIZE2-1); 
	printf("calculating displacement spectrum \n"); 
	calculateSpectrum((disps),&(bob[0].dispspectrum[0]),&(bob[0].peakdisp),SIZE1,2*SIZE2-1); */
	return 0; 
}
int compareentry(void * s11, void * s22)
{
	int n; 
	struct entry * s1=(struct entry *)s11; 
	struct entry * s2=(struct entry *)s22; 
	for(n=0; n<SIZE1; n++)
	{
		//printf("int comparisons s1 %d s2 %d \n ",s1[n],s2[n]); 
		if(s1[0].rows[n]>s2[0].rows[n])
			return 1; 
		else if(s1[0].rows[n]<s2[0].rows[n])
			return -1; 
		else
			continue; 
	}
	return 0; 
}
int comparefullentry(void * s11, void * s22)
{
	int n,diff1,diff2; 
	struct fullentry * s1=(struct fullentry *)s11; 
	struct fullentry * s2=(struct fullentry *)s22; 
	
	//sort first based on probability of misdetection, go through spectrum, only if has identical spectrum will it go to other tests	

	if((s1[0].ones-s1[0].peaksidelobe) == (s2[0].ones-s2[0].peaksidelobe)) //only directly compare spectrum if have the same ones-sidelobes, equivalent to 1st approximation
	{
		if(s1[0].ones>s2[0].ones)
		{
			diff1=s1[0].ones-s2[0].ones; 
			diff2=0; 	
		}
		else
		{
			diff2=s2[0].ones-s1[0].ones; 
			diff1=0; 
		}
		for(n=MAXSIDELOBE+diff1+diff2; n>=0; n--)  //need to shift so can compare on the correct footing
		{
			if(s1[0].spectrum[n+diff1]>s2[0].spectrum[n+diff2])
				return 1; 
			if(s1[0].spectrum[n+diff1]<s2[0].spectrum[n+diff2])
				return -1;
			continue;  
		}
	}
	else if((s1[0].ones-s1[0].peaksidelobe) > (s2[0].ones-s2[0].peaksidelobe))
	{
		return -1; 
	}
	else
	{
		return 1;
	}	

	//second sort based on overall magnitude of peak
	if(s1[0].ones>s2[0].ones)
		return 1; 
	if(s1[0].ones<s2[0].ones)
		return -1; 
	
	//next sort on basis of lexographic representation, if all else is equivalent

	for(n=0; n<SIZE1; n++)
	{
		//printf("int comparisons s1 %d s2 %d \n ",s1[n],s2[n]); 
		if(s1[0].intrep.rows[n]>s2[0].intrep.rows[n])
			return 1; 
		else if(s1[0].intrep.rows[n]<s2[0].intrep.rows[n])
			return -1; 
		else
			continue; 
	}
	return 0; 
}
//easy version of code so we can check agaisnt faster version later, if convert option is enabled finds lex least version of mat and stores at mat
int lexleast(int mat[SIZE1][SIZE2], int convert)
{
	int matp[SIZE1][SIZE2]; 
	int matc[SIZE1][SIZE2]; 
	int matleast[SIZE1][SIZE2]; 
	struct entry s1; 
	struct entry s2; 
	int len,n,nx,ny; 
	if(SIZE1==SIZE2)
		len=7; 
	else
		len=3; 

	//printf("starting lex least code \n"); 
	identity(mat,matleast,SIZE1,SIZE2); //store starting value of matleast as mat		
	for(n=0; n<(len+1); n++)
	{
		//n=0 just represents first matrix
		if(n==0)
		{
			identity(mat,matp,SIZE1,SIZE2);
			//printf("i \n"); 			
		} 
		if(n==1)
		{
			verticalflip(mat,matp,SIZE1,SIZE2); 
			//printf("v \n"); 
		}
		if(n==2)
		{
			horizontalflip(mat,matp,SIZE1,SIZE2); 
			//printf("h \n"); 
		}
		if(n==3)
		{
			verticalflip(mat,matc,SIZE1,SIZE2); 
			horizontalflip(matc,matp,SIZE1,SIZE2);
			//printf("vh \n"); 
		}
		if(n==4)
		{
			transpose(mat,matp,SIZE1,SIZE2); 
			//printf("t \n"); 
		}
		if(n==5)
		{
			//identity(mat,matp,size1,size2);
			horizontalflip(mat,matc,SIZE1,SIZE2);
			transpose(matc,matp,SIZE1,SIZE2); 
			//printf("th \n"); 
		}
		if(n==6)
		{
			//identity(mat,matp,size1,size2);
			verticalflip(mat,matc,SIZE1,SIZE2); 
			transpose(matc,matp,SIZE1,SIZE2); 
			//printf("tv \n"); 
		}
		if(n==7)
		{
			//identity(mat,matp,size1,size2);
			verticalflip(mat,matc,SIZE1,SIZE2); 
			horizontalflip(matc,matp,SIZE1,SIZE2);
			transpose(matp,matc,SIZE1,SIZE2);
			identity(matc,matp,SIZE1,SIZE2);
			//printf("tvh \n"); 
		}
		//displaymatrix(matleast,NULL); 
		//displaymatrix(matp,NULL); 
		calculateEntry(matleast,&s1); 
		calculateEntry(matp,&s2); 

		//displayentry(s1,NULL); 
		//displayentry(s2,NULL); 

		if(compareentry(&s1,&s2)==1)
		{	
			if(convert==0)
		 		return 0;
			else
				identity(matp,matleast,SIZE1,SIZE2); 
		} 
	}
	if(convert==1)
	{
		identity(matleast,mat,SIZE1,SIZE2); //return lex least identity of matrix
	} 
	//only returns one if makes it through all parts
	return 1; 
}

//code to test if results are lex least
int transpose(int mat[SIZE1][SIZE2],int matp[SIZE1][SIZE2],int size1,int size2)
{
	//matp is intermediate matrix, store intermediate results of operation
	int nx,ny;
	for(nx=0; nx<SIZE1; nx++)
	{
		for(ny=0; ny<SIZE2; ny++)
		{
			matp[nx][ny]=mat[ny][nx];
		}
	}
}
int verticalflip(int mat[SIZE1][SIZE2],int matp[SIZE1][SIZE2],int size1,int size2)
{
	//matp is intermediate matrix, store intermediate results of operation
	int nx,ny;
	for(nx=0; nx<SIZE1; nx++)
	{
		for(ny=0; ny<SIZE2; ny++)
		{
			matp[nx][ny]=mat[nx][SIZE2-1-ny];
		}
	}
}
int identity(int mat[SIZE1][SIZE2],int matp[SIZE1][SIZE2],int size1, int size2)
{
	int nx,ny; 
	for(nx=0; nx<SIZE1; nx++)
	{
		for(ny=0; ny<SIZE2; ny++)
		{
			matp[nx][ny]=mat[nx][ny];
		}
	}
}
int horizontalflip(int mat[SIZE1][SIZE2],int matp[SIZE1][SIZE2],int size1,int size2)
{
	//matp is intermediate matrix, store intermediate results of operation
	int nx,ny;
	for(nx=0; nx<SIZE1; nx++)
	{
		for(ny=0; ny<SIZE2; ny++)
		{
			matp[nx][ny]=mat[SIZE1-1-nx][ny];
		}
	}
}
//same method as before except in loop form for increased speed, minimize the number of function calls
int imperativeform(int mat[SIZE1][SIZE2], struct coordinate disps[MAXONES][MAXONES], struct coordinate listones[MAXONES], int displacements[SIZE1][2*SIZE2-1],int n1[MAXONES],int n2[MAXONES],int minx[MAXONES], FILE *fp, int *outcount,int counter,int * minsidelobes, int * maxsidelobes,struct fullentry * bob)
{
	int inc,inc2,x1,y1;
	int matp[SIZE1][SIZE2];
        //when 1st comes in counter=1 
	struct entry ref; 
	ref.rows[0]=2; 
	ref.rows[1]=17; 
	ref.rows[2]=22; 	 


	//first loop needs to be of special form because of restriction on search space, but only amounts to Size/4 function calls anyways so don't need here
	start: ;
	
	//printf("counter %d %d %d \n",counter,listones[counter].x,listones[counter].y); 

	/*printf("\n \n"); 
	printf("level %d \n",counter+1);
	displaylist(listones,counter+1);  
	displaymatrix(mat,NULL); 
	printf("added one %d %d \n",listones[counter].x,listones[counter].y);  */

	/*if((counter+1)==MINONES)
	{
		identity(mat,matp,SIZE1,SIZE2); 
		lexleast(matp,1);  
		calculateFullEntry(matp,bob); 
		//displayentry((*bob).intrep,NULL); 
		if(compareentry(&ref,&((*bob).intrep))==0)
		{
			printf("Have match! \n"); 
		}
	}*/

	//need mod offset for odd dimensions so can strictly define with bigger half of matrix, otherwise can't gaurantee anything with given class
	if(SIZE2*SIZE1/2+MOD <(listones[counter].y*SIZE1+listones[counter].x) && counter <MINONES/2 )
	{
		//will never satisfy this for counter =1, can just goto loop here
		counter=counter-1; 
		goto loop; 
	} 
	
	//printf("2nd part \n"); 
	//2nd check whether or not we can add displacements to list
	maxsidelobes[counter+1]=maxsidelobes[counter]; //start with max from previous level 
	for(inc=0; inc<(counter); inc++)   //entry in counter-1 position is newest 1, we need to verify its displacement with the older ones is not already in the matrix
	{
		y1=listones[counter].y-listones[inc].y; 
		x1=listones[counter].x-listones[inc].x; 
		if(x1<0)  //only half of displacements are unique, we use this convention to pick half
		{
			x1=x1*-1; //just inverting lines, b/c displacements unique up to inversion
			y1=y1*-1; 
		} 
		y1=y1+SIZE1-1; 
		
		//printf("next displacement added %d %d \n",x1,y1); 
		displacements[x1][y1]++;
		disps[inc][counter].x=x1;  //add new displacements to displacement list 
		disps[inc][counter].y=y1; 
 		
		if(displacements[x1][y1]>maxsidelobes[counter+1])
			maxsidelobes[counter+1]=displacements[x1][y1]; 
		if(displacements[x1][y1]>MAXSIDELOBE  ) //use sidelobe criteria and previous maxdistance to sort results
		{
			mat[listones[counter].x][listones[counter].y]=0; //need to remove 1 from matrix
			for(inc2=0; inc2<=inc; inc2++)
			{
				displacements[disps[inc2][counter].x][disps[inc2][counter].y]--; //remove all previously added displacements
			}
			counter=counter-1; //go back up level, again will never satisfy for counter=0 here
			goto loop; 
		}
	}
	
	//printf("3rd part \n"); 
	//3rd check if satisfies the minimum ones requirement, if so print out
	if( (counter+1)>=MINONES && ((counter+1-maxsidelobes[counter+1])>=MINDISTANCE) )
	{
		(*outcount)++; 	
	 	displaymatrix(mat,fp);
		//if(minsidelobes[bob[0].ones]>bob[0].peaksidelobe)
		//	minsidelobes[bob[0].ones]=bob[0].peaksidelobe; 
	}  
	minx[counter]=listones[counter].x; 
	//iterate deeper by adding more ones 
	for(n2[counter]=listones[counter].y; n2[counter]<SIZE2; n2[counter]++)
	{
		for(n1[counter]=minx[counter]; n1[counter]<SIZE1; n1[counter]++)
		{	
			if(mat[n1[counter]][n2[counter]]==0)  //only add 1 to spot if none already there, don't touch ones from other levels
			{
				//need to put one more spot in matrix, needs to put list coordinate at counter+1, because loop currently on counter interation
				listones[counter+1].x=n1[counter]; 
				listones[counter+1].y=n2[counter];
				mat[n1[counter]][n2[counter]]=1; 	
				counter=counter+1; 
				goto start;   //only can go to start from this point
 				//bad cases, or terminations will return here, decrementing counter as they come back up
				loop: ;				
			 	mat[n1[counter]][n2[counter]]=0; //have to conserve ones at a given level, once test with one take it away and add another place
			}
		}
		minx[counter]=0; //allow to access any column in row after restriction on row with original entry
	}	
	//needs to remove displacements when returning so it will not effect the operation of the next branch
	for(inc=0; inc< counter; inc++)
	{
		displacements[disps[inc][counter].x][disps[inc][counter].y]--; //remove all previously added displacements
	}
	//go back up level
	counter=counter-1; 
	if(counter!=-1)
		goto loop;  //done checking cases needs to go back to place where called from 
	else
		return 0;
}
//take input file from fortran code and convert into format which can be processed by this code, so we can check if equivalent
int convert(int mat[SIZE1][SIZE2],char *infile,char *outfile,int * numbers)
{
	printf("reading in data file \n"); 
	int swit2,count,n,val;
	struct entry bob; 
	FILE * fpin;
	FILE * fpout;  
	char text [1000]; 
	fpin=fopen(infile,"r"); 
	fpout=fopen(outfile,"w");
	(*numbers)=0; //we don't know how many results we are reading in

	fgets(&text[0],1000,fpin); //get first two lines
        fgets(&text[0],1000,fpin);  
	swit2=0; 
	while(1)
	{
		//code for locating set of values from line and extracting
		count=0; 
		while(count<SIZE1)
		{
                        //second method for simpler output format
                        fscanf(fpin,"%d",&val); 
                        if(feof(fpin)) //end of file indicator, read in all words which are part of matrices
			{	
				swit2=1; 
				break;
			} 
			//printf("%d \n",val); 
        		bob.rows[count]=val;
			//printf("%d \n",bob.rows[count]);  
			count++; 
		}	
                if(swit2==1)
		{
			break;
		}
		if(count==SIZE1)  //if there are SIZE1 components read in, print out matrix to outfile              
		{
			calculateMatrix(bob,mat); 
			//displayentry(bob,NULL); 
			displaymatrix(mat,fpout);
			(*numbers)++; 
			fgets(&text[0],1000,fpin); //go to next line
		}
	}
	//printdata(data,size1,(*numbers)); 
	printf("read data in \n"); 
	printf("numbers %d \n",(*numbers)); 
	
	fclose(fpin); 
	fclose(fpout); 
	
	return 0; 
}
//1st thing need to do is add new differences to displacements and confirm that these displacements are new
int addones(int mat[SIZE1][SIZE2], struct coordinate disps [MAXONES][MAXONES], struct coordinate listones[MAXONES], int counter, int displacements[SIZE1][2*SIZE2-1], FILE * fp, int * outcount )
{
	int n1, n2, minx, miny,x1,y1;
	/*if(counter>=11)
	{
		printf("\n \n"); 
		printf("level %d \n",counter);
		displaylist(listones,counter);  
		displaymatrix(mat,NULL); 
		printf("added one %d %d \n",listones[counter-1].x,listones[counter-1].y);  
	}*/

	//1st check whether or not matrix is lexographically least, criteria is if less than half of max ones in upper half
	
	//need mod offset for odd dimensions so can strictly define with bigger half of matrix, otherwise can't gaurantee anything with given class
	if(SIZE2*SIZE1/2+MOD <(listones[counter-1].y*SIZE1+listones[counter-1].x) && counter <MINONES/2 )
	{
		return 0;
	} 	
	//2nd check whether or not we can add displacements to list
	for(n1=0; n1< (counter-1); n1++)   //entry in counter-1 position is newest 1, we need to verify its displacement with the older ones is not already in the matrix
	{
		y1=listones[counter-1].y-listones[n1].y; 
		x1=listones[counter-1].x-listones[n1].x; 
		if(x1<0)  //only half of displacements are unique, we use this convention to pick half
		{
			x1=x1*-1; //just inverting lines, b/c displacements unique up to inversion
			y1=y1*-1; 
		} 
		y1=y1+SIZE2-1; 
		
		//printf("next displacement added %d %d \n",x1,y1); 
		displacements[x1][y1]++;
		disps[n1][counter-1].x=x1;  //add new displacements to displacement list 
		disps[n1][counter-1].y=y1; 
 
		if(displacements[x1][y1]>1)
		{
			mat[listones[counter-1].x][listones[counter-1].y]=0; //need to remove 1 from matrix
			for(n2=0; n2<=n1; n2++)
			{
				displacements[disps[n2][counter-1].x][disps[n2][counter-1].y]--; //remove all previously added displacements
			}
			return 0; 
		}
	}
	
	//3rd check if satisfies the minimum ones requirement, if so print out
	if(counter>=MINONES)
	{
		//if(lexleast(mat,0)==1) //only display matrix if lex least
		//{
			(*outcount)++; 	
	 		displaymatrix(mat,fp);
		//}
		//displaydisplacements(displacements,fp); 
	}  

	minx=listones[counter-1].x; 
	miny=listones[counter-1].y; 
	//iterate deeper by adding more ones 
	for(n2=miny; n2<SIZE2; n2++)
	{
		for(n1=minx; n1<SIZE1; n1++)
		{
			listones[counter].x=n1; //put one at spot in matrix
			listones[counter].y=n2;	
			if(mat[n1][n2]==0)  //only add 1 to spot if none already there, don't touch ones from other levels
			{
				mat[n1][n2]=1; 	
				addones(mat,disps,listones,counter+1,displacements,fp,outcount); 
			 	mat[n1][n2]=0; //have to conserve ones at a given level, once test with one take it away and add another place
			}
		}
		minx=0; //allow to access any column in row after restriction on row with original entry
	}	
	//needs to remove displacements when returning so it will not effect the operation of the next branch
	for(n2=0; n2< (counter-1); n2++)
	{
		displacements[disps[n2][counter-1].x][disps[n2][counter-1].y]--; //remove all previously added displacements
	}
	return 0; //shouldn't need to return whether or successful or not for code to contineu
}
int sort(char * fname, int counts, int mat[SIZE1][SIZE2])
{
	FILE * fp; 
	int n,n1,n2, inti,max,start,maxdiff;
	struct fullentry * matrices;
	int minsidelobes [MAXONES]; 
	fp=fopen(fname,"r");
	matrices=(struct fullentry *)calloc(counts,sizeof(struct fullentry)); 
	printf("Beginning to sort results \n");
	max=0;
 
	for(n=0; n<MAXONES; n++)
	{
		minsidelobes[n]=100;	
	}
	for(n=0; n<counts; n++)
	{
		for(n2=0; n2<SIZE2; n2++)
		{				
			for(n1=0; n1<SIZE1; n1++)
			{
				fscanf(fp,"%d ",&inti); 
				mat[n1][n2]=inti;
			}
			fscanf(fp,"\n ",&inti); 
		}
		lexleast(mat,1);  
		calculateFullEntry(mat,&matrices[n]);
		//displayentry(matrices[n].intrep,NULL); 
		if(matrices[n].ones>max)
			max=matrices[n].ones;
		if(minsidelobes[matrices[n].ones]>matrices[n].peaksidelobe)
			minsidelobes[matrices[n].ones]=matrices[n].peaksidelobe;   
	}
	fclose(fp);
	printf("Maximum ones reached in search is %d \n",max);
	printf("optimal sidelobe for each level \n"); 
	start=-1; 
	//
	//printf("minones %d max %d \n",MINONES,max);
	int min=100; 
	//need to be careful here, get start of range and minimum sidelobe independently
	for(n=MINONES; n<=max; n++)
	{
		//printf("%d \n",minsidelobes[n]); 
		if(minsidelobes[n]<min)
		{
			min=minsidelobes[n]; 
		}
		if(start==-1 && minsidelobes[n]!=100)
			start=n; 
		if(start!=-1)
			printf("%d ",minsidelobes[n]); 
	} 
	printf(" \n"); 
	//create matrix for storing sidelobe and length statistics, if find lower sidelobe, need to also break down by sidelobe		 
	int * counttype=(int *)calloc((max-start+1)*(MAXSIDELOBE-min+1),sizeof(int)); 
	qsort((void *)matrices,counts,sizeof(struct fullentry),comparefullentry);
	n2=0;


	//now print out only the unique entries 
	fp=fopen(fname,"w"); 
	fprintf(fp,"Matrices for maximum sidelobe %d with dimensions %d by %d \n",MAXSIDELOBE,SIZE1,SIZE2); 

        //need to print out 1st matrix in list first, code works by printing out 1st unique matrix of sorted group and ignoring the rest
	calculateMatrix(matrices[0].intrep,mat); 
	displaymatrix(mat,fp); 	
	displayentry(matrices[0].intrep,fp);
	fprintf(fp,"\n");

	
	//printf("start %d \n",start);  
	counttype[(max-start+1)*(matrices[0].peaksidelobe-min)+matrices[0].ones-start]++; 
	for(n1=0; n1<=MAXSIDELOBE; n1++)
	{
		fprintf(fp,"%d ",matrices[0].spectrum[n1]); 
	}
	fprintf(fp," \n"); 
	fprintf(fp, "ones %d, peak sidelobe %d \n \n",matrices[0].ones,matrices[0].peaksidelobe);  
	n2++;  
	maxdiff=matrices[0].ones-matrices[0].peaksidelobe;

	printf("range for table is %d ones to %d, and sidelobe %d to %d \n",max,start,MAXSIDELOBE,min); 
	printf("entering loop %d counts \n",counts); 	
 
	for(n=1; n<counts; n++)
	{
		if(compareentry(&matrices[n],&matrices[n-1])!=0) //do not print out as long as previous entry same as current entry
		{

			calculateMatrix(matrices[n].intrep,mat);
			if(matrices[n].peaksidelobe>MAXSIDELOBE)
				continue;  
			displaymatrix(mat,fp); 			
			displayentry(matrices[n].intrep,fp);
			fprintf(fp,"\n"); 
			for(n1=0; n1<=MAXSIDELOBE; n1++)
			{
				fprintf(fp,"%d ",matrices[n].spectrum[n1]); 
			}
			fprintf(fp," \n"); 
			fprintf(fp, "ones %d, peak sidelobe %d \n \n",matrices[n].ones,matrices[n].peaksidelobe);  
			//printf("%d %d \n",(max-start+1)*(matrices[n].peaksidelobe-min)+matrices[n].ones-start,(max-start+1)*(MAXSIDELOBE-min+1)); 
			//printf("%d peaksidelobe, %d ones \n",matrices[n].peaksidelobe,matrices[n].ones); 
			counttype[(max-start+1)*(matrices[n].peaksidelobe-min)+matrices[n].ones-start]++; 
			n2++; 
			if((matrices[0].ones-matrices[0].peaksidelobe)>maxdiff)
				maxdiff=(matrices[0].ones-matrices[0].peaksidelobe);
		}		
	} 
	fprintf(fp,"Total Unique matrices are %d \n",n2); 
	printf("sorted through matrices and eliminated redundant elements, have %d unique matrices out of %d \n",n2,counts); 
	fprintf(fp,"Maximum ones reached in search is %d \n",max);
	fprintf(fp,"optimal sidelobe for each level \n"); 
	for(n=start; n<=max; n++)
		fprintf(fp,"%d : %d ",n,minsidelobes[n]); 
	fprintf(fp," \n"); 
	printf("maxdiff is %d, start is %d, max is %d \n",maxdiff,start,max); 
	printf("printing out table \n"); 
	//print out table of counts to give breakdown and sidelobe and int counts for given max
	for(n1=0; n1<=(max-start); n1++)
	{
		fprintf(fp," \n");
		if(n1==0) 
			fprintf(fp,"   ");
		else
			fprintf(fp,"%d  ",n1+start);  	
		for(n2=0; n2<=(MAXSIDELOBE-min); n2++)
		{
			if(n1==0)
				fprintf(fp,"%d  ",n2+min); 
			else
				fprintf(fp,"%d  ",counttype[(max-start+1)*n2+(n1)]); 			
		}
	}
	printf("finished loop \n"); 
	//needs to also give sum of lowest diagonal corresponding to highest max
	int sum=0; 
	for(n1=0; n1<=(max-start); n1++)
	{
		//printf("%d \n",start+n1-maxdiff-minsidelobes[start]); 
		if((start+n1-maxdiff-min)>=0) //if not lined up properly can give sidelobe we did not include before
			sum=sum+counttype[(max-start+1)*(start+n1-maxdiff-min)+n1]; //need to sweep constant diffmax contour	
	}
	printf("\n \n There are %d results with optimal difference of %d \n",sum,maxdiff); 
	fprintf(fp,"\n \n There are %d results with optimal difference of %d \n",sum,maxdiff);  
	fclose(fp);
	printf("reached end \n");  
	return 0; 
}
//we want to implement backtracking algorithim, for golomb rectangles, we will only include matrix in output if includes a minimum number of ones
int main()
{
	int mat[SIZE1][SIZE2]; //includes zeros and 1s
	int displacements[SIZE1][2*SIZE2-1]; //list of displacements already taken up
	int c1,c2, counter, outcount; 
	counter=0; 
	outcount=0; 
	struct coordinate disps[MAXONES][MAXONES]; 
	struct coordinate listones[MAXONES];
	int minsidelobes[MAXONES];
	int maxsidelobes[MAXONES];  
	struct fullentry bob; 
	int inc1[MAXONES]; 
	int inc2[MAXONES]; 
	int minx[MAXONES]; 
	
	//we need to keep a list of one coordinate and see the effect of adding one to a spot, we go further down tree if we can keep adding ones after that one, otherwise we place a one at the      
        //next spot available and continue on, in order for the results of a particular branch to be printed we have to satisfy a minimum number of ones, and have a matrix which is lexographically         
       //least
        //initialize matrix to zero
	
	/*
	FILE *fp; 
	fp=fopen("Golomb.txt","w");  	
	printf("initializing matrix and displacement matrix \n"); 
	for(c1=0; c1<SIZE1; c1++)
	{
		for(c2=0; c2<SIZE2; c2++)
		{
			mat[c1][c2]=0; 
		}
	}
	for(c1=0; c1<MAXONES; c1++)
	{
		minsidelobes[c1]=100; 
		maxsidelobes[c1]=0; 
	}
	for(c1=0; c1<SIZE1; c1++)
	{
		for(c2=0; c2<2*SIZE2-1; c2++)
		{
			displacements[c1][c2]=0; 
		}
	}
	printf("modulus is %d \n",MOD); 
	printf("doing backtrack search \n"); 
        for(c1=0; c1<(SIZE1/2+MOD); c1++) //only need to search small portion of structure
	{
		c2=0; 
		listones[counter].x=c1; //put one at spot in matrix
		listones[counter].y=c2;
		mat[c1][c2]=1; 		
		//addones(mat,disps,listones,counter+1,displacements,fp,&outcount); 
		imperativeform(mat,disps,listones,displacements,inc1,inc2,minx,fp,&outcount,counter,minsidelobes,maxsidelobes,&bob); 
		mat[c1][c2]=0; 

	}
	fclose(fp); 
	*/

	//printing out minimum sidelobe at each level
	/*printf("minimum sidelobe \n");
	for(c1=0; c1<20; c1++)
		printf("%d ",minsidelobes[c1]);
	printf("\n");*/  

	convert(mat,INFILE,"Golomb.txt",&outcount); 
	printf("%d matrices found \n",outcount); 
	sort("Golomb.txt",outcount,mat); 	

	return 0; 
}


