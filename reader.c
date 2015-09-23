	
#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>

#define SIZE1 7
#define SIZE2 3
#define TYPE  1
#define MSL   3
#define PEAK  36
#define FNAME "RESULT.TXT"
#define SIZESPEC 20
#define OUTFILE "ranked66.txt"
//pars is the number of parallel output files 
#define PARS 4 

struct array   //define structure which will store matrix data
{
	int rows [SIZE1]; 
};

/*int printhex(int mat[SIZE1][SIZE2],struct array * data, int size, int len) //print out matrix information on screen
{
	int n,n2; 

	int a=255; 
	sprintf(buf,"%X",a);
	printf("Dec %d \n",a); 
	printf("Hex %s \n",buf);

	for(n=0; n<len; n++)
	{
		for(n2=0; n2<size; n2++)
		{
			printf("%d ", data[n].rows[n2]); 
		}
		printf(" \n"); 
	}

}*/
int printdata(struct array * data, int size, int numbers) //print out matrix information on screen
{
	int n,n2; 
	//printf("numbers %d \n ", numbers); 
	for(n=0; n<numbers; n++)
	{
		//printf(" %d ,", n); 
		for(n2=0; n2<size; n2++)
		{
			printf(" %d ", data[n].rows[n2]); 
		}
		printf(" \n");
	}
	printf("\n"); 

}
int getmemory(void ** data, int size, struct array **trash, int * trc) //gets structure memory of size, size
{
	//dereference address of address to place address 
	*data=(void*)calloc(size,sizeof(struct array)); 
	printf("making entry memory allocation \n"); 
	if((*data)==NULL)
	{ 
		printf("null pointer returned \n"); 
		return 1;
	}
	printf("%d memory allocated \n", size); 
	trash[*trc]=(struct array *)*data; 
	(*trc)=(*trc)+1; 
	return 0; 
}
int freeData( int ** trash, int trc )  //frees array of integer pointers which stores pointers to dynamically allocated memory
{
	int i; 
	for(i=0; i<trc; i++)
	{
		free(trash[i]); 
	} 
	printf("data freed \n");
	return 0; 
}
//if type ==1 use -1, 1 for entries, otherwise use 0,1
int matrixconvert (int mat[SIZE1][SIZE2],struct array entry, int size1, int size2, int type)  //need to find the matrix representation of the numbers
{
	int n,n1; 
	int num; 
	int val1,val2; 

	if(type==1)
	{
		val1=1; 
		val2=-1; 
	}
	else
	{
		val1=1; 
		val2=0; 
	}
	
	for(n=0; n<size1; n++)
	{
		num=entry.rows[n];
                //printf("%d \n ", num); 
                for(n1=0; n1<size2; n1++) //array has size1 by size2 bits, need to extract binary representation from decimal
		{
			if( (num-pow(2.0,(double)(size2-n1-1)))>=0 )
			{
                                //printf("%d ", num); 
				num=num-pow(2.0,(double)(size2-n1-1)); 
				mat[n][n1]=val1; 
			} 
			else
			{
				mat[n][n1]=val2; 
			}
		}
                //printf("\n");  %disp(Mv);
	}
	//printdata(&entry,size1,1); 
	//displaymatrix(mat,size1,size2); 

}
//calculate decimal representation of matrix
int decconvert (int mat[SIZE1][SIZE2],int size1, int size2,struct array * data)
{
	int nx, ny; 
	//printf("base %d \n",base); 
	int sum=0;   
	for(nx=0; nx<SIZE1; nx++)
	{
		sum=0; 
		for(ny=0; ny<SIZE2; ny++)
		{
			if(mat[nx][ny]==1)
				sum=sum+pow((double)2,(double)(size2-1-ny)); 	
		}	
		//printf("sum %d \n",sum); 
		data[0].rows[nx]=sum; 
		//printf("bob %ld \n",bob);  
	}	
}

//calculate 64 bit representation that allows lexleast comparision tests
int intconvert (int mat[SIZE1][SIZE2],int size1, int size2, long * lee)
{
	int nx, ny; 
	long bob=0; 
	int base=(int)pow(2.0,8.0);
	//printf("base %d \n",base); 
	int sum=0;   
	for(nx=0; nx<SIZE1; nx++)
	{
		sum=0; 
		for(ny=0; ny<SIZE2; ny++)
		{
			if(mat[nx][ny]==1)
				sum=sum+pow((double)2,(double)(size2-1-ny)); 	
		}	
		//printf("sum %d \n",sum); 
		bob=bob+(int)sum*pow((double)base,(double)(7-nx));
		//printf("bob %ld \n",bob);  
	}
	(*lee)=bob; 
		
}
//returns 1 if lex least in equivalence class, 0 otherwise
int lexleast(int mat[SIZE1][SIZE2],int matp[SIZE1][SIZE2], int matc[SIZE1][SIZE2],int size1,int size2, int type)
{
	int len,n,nx,ny;
	if(SIZE1==SIZE2)
		len=3; 
	else
		len=3; 
	long ref,comp; 
	intconvert(mat,size1,size2,&ref); 
	//printf("reference matrix size %ld \n",ref); 

	//need to keep mat the same throughout all operations		
	for(n=0; n<(len+1); n++)
	{
		//n=0 just represents first matrix
		if(n==0)
		{
			identity(mat,matp,size1,size2);
			//printf("i \n"); 			
		} 
		if(n==1)
		{
			verticalflip(mat,matp,size1,size2); 
			//printf("v \n"); 
		}
		if(n==2)
		{
			horizontalflip(mat,matp,size1,size2); 
			//printf("h \n"); 
		}
		if(n==3)
		{
			verticalflip(mat,matc,size1,size2); 
			horizontalflip(matc,matp,size1,size2);
			//printf("vh \n"); 
		}
		if(n==4)
		{
			rotation(mat,matp,size1,size2); 
			//printf("t \n"); 
		}
		if(n==5)
		{
			//identity(mat,matp,size1,size2);
			horizontalflip(mat,matc,size1,size2);
			rotation(matc,matp,size1,size2); 
			//printf("th \n"); 
		}
		if(n==6)
		{
			//identity(mat,matp,size1,size2);
			verticalflip(mat,matc,size1,size2); 
			rotation(matc,matp,size1,size2); 
			//printf("tv \n"); 
		}
		if(n==7)
		{
			//identity(mat,matp,size1,size2);
			verticalflip(mat,matc,size1,size2); 	
                        horizontalflip(matc,matp,size1,size2);
			rotation(matp,matc,size1,size2);
			identity(matc,matp,size1,size2);
			//printf("tvh \n"); 
		}
		//printf("ref \n"); 
		//displaymatrix(mat,size1,size2,type); 
		//displaymatrix(matp,size1,size2,type); 
		if(type==1)
		{
			phasemin(matp,size1,size2);  //this operation only meaningful if we are dealing with 1, -1 structure
		}
		intconvert(matp,size1,size2,&comp);
		//printf("comp %ld \n",comp); 
		//displaymatrix(matp,size1,size2); 
		if(comp<ref)
		 	return 0; 
	}
	//only returns one if makes it through all parts
	return 1; 
}
//this function uses phase shift operations to make mat lex least
int phasemin(int mat[SIZE1][SIZE2],int size1,int size2)
{
	int c=0;
	int p=0;
	int t=0;
	if(mat[0][0]==1)
	{
		if(mat[0][1]==1)
		{
			if(mat[1][0]==1)
			{
				c=1;
				p=0;
				t=0;
			}
			if(mat[1][0]==-1)
			{
				c=1;
				t=0; 
				p=1; 
			}
		}
		if(mat[0][1]==-1)
		{
			if(mat[1][0]==1)
			{
				c=1;
				t=1;
				p=0;
				
			}
			if(mat[1][0]==-1)
			{
				c=1;
				t=1; 
				p=1; 
			}
		}
	}
	else
	{
		if(mat[0][1]==1)
		{
			if(mat[1][0]==1)
			{
				c=0;
				t=1;
				p=1;
			}
			if(mat[1][0]==-1)
			{
				c=0;
				t=1; 
				p=0; 
			}
		}
		if(mat[0][1]==-1)
		{
			if(mat[1][0]==1)
			{
				c=0;
				p=1;
				t=0;
			}
			if(mat[1][0]==-1)
			{
				c=0;
				t=0; 
				p=0; 
			}
		}
	}
	//printf("p %d t %d c %d \n",p,t,c); 
	if(c==1)
	{
		comp(mat,size1,size2); 
	}
	if(t==1)
	{
		the(mat,size1,size2); 
	}
	if(p==1)
	{
		phi(mat,size1,size2); 
	}
		
}
int phi(int mat[SIZE1][SIZE2],int size1,int size2)
{
	int nx,ny;
	for(nx=0; nx<SIZE1; nx++)
	{
		for(ny=0; ny<SIZE2; ny++)
		{
			mat[nx][ny]=mat[nx][ny]*pow(-1,nx);
		}
	}
}
int the(int mat[SIZE1][SIZE2],int size1,int size2)
{
	int nx,ny;
	for(nx=0; nx<SIZE1; nx++)
	{
		for(ny=0; ny<SIZE2; ny++)
		{
			mat[nx][ny]=mat[nx][ny]*pow(-1,ny);
		}
	}
}
int comp(int mat[SIZE1][SIZE2],int size1,int size2)
{
	int nx,ny;
	for(nx=0; nx<SIZE1; nx++)
	{
		for(ny=0; ny<SIZE2; ny++)
		{
			mat[nx][ny]=mat[nx][ny]*(-1);
		}
	}
}
int rotation(int mat[SIZE1][SIZE2],int matp[SIZE1][SIZE2],int size1,int size2)
{
	//matp is intermediate matrix, store intermediate results of operation
	int nx,ny;
	for(nx=0; nx<SIZE1; nx++)
	{
		for(ny=0; ny<SIZE2; ny++)
		{
			matp[nx][ny]=mat[SIZE1-ny-1][nx];
		}
	}
} //scought, Bejammin, Hvan, Antauntaun, 
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
int displaymatrix(int mat[SIZE1][SIZE2], int size1, int size2,int type) //display binary form of matrix
{
	int n1; 
	int n2; 
	for(n1=0; n1<size1; n1++)
	{
		for(n2=0; n2<size2; n2++)
		{ 
                        //if(mat[n1][n2]==1 && type ==1) //dont need space if just 1s and 0s
			//	printf(" "); 
			if(mat[n1][n2]==1)
				printf(" %d ",1);
			else
				printf(" %d ",0);  
			
		}	
		printf("\n"); 
	}
	printf("\n"); 
} 
//code will compute autocorrelation and check that matrices satisfy minimum sidelobe criteria
int autocorrelation(int autocor[2*SIZE1-1][2*SIZE2-1], int mat[SIZE1][SIZE2],struct array entry,int size1, int size2)
{
    int lim1=size1-1; 
    int lim2=size2-1;         
    int nxt,nx,nyt,ny; 
        
	//initialize autocorrelation matrix
	for(nxt=0; nxt<2*(size1-1)+1; nxt++ )
	{
		for(nyt=0; nyt<2*(size2-1)+1; nyt++ )
		{
			autocor[nxt][nyt]=0; 
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
                                                
                         			autocor[nxt+lim1][nyt+lim2]=mat[nx][ny]*mat[nx-nxt][ny-nyt]+autocor[nxt+lim1][nyt+lim2]; 
                     			}
                		}
            		}
			if(nxt==0 && nyt==0 && TYPE==2)//in addition have minimum peak requirement 
			{
				if(autocor[nxt+lim1][nyt+lim2]!=PEAK)
					return 0; 
			}
                        //check if doesn't satisfy minimum sidelobe condition, account for case of alignment, do not include case lined up correctly
                        if( ((autocor[nxt+lim1][nyt+lim2]>MSL) || (autocor[nxt+lim1][nyt+lim2]<-MSL)) && !(nxt==0 && nyt==0) )
			{
				//printf("Does not satisfy minimum sidelobe condition \n"); 
				//printf("%d \n",autocor[nxt+lim1][nyt+lim2]); 
				//printdata(&entry,size1,1);
				return 0;  
			}
      		}
 	}
	return 1; 
        displayautocorrelation(autocor,size1,size2); 
	//int energy; 
	//slenergy(autocor,size1,size2,&energy); 
	//printf("Side lobe energy %d \n",energy);
	printf("\n");  
	
}
int displayautocorrelation(int autocor[2*SIZE1-1][2*SIZE2-1], int size1, int size2) //display autocorrelation
{

	int n1; 
	int n2;
	size1=2*size1-1; 
        size2=2*size2-1;  
	for(n1=0; n1<size1; n1++)
	{
		for(n2=0; n2<size2; n2++)
		{ 
                        if(autocor[n1][n2]>=0)
				printf(" "); 
			printf("%d ",autocor[n1][n2]); 
			
		}	
		printf("\n"); 
	}
	printf("\n"); 
} 
//need to calculate energy from top half of autocorrelation matrix plus the middle half of the first row, not including center peak
int slenergy(int autocor[2*SIZE1-1][2*SIZE2-1], int size1, int size2, int * energy)
{
	int nx,ny; 
	(*energy)=0; 
	for(nx=0; nx<(2*size1-1) ;nx++) 
	{
		for(ny=0; ny<(size2-1); ny++)
		{
			(*energy)=(*energy)+pow(autocor[nx][ny],2.0); 
			//printf("energy %d \n",(*energy)); 
		}
	}
	for(nx=(size1);nx<(2*size1-1); nx++)
	{
		(*energy)=(*energy)+pow(autocor[nx][size2-1],2.0); 
	}
	//printf("energy %d \n",(*energy)); 
}
//calculate spectrum and read this into SIZESPEC
int calculatespec(int autocor[2*SIZE1-1][2*SIZE2-1], int size1, int size2, int spectrum[SIZESPEC])
{
	int nx,ny; 
	//displayautocorrelation(autocor,size1,size2); 
        for(nx=0; nx<SIZESPEC; nx++)
	{
		spectrum[nx]=0; 
	}
	for(nx=0; nx<(2*size1-1) ;nx++) 
	{
		for(ny=0; ny<(2*size2-1); ny++)
		{
			if(autocor[nx][ny]>=0)
				spectrum[autocor[nx][ny]]++;
			else
				spectrum[-autocor[nx][ny]]++;
		}
	}
	return 0; 
}
//calculate the spectrum of a given array and compare to a reference spectrum
int comparespec(int autocor[2*SIZE1-1][2*SIZE2-1], int size1, int size2, int spectrum[SIZESPEC])
{
	int nx,ny; 
	int spec[SIZESPEC]; 
	calculatespec(autocor,size1,size2,spec); //calculate spectra and then compare result
	for(nx=0; nx<SIZESPEC; nx++)
	{
		if(spectrum[nx]!=spec[nx])
			return 0; 
	}
	return 1; //will only reach this condition if matrix is identical
	 
}
//display spectrum of a list of finds
int displayspectrum(struct array * data,int mat[SIZE1][SIZE2], int autocor[2*SIZE1-1][2*SIZE2-1], int numbers, int size1,int size2, int spectrum[SIZESPEC],int type)
{
	int n,n2; 
	int len; 
	len=numbers; 
	for(n=0; n<len; n++)
	{
		printf("\n"); 
		matrixconvert(mat,data[n],size1,size2,type); 
                printdata(&data[n],size1,1);
                displaymatrix(mat,size1,size2,type); 
 		autocorrelation(autocor,mat,data[n],size1,size2);
                displayautocorrelation(autocor,size1, size2); 
		calculatespec(autocor,size1,size2,spectrum); 
		for(n2=0; n2<SIZESPEC; n2++)
		{
			printf("%d ",n2); 
		}
		printf("\n"); 
		for(n2=0; n2<SIZESPEC; n2++)
		{
			printf("%d ",spectrum[n2]); 
		}
		printf("\n"); 
	}
	return 0;  
}
//need to identify matrices that have an equivalent spectrum to the result listed, first need to calculate autocorrelation
int findspectrum(struct array * data,int mat[SIZE1][SIZE2], int autocor[2*SIZE1-1][2*SIZE2-1], int numbers, int size1,int size2, int spectrum[SIZESPEC],int type)
{
	//printf("numbers %d \n",numbers); 
	int len; 
	len=numbers; 
	int g,n2,n;
	int check; 
	int matchcounts=0; //use these to keep track of sprintf Carray
	struct array matches[50]; //store equivalent minimum arrays
	for(g=0; g<len; g++)
	{
		//if(n%10000==0)
		//	printf(" %d \n",g);
		//	printf(" %d \n",numbers) //really weird error where increments numbers for some reason
		//	printf(" %d \n",len);  		
		matrixconvert(mat,data[g],size1,size2,type); 
                printdata(&data[g],size1,1); 
 		autocorrelation(autocor,mat,data[g],size1,size2);
		check=comparespec(autocor,size1,size2,spectrum); 
		if(check==1)
		{
			matches[matchcounts]=data[g]; 
			matchcounts++;  
		}
	}

	//print min arrays and autocorrelation
	printf("size of search space %d \n",len);
	printf("spectrum matched \n");
        for(n=0; n<SIZESPEC; n++)
	{
		printf("%d  ",n); 
	} 
	printf("\n");  
	for(n=0; n<SIZESPEC; n++)
	{
		printf("%d ",spectrum[n]); 
	} 
	printf("\n"); 
	printf("Number of matches with spectrum %d \n",matchcounts); 
	printdata(matches,size1,matchcounts); 
	//printhex(mat,mins,size1,mincounts); 

}
//calculate boundary of 
int boundary(int mat[SIZE1][SIZE2],int * bp, int * wp)
{
	int boundb=0;
	int incr;  
	int nx,ny,nz, nzc,nc; 
	for(nz=0; nz<2; nz++)
	{
		if(nz==1) //set complement bit for calculating boundary 
			nzc=1; 
		else
			nzc=-1;
		nc=nzc*-1; 
 
		boundb=0; 			
		for(nx=0; nx<SIZE1; nx++)
		{
			for(ny=0; ny<SIZE2; ny++)
			{
				incr=boundb; 	
				//go through cases, and add up all contributions from boundaries
				if(mat[nx][ny]==nc)
				{
					//if(nx==0||nx==(SIZE1-1)) //boundary cases
					//	boundb++;
					//if(ny==0||ny==(SIZE2-1))
					//	boundb++;	
					if(nx>0 ) //if within boundaries
						if(mat[nx-1][ny]==nzc)
							boundb++; 
					if(nx<(SIZE1-1) ) //if within boundaries
						if(mat[nx+1][ny]==nzc)
							boundb++;
					if(ny>0 ) //if within boundaries
						if(mat[nx][ny-1]==nzc)
							boundb++; 
					if(ny<(SIZE2-1) ) //if within boundaries
						if(mat[nx][ny+1]==nzc)
							boundb++;
					
				}
				incr=boundb-incr; 
				//printf("nx %d, ny %d incremented %d \n",nx,ny,incr); 
			}	
		}
		if(nz==0)
		{
			(*bp)=boundb;
			//printf("boundary %d \n",boundb); 
		}	 
		else
		{
			(*wp)=boundb;
			//printf("boundary %d \n",boundb); 		
		}
	}
	return 0; 
}
//search for lex least matrices of equivalence classes which satisfy sidelobe condition, search for up to 5*7 matrices
int findmatrices(struct array * data, int mat[SIZE1][SIZE2],int matp[SIZE1][SIZE2], int matc[SIZE1][SIZE2],int autocor[2*SIZE1-1][2*SIZE2-1],int size1,int size2, int sidelobe,int * count,int * ni, int index,int type)
{
	//printf("current index %d \n",index); 
	for(ni[index]=0; ni[index]<(pow(2,size1)); ni[index]++)
	{
		if((size2-1)>index)
		{
			findmatrices(data,mat,matp,matc,autocor,size1,size2,sidelobe,count,ni,index+1,type); 
		}
		else
		{
			//printf("indices %d %d %d \n",ni[0],ni[1],ni[2]); 
			check(data,mat,matp,matc,autocor,size1,size2,sidelobe,count,ni,type); 
		}
	}
	return 0; 
}
int check(struct array * data,int mat[SIZE1][SIZE2],int matp[SIZE1][SIZE2], int matc[SIZE1][SIZE2],int autocor[2*SIZE1-1][2*SIZE2-1],int size1,int size2,int sidelobe,int * count,int * ni,int type)
{
	printf("entering check \n"); 
	struct array bob; 
	int n, swit; 
	//printf("size2 %d \n",size2); 
	for(n=0; n<size2; n++)
	{
		bob.rows[n]=ni[n]; 
		//printf("indices %d %d %d \n",ni[0],ni[1],ni[2]); 
	}
	//printdata(&bob,size1,1); 
	matrixconvert(mat,bob,size1,size2,type); 
	//displaymatrix(mat,size1,size2); 
	//if matrix is lex least and satisfies autocorrlation, add to final count
	if(lexleast(mat,matp,matc,size1,size2,type)==1)
	{
		if(autocorrelation(autocor,mat,bob,size1,size2)==1)
		{
			data[(*count)]=bob; 
			(*count)++; 
		}
	}
}
int printdatafull(struct array * data,int len, int matp[SIZE1][SIZE2],int autocor[2*SIZE1-1][2*SIZE2-1],int size1,int size2,FILE * fp,int type )
{
	printf("printing data full \n"); 
	int boundb; 
	int boundw;
	int n,n2,energy;  
	
	for(n=0; n<len; n++)
	{
		matrixconvert(matp,data[n],size1,size2,type);
		boundary(matp,&boundb,&boundw);
		for(n2=0; n2<size1; n2++)
		{
			fprintf(fp,"%d ",data[n].rows[n2]); 
		} 
		fprintf(fp,"\n"); 
		fprintf(fp,"boundary %d \n ",boundb); 
		//fprintf(fp,"boundary white %d \n ",boundw); 
 		autocorrelation(autocor,matp,data[n],size1,size2); 
		slenergy(autocor,size1,size2,&energy); 
		fprintf(fp,"sidelobe energy %d \n",energy); 
		fprintf(fp,"\n"); 
	}
}
//code searches equivalence classes of matrices selected for spectrum for optimal boundary properties, this is not conserved across equivalence class
int findminboundary(struct array * data,int mat[SIZE1][SIZE2],int matp[SIZE1][SIZE2],int autocor[2*SIZE1-1][2*SIZE2-1], int len, int size1,int size2, FILE * fp, int type)
{
	//printf("hello \n"); 
	struct array intm; 
	int n,n2,n3,n4,n5;
	int min=10000; 
	int x=0;
	int boundb;
	int boundw; 
	int mincounts=0; //use these to keep track of array
	struct array mins[50]; //store equivalent minimum arrays
	for(n=0; n<len; n++)
	{
		if(n%10000==0)
			printf(" %d \n",n); 		
		matrixconvert(mat,data[n],size1,size2,type); 
		
		//search equivalence class
		//printf("equivalence class \n"); 
		//printdata(&data[n],size1,1); 
		//displaymatrix(mat,size1,size2); 
		//printf(" \n"); 
                
		
		//for(n3=0; n3<2; n3++)
			for(n4=0; n4<2; n4++)
			{
				for(n5=0; n5<2; n5++)
				{
					identity(mat,matp,size1,size2); 
					/*if(n3==1)
					{
						comp(matp,size1,size2); 
					}*/
					if(n4==1)
					{
						the(matp,size1,size2); 
					}
					if(n5==1)
					{
						phi(matp,size1,size2); 
					}
					boundary(matp,&boundb,&boundw); 
					if(boundb<min)
					{
						min=boundb;
						mincounts=0;  
					}
					//displaymatrix(matp,size1,size2); 
					//printf("boundary %d %d \n",boundb,boundw); 
					if(boundb==min)
					{
						//calculate and store binary representation of particular matrix, not just lex least representation of equivalence class
						decconvert(matp,size1,size2,&intm);
						mins[mincounts]=intm; 
						mincounts++; 
					}
				}
			}
		//}
	}

	//print min arrays and autocorrelation
	printf("size of search space %d \n",len); 
	printf("minimum black boundary %d \n",min); 
	printf("min counts %d \n",mincounts); 
	printdata(mins,size1,mincounts);
	fprintf(fp,"Least boundary matrices \n"); 
	printdatafull(mins,mincounts,matp,autocor,size1,size2,fp,type); 
}
int findminenergy(struct array * data,int mat[SIZE1][SIZE2], int autocor[2*SIZE1-1][2*SIZE2-1], int len, int size1,int size2, FILE * fp,int type)
{
	//printf("hello \n"); 
	int n,n2;
	int min=10000; 
	int energy; 
	int x=0;
	int swit; 
	int mincounts=0; //use these to keep track of array
	struct array mins[50]; //store equivalent minimum arrays
	for(n=0; n<len; n++)
	{
		if(n%10000==0)
			printf(" %d \n",n); 		
		matrixconvert(mat,data[n],size1,size2,type); 
 		swit=autocorrelation(autocor,mat,data[n],size1,size2); 
		if(swit==0)
		{
			printf("matrix does not satisfy sidelobe criteria \n"); 
			printdata(&data[n],size1,1); 
		}
		energy=0;  
		slenergy(autocor,size1,size2,&energy); 
		if(energy<min)
		{
			min=energy;
			mincounts=0;  
		}
		if(energy==min)
		{
			mins[mincounts]=data[n]; 
			mincounts++; 
		}
	}

	//print min arrays and autocorrelation
	printf("size of search space %d \n",len); 
	printf("minimum sidelobe energy %d \n",min); 
	printdata(mins,size1,mincounts); 
	fprintf(fp,"minimum energy arrays \n"); 
	printdatafull(mins,mincounts,mat,autocor,size1,size2,fp,type); 
	//printhex(mat,mins,size1,mincounts); 

}
int findarray(struct array find,struct array * data, int len, int size1,int size2)
{
	//printf("hello \n"); 
	int n2,swit,n; 	

	for(n=0; n<len; n++)
	{
		swit=0; 
		for(n2=0; n2<size1; n2++)
		{
			if((data[n].rows[n2])!=(find.rows[n2]))
				swit=1; 
		}
		if(swit!=1)
		{
			printf("found matrix \n");
			printdata(&data[n],size1,1); 
			break; 
		} 
	}

	//printhex(mat,mins,size1,mincounts); 

}
int verifylexleast (struct array * data, int mat[SIZE1][SIZE2],int matp[SIZE1][SIZE2], int matc[SIZE1][SIZE2],int len,int size1,int size2,int type)
{
	//printf("hello \n"); 
	int n,n2;
	int lexcounts=0; //use these to keep track of array
	int autocor [2*SIZE1-1][2*SIZE2-1];  
	struct array bob; 
	struct array nll[1000]; //store non lex least arrays
	for(n=0; n<len; n++)
	{
		//printf("%d \n",n);
		if(n%100000==0)
			printf(" %d of len %d \n",n,len); 		
		matrixconvert(mat,data[n],size1,size2,type); 
		//displaymatrix(mat,size1,size2,type); 
 		if((lexleast(mat,matp,matc,size1,size2,type)==0))//||(autocorrelation(autocor,mat,bob,size1,size2)==0))
 		{
	 		//nll[lexcounts]=data[n]; 
	 		lexcounts++;		
 		}
		else
		{
			printdata(&data[n],size1,1); 
		}
	}
	//print min arrays and autocorrelation
	printf("size of search space %d \n",len); 
	//printf("number of non lex least matrices %d \n",lexcounts); 
	printf("number of lex least matrices %d \n",len-lexcounts); 
	//printdata(nll,size1,lexcounts); 
	//printhex(mat,mins,size1,mincounts); 
	
}

int readin(struct array * data,int * lex,char * text,int buff,int len,int * numbers,int size1,int size2)
{
	printf("reading data in \n"); 
	FILE *fp; 
	fp=fopen(FNAME,"r");
        if(fp==NULL)
        {
		printf("Null file pointer \n"); 
                return 1; 
        }
        openfile(data,lex,text,buff,len,numbers,size1,size2,fp);
	return 0; 
}
//combine files from parallel processing code
int readinmulti(struct array * data,int range,int * lex,char * text,int buff,int len,int * numbers,int size1,int size2)
{
	printf("reading data in \n"); 
	FILE *fp; 
	int n; 
	char filename[40]; 
	for(n=0; n<range; n++)
	{
		sprintf(filename,"RESULT%d.TXT",n); //select range of output files
		printf(filename); 
		printf("\n"); 
		fp=fopen(filename,"r");
        	if(fp==NULL)
        	{
			printf("Null file pointer \n"); 
                	return 1; 
        	}
        	openfile(data,lex,text,buff,len,numbers,size1,size2,fp);//number needs to be not set to 0 inbetween read ins
		fclose(fp); 
	}
	return 0; 
}
int openfile(struct array * data,int * lex,char * text,int buff,int len,int * numbers,int size1,int size2,FILE * fp)
{
	int swit2,count,n,val;
	//file IO
        fgets(&text[0],buff,fp); //get first two lines
        fgets(&text[0],buff,fp);  
         
	swit2=0; 
	while(1)
	{
		//code for locating set of values from line and extracting
		count=0; 
		while(count<SIZE1)
		{
                        //second method for simpler output format
                       
                        fscanf(fp,"%d",&val); 

                        if(feof(fp))
			{	
				swit2=1; 
				break;
			} 
        		lex[count]=val; 
			count++; 
		}	
                if(swit2==1)
		{
			break;
		}
		if(count==size1)  //if there are n numbers, include in final list                
		{
			for(n=0;n<count;n++)

				data[(*numbers)].rows[n]=lex[n]; 
			(*numbers)++; 
			if( (*numbers)>len-1)
			{
				printf("reached end of memory allocation"); 
				return 1; 
			}
			//if(numbers%1000==0)
			//{
			//	//printf("%d \n",numbers); 
			//}
		}
		//for(n=0; n<count; n++)
		//{
		//	printf("%d ",lex[n]); //output read in value
		//}
		//printf(" \n");
		fgets(&text[0],buff,fp); //go to next line
		
	}
	//printdata(data,size1,(*numbers)); 
	printf("read data in \n"); 
	printf("numbers %d \n",(*numbers)); 
	return 0; 

}

int main()
{
        int buff=1000;  //char buffer for getting data lines
        char text [1000];
	struct array *trash[20]; 
        int trashcount=0; 
	
	int len=700000; 
	int size1=SIZE1; //filled in my preprocessor directive
	int size2=SIZE2; 
	int type=TYPE; 
	struct array *data;  
 	getmemory( (void**)&data,len,trash,&trashcount); 
	int mat [SIZE1][SIZE2];
	int matp [SIZE1][SIZE2];
	int matc [SIZE1][SIZE2];     
        int autocor [2*SIZE1-1][2*SIZE2-1];      
        int spectrum [SIZESPEC]; 
	int sidelobe=MSL;
	int numbers=0; 

	char c[3]; 
        int  lex[6];
        int n,n3; 
        for(n=0; n<SIZESPEC; n++) //initialize to zero, set several low values to nonzero
	{
		spectrum[n]=0; 
	}

	readin(data,lex,text,buff,len,&numbers,size1,size2); 
	//readinmulti(data,PARS,lex,text,buff,len,&numbers,size1,size2); 
	
	//struct array find; 
	//find.rows[0]=1; 
	//find.rows[1]=45; 
	//find.rows[2]=71; 
	//find.rows[3]=31; 
	//find.rows[4]=19; 

	//matrixconvert(mat,find,size1,size2);
	//lexleast(mat,matp,matc,size1,size2);
        //findarray(find,data,len,size1,size2); 

	//printf("opening output file \n"); 
	//FILE * bob;
	//bob=fopen(OUTFILE,"w");
	//printf("opened out file \n");  
	//findminenergy(data,mat,autocor,numbers,size1,size2,bob); 
        //findminboundary(data,mat,matp,autocor,numbers,size1,size2,bob);
	//fclose(bob); 
 	verifylexleast(data,mat,matp,matc,numbers,size1,size2,type); 

	int ni[SIZE2]; 
	int num2=0; 
	int index=0; 
	//check(data,mat,matp,matc,autocor,size1,size2,sidelobe,numbers,ni,type); 
        //findmatrices(data,mat,matp,matc,autocor,size1,size2,sidelobe,&num2,ni,index,type); 
	//printdata(data,size1,num2); 
	//printf("found %d matrices \n",num2);
        //displayspectrum(data,mat,autocor,num2,size1,size2,spectrum,type);//takes care of all displaying functions        
	
	/*spectrum[0]=26; 
	spectrum[1]=22; 
	spectrum[2]=36;
	spectrum[3]=6;  
        findspectrum(data,mat,autocor,numbers,size1,size2,spectrum);*/ 	
	//printf("numbers %d \n",numbers); 


	return 0; 
}

