
clc; 
clear all; 
close all;

%data=importdata('RESULT.TXT'); 

%for now just get 20 values if 2, 33 if 3, 46 if 4
disp('hello'); 

bob=fopen('RESULT(47).TXT','r'); 

%scan in 7 characters, next 1 or 2 will be number

fscanf(bob,'%c',1); %at start of line or start of file

lexs=zeros(4,1000); 
smin=10000; 
lexmin=zeros(4,1); 
ed=0; 

for n1=1:1:1000

    %if(n1==2)
    %    joe=fscanf(bob,'%c',40); 
    %    disp(joe); 
    %end
       
    for n=1:1:7
        fscanf(bob,'%c',5);  %a in 100s, b in 10s, c in 1s
        a=fscanf(bob,'%c',1); 
        b=fscanf(bob,'%c',1); 
        c=fscanf(bob,'%c',1); 
        %need to convert char into number
    
        if(c==' ')
            break; %no longer reading in numbers, exit loop
        end
        
        if((isempty(a))&&(isempty(b))&&(isempty(c)))
            ed=1; 
            break; 
        end
        
        val=str2num(c); 
        if(a~=' ')
            val=val+100*str2num(a); 
        end
        if(b~=' ')
            val=val+10*str2num(b); 
        end
        lexs(n,n1)=val; 
    end 
    
    if(ed==1); %when gets to this condition end read in
         break; 
    end
    
    %convert lex into a matrix
    M=ones(n-1,7); 
    for n3=1:1:(n-1)
        a=dec2bin(lexs(n3,n1),7); 
        %disp(lexs(n3,n1)); 
        for n4=1:1:7
            val=str2num(a(1,n4));
            if(val==0)
                M(n3,n4)=1; 
            else
                M(n3,n4)=-1;
            end
        end
    end
    %disp(M);

    read=0; 
    
    if(n==3)
        read=20; 
    end
    if(n==4)
        read=33;
    end
    if(n==5)
        read=46; 
    end
    
    %we want to compute and display the autocorrelation of the 4 by 7
    %matrices
    
    auto=zeros(2*(n-1)+1,14+1); 
    x=-7:1:7; 
    y=-(n-1):1:(n-1); 
    if(n==5)
        for nxt=-(n-1):1:(n-1)
            for nyt=-7:1:7
                for nx=1:1:(n-1)
                    for ny=1:1:(7)
                        if( ((nx-nxt)>0)&& ((ny-nyt)>0) && ((nx-nxt)<(n)) && ((ny-nyt)<8) )
                            auto(nxt+(n-1)+1,nyt+7+1)=M(nx,ny)*M(nx-nxt,ny-nyt)+auto(nxt+(n-1)+1,nyt+7+1); 
                        end
                        %if( auto(nxt+(n-1)+1,nyt+7+1)>4 || auto(nxt+(n-1)+1,nyt+7+1)<(-4) )
                        %    disp('something wrong, exceeds sidelobe criteria'); 
                        %    disp(lexs(:,n1)); 
                        %end
                    end
                end
            end
        end
        %figure; 
        %surf(x,y,auto); 
        %axis equal; 
        %disp(auto); 
        auto=abs(auto); 
        s=size(auto); 
        s=s(1,1)*s(1,2); 
        auto=reshape(auto,s,1); 
        bins=-1:1:30; 
        counts=hist(auto,bins); 
        %figure; 
        %bar(bins,counts);
        %disp(lexs(:,n1)); 
        %disp('spectrum'); 
        %disp(counts(1:6)); 
        %disp('sidelobe energy'); 
        se=0; 
        for n=1:6
            se=se+counts(n)*(n-2)^2; 
        end
        se=se/2; 
        disp(se); 
        if(se<smin)
            smin=se; 
            lexmin=lexs(:,n1); 
        end
        
    end
    
    if(read==0)
        break; %reached end of file 
    end
    
    sidelobes=fscanf(bob,'%d',read); 
    fgets(bob); %get new line
    %disp(sidelobes); 
    
end

disp('optimal array with minimum sidelobe energy'); 
disp(lexmin); 
disp(smin); 



