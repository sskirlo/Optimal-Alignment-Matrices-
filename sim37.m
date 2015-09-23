close all; 
clear all; 
clc; 


sidelobe=2;
size=4;
width=4; 
%M=[10,57,45,120,108,95]; 
%M=[2,27,112,90,49,40]; 
count=1; 
countmins=1; 
mins=zeros(size,10);
opts=zeros(size,30); 
minsse=10000; 

for n4=0:1:(2^(width)-1)
for n3=0:1:(2^(width)-1)
for n2=0:1:(2^(width)-1)
for n1=0:1:(2^(width)-1)

    M=[n1,n2,n3,n4];  
    
    [islexleast,degen]=lexleast2(M,width,0); 
    %check particular case to see whats wrong with code
    %if((n1==0) && (n2==2) && (n3==1))
    %    disp('012'); 
    %    disp(islexleast); 
    %end
    
    if(islexleast==1)
        sse=tester2(M,sidelobe,width); 
        if(sse==-1)
            continue; 
        else
            disp(M);
            opts(:,count)=M; 
            if(degen==1)
                disp('degenerate'); 
            end
            count=count+1;
        end
        if(sse<minsse)
            minsse=sse; 
            countmins=1;
        end
        if(sse==minsse)
            mins(:,countmins)=M; 
            countmins=countmins+1; 
        end
    end
   

end
end
end
end

disp('Number of optimal matrices'); 
disp(count-1); 
for n2=1:1:(count-1)
    %tester(opts(:,n2).'); 
end

disp('Number of minimum sidelobe energy matrices'); 
disp(countmins-1); 
for n2=1:1:(countmins-1)
    %tester(mins(:,n2).'); 
end