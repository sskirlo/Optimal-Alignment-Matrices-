

%code reads in entries from both files, and finds the differences between
%them

bob1=fopen('RESULT(37).TXT','r');
fgets(bob1); 
fgets(bob1); 
M1=textscan(bob1,'%d %d %d'); 
fclose(bob1); 
M1=cell2mat(M1); 
s1=size(M1); 
s1=s1(1,1); 

bob1=fopen('correct(37).txt','r');
fgets(bob1); 
fgets(bob1); 
M2=textscan(bob1,'%d %d %d'); 
fclose(bob1); 
M2=cell2mat(M2); 
s2=size(M2); 
s2=s2(1,1); 

disp('Size 1'); 
disp(s1); 
disp('Size 2'); 
disp(s2);

%need to search for M2 matrices in M1, if don't find print out
searchcount=0; 
for n1=1:1:s2
   search=M2(n1,:);
   for n2=1:1:s1
       tr=1; 
       for n3=1:1:3
            if(M1(n2,n3)~=search(n3))
                tr=0; 
            end
       end
       if(tr==1)
           break; 
       end
   end
   if(tr==1)
       %found matrix
   else
       disp(search);
       searchcount=searchcount+1; 
   end
    
end
disp(searchcount); 