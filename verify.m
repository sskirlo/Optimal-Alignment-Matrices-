
fp=fopen('RESULT55.TXT','r'); 
fgets(fp); 
fgets(fp); 
data=textscan(fp,'%d %d %d %d %d'); 
data=cell2mat(data); 
s=size(data); 
sz=s(1,1); 
len=s(1,2); 
count=0; 
for n=1:1:sz
   [swit,joe]=lexleast2(data(n,1:len),len,0);  
   if(swit==0)
       disp('Not lex least'); 
       disp(data(n,1:len)); 
   else
       count=count+1; 
   end
    
end
disp('Number of lex least matrices compared to number searched'); 
disp(count);
disp(sz); 
fclose(fp); 