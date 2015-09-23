
close all; 
clear all;
clc; 

fp=fopen('RESULT(55).TXT','r'); 

fgets(fp);
fgets(fp); 

data=textscan(fp,'%d %d %d %d %d'); 
data=cell2mat(data); 

fclose(fp); 

s=size(data); 
s=s(1,1); 

for n=1:1:s
    [islexleast,degenerate]=lexleast2(data(n,1:5),5,0);     
    if(islexleast==0)
        disp('Not lex least'); 
        disp(data(n,:)); 
    end
    disp(data(n,1:5)); 

end