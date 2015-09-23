
%convert hexidecimal value decimal value
%clc; 
%close all; 
%clear all; 


%M=hex2bin('008D4D');
%M=hex2bin('07795FCB859');
% M=hex2bin('02055F06D8C'); 
%'0454CB43D';
%'0442EA4B9';
%'01B2B1C'
%'02B1B1C'
dim=5;
len=5; 
G=dec2bin(hex2dec('012C5C4')); 
s=size(G); 
s=s(1,2);
M=zeros(1,s); 
for n=1:1:s
    M(1,n)=str2num(G(1,n));
end

%assemble starting at back
Mp=zeros(dim,len); 
count1=dim; 
count2=len; 

%work backwards through number, 
for n=1:1:s
    Mp(count1,count2)=M(s-n+1);
    if(count1>0)
        count1=count1-1;  
    end
    if(count1==0)
        count1=dim; 
        count2=count2-1; 
    end
    if(count2==0)
        break; 
    end
end
%transpose to make lex least
Mp=Mp.';
disp(Mp);
vals=zeros(1,dim); 
%convert to decimal
for n2=1:1:dim
    for n1=1:1:len
        vals(n2)=Mp(n2,len+1-n1)*2^(n1-1)+vals(n2); 
        %disp(vals(n2)); 
    end
end
disp(vals);
tester(vals,len); 