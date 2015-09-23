
%we need to code operators and the methods for generating a lex least
%matrix, we need to also generate integer representation based on rules
%from the code 

close all; 
clc; 


%lexs=[10   , 8,  9 ,22]; 
%lexs=[10,57,45,120,108,95]; 
lexs=[0,19,46,44,6]; 
width=7; 

s=size(lexs); 
s=s(1,2); 

s2=width; 

M=ones(s,width); 
for n3=1:1:s
     a=dec2bin(lexs(n3),width); 
     %disp(lexs(n3,n1)); 
     for n4=1:1:width
         val=str2num(a(n4));
         if(val==0)
            M(n3,n4)=0; 
         else
            M(n3,n4)=1;
         end
     end
end

disp(M);
%calculate integer representation
base=2^8;
sum2=0; 
for n3=1:1:s
    sum=0;
    for n2=1:1:width
        sum=M(n3,n2)*2^(width-n2)+sum; 
    end
    %disp(sum); 
    sum2=sum*base^(8-n3)+sum2; 
end
fprintf('%.15g \n',sum2); 
disp(' '); 
%disp(sum2); 

search=3; 
if(width==s)
    search=7; 
end

%makes most sense to group these in independent sections
for n=1:1:search
    
    %vertical reversal
    Mv=ones(s,width); 
    if(n==1 )
        disp('H');
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=M(n3,width+1-n2); 
            end
        end
    end

    %horizontal reversal
    if(n==2)
        disp('V');
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=M(s+1-n3,n2); 
            end
        end
        %disp(Mv);
    end
    
    %horizontal, vertical reversal
    if(n==3)
        disp('VH');
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=M(n3,width+1-n2); 
            end
        end
        Mprev=Mv; 
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=Mprev(s+1-n3,n2); 
            end
        end
        %disp(Mv);
    end
    
    
    %do transpose first, then other operations
    if(n>=4)  %test transpose operation, two methods
          %horizontal reversal
        if(n==4)
            disp('T');
        end
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=M(n2,n3); 
            end
        end
    end
    Mprev=Mv; 
    if(n==5 )
        disp('TH');
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=Mprev(n3,width+1-n2); 
            end
        end
        Mprev=Mv; %set equal so will apply horizontal reversal next cycle
    end

    %horizontal reversal
    if(n==6)
        disp('TV');
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=Mprev(s+1-n3,n2); 
            end
        end
    end
    
    %horizontal, vertical reversal
    if(n==7)
        disp('THV');
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=Mprev(n3,width+1-n2); 
            end
        end
        Mprev=Mv; 
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=Mprev(s+1-n3,n2); 
            end
        end
    end
    %end of operations that include transpose
    
    
    % print intermediate result of operation
    sum2=0; 
    for n3=1:1:s
        sum=0;
        for n2=1:1:width
            sum=Mv(n3,n2)*2^(width-n2)+sum; 
        end
        %disp(sum); 
        sum2=sum*base^(8-n3)+sum2; 
    end
    disp(' '); 
    disp(' '); 
    disp('before'); 
    disp(Mv); 
    fprintf('%.15g \n',sum2); 
    

%use complement, vertical phase shift or horizontal phase shift to produce
%lex least matrix

%using the three operations, there is only one way to guartantee that
%matrix is lex least, and that is combination which sets diagonal to zero

     %Mv=M; 
     if(Mv(1,1)==0)
         if(Mv(1,2)==0)
             if(Mv(2,1)==0)
                 C=0;
                 P=0; 
                 T=0; 
             else
                 C=0;
                 P=0; 
                 T=1;
             end
         else
             if(Mv(2,1)==0)
                 C=0;
                 P=1; 
                 T=0; 
             else
                 C=0;
                 P=1; 
                 T=1;
             end
         end
     else
         if(Mv(1,2)==0)
             if(Mv(2,1)==0)
                 C=1;
                 P=1; 
                 T=1; 
             else
                 C=1;
                 P=1; 
                 T=0;
             end
         else
             if(Mv(2,1)==0)
                 C=1;
                 P=0; 
                 T=1; 
             else
                 C=1;
                 P=0; 
                 T=0;
             end
         end
     end
     
     %disp('choices'); 
     %disp(C);
     %disp(P); 
     %disp(T); 
         
     for n3=1:1:s
        for n2=1:1:width
            if(C==1)
                if(Mv(n3,n2)==1)
                    Mv(n3,n2)=0; 
                else
                    Mv(n3,n2)=1; 
                end
            end
            if( (P==1) && (mod(n2,2)==0) )
                if(Mv(n3,n2)==1)
                    Mv(n3,n2)=0; 
                else
                    Mv(n3,n2)=1; 
                end
            end
            if( (T==1) && (mod(n3,2)==0) )
                if(Mv(n3,n2)==1)
                    Mv(n3,n2)=0; 
                else
                    Mv(n3,n2)=1; 
                end
            end           
        end
     end
     disp(Mv); 

%display output in integer form
    sum2=0; 
    for n3=1:1:s
        sum=0;
        for n2=1:1:width
            sum=Mv(n3,n2)*2^(width-n2)+sum; 
        end
        %disp(sum); 
        sum2=sum*base^(8-n3)+sum2; 
        %disp(sum); 
    end
    fprintf('%.15g \n',sum2); 

end
