%code returns 1 if lex least, zero if otherwise
%if there is a degenerate lexographically least array, return 1, else 0

function [islexleast,degenerate]=lexleast2(lexs,width,verb)

s=size(lexs); 
s=s(1,2); 
degenerate=0; 

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

%if matrix has one in any of the corner elements, because of the structure
%of our search, and the condition that we are looking for the
%lexographically least array with the upper elements normalized, we can
%remove these elements from consideration
if(M(1,1)==1)
    islexleast=0; 
    return; 
end
if(M(1,2)==1)
    islexleast=0; 
    return; 
end
if(M(2,1)==1)
    islexleast=0; 
    return; 
end


%disp(M);
%calculate integer representation
base=2^8;
sum2=0; 
for n3=1:1:s
    sum=0;
    for n2=1:1:width
        sum=M(n3,n2)*2^(width-n2)+sum; 
    end
    if(verb==1)
        %disp(sum);
    end
    sum2=sum*base^(8-n3)+sum2; 
end
sumref=sum2; 
%fprintf('%.15g \n',sum2); 
%disp(sum2); 

search=3; 
if(width==s)
    search=7; 
end

for n=1:1:search
    
    %vertical reversal
  %vertical reversal
    Mv=ones(s,width); 
    if(n==1 )
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=M(n3,width+1-n2); 
            end
        end
        %disp(Mv);
        Mprev=Mv; %set equal so will apply horizontal reversal next cycle
    end

    %horizontal reversal
    if(n==2)
        
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=Mprev(s+1-n3,n2); 
            end
        end
        %disp(Mv);
    end
    
    %horizontal, vertical reversal
    if(n==3)
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
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=M(n2,n3); 
            end
        end
    end
    Mprev=Mv; 
    if(n==5 )
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=Mprev(n3,width+1-n2); 
            end
        end
        Mprev=Mv; %set equal so will apply horizontal reversal next cycle
    end

    %horizontal reversal
    if(n==6)
        for n3=1:1:s
            for n2=1:1:width
                Mv(n3,n2)=Mprev(s+1-n3,n2); 
            end
        end
    end
    
    %horizontal, vertical reversal
    if(n==7)
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
     %disp(Mv); 

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
    if(verb==1)
        fprintf('%.15g \n',sum2);
    end
    if(sum2==sumref)
        degenerate=1; 
    end
    if(sum2<sumref)
        islexleast=0; 
        return; 
    end
    
end

islexleast=1; 
return; 
