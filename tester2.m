function [se]=tester2(lexs,sidelobe,width)


%close all; 
%clear all; 
%clc; 

%lexs=[1,3,117,25,105];
%lexs=[25,13,21,32,79];
%lexs=[9,13,10,48,79];
%lexs=[2 ,6 ,98,86,27,44]; 
%lexs=[10, 26,   9, 31,  70,  48];
 %lexs= [10,    57 ,   45,  120,  108,   95]; 


s=size(lexs); 
s=s(1,2); 

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
   
%disp(lexs); 
disp(M); 

%Now we want to determin the autocorrelation

 auto=zeros(2*(s)+1,2*width+1); 
 x=-width:1:width; 
 y=(-s):1:(s); 
 for nxt=-(s):1:(s)
      for nyt=-width:1:width
           for nx=1:1:s
                for ny=1:1:(width)
                     if( ((nx-nxt)>0)&& ((ny-nyt)>0) && ((nx-nxt)<(s+1)) && ((ny-nyt)<(width+1)) )
                         auto(nxt+s+1,nyt+width+1)=M(nx,ny)*M(nx-nxt,ny-nyt)+auto(nxt+s+1,nyt+width+1); 
                     end
                end
           end
           %disp(7*s); 
           if( ((auto(nxt+s+1,nyt+width+1)>(sidelobe)) || (auto(nxt+s+1,nyt+width+1)<(-sidelobe))) && auto(nxt+s+1,nyt+width+1)~=(width*s) )
               %disp('Does not satisfy sidelobe condition'); 
               se=-1; % this is way function will flag does not satisfy condition
               %return; 
           end
      end
 end
 %figure; 
 surf(x,y,auto); 
 axis equal; 
 disp(auto); 
 disp(sidelobe); 
 
 s4=size(auto);
 %disp(auto); 
 auto=abs(auto); 
 s1=size(auto); 
 s1=s1(1,1)*s1(1,2); 
 auto=reshape(auto,s1,1); 
 bins=-1:1:30; 
 counts=hist(auto,bins); 
 %figure; 
 %bar(bins,counts); 
 dist=[0,0,1,2,3,4,5,6,7,8,9,10]; 
  
  %perimeter is zeros, subtract away
 %disp(s4); 
 %disp(counts(2)); 
 counts(2)=counts(2)-s4(1,1)*2-(s4(1,2)-2)*2; %subtract away false zeros
 
 %disp('spectrum'); 
 %disp(dist); 
 %disp(counts(1:12));  
 
 %disp('sidelobe energy'); 
 se=0; 
 for n=1:6
     se=se+counts(n)*(n-2)^2; 
 end
 se=se/2; 
 return; 
