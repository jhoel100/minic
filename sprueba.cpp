float e,f;
float ee,ff;

f=15;
print(f);

int bfun(int c,int d){
   int i;
   for(i=0;i<3;i=i+1){
      print(i);
   }
   return (c+d);
}

e=12;
print(e);

int cfun(int m,int n){
   int ii;
   for(ii=3;ii>0;ii=ii-1){
      print(ii);
   }
   return (m-n);
}

f=40;
print(f);

do{
   f=f-1;
   print(f);
}while(f>38);

int dfun(int g,int h){
   int j;
   for(j=0;j<3;j=j+1){
      print(j);
   }
   return (g*h);
}

f=23;
print(f);

int afun(int k,int l){
   int jj;
   for(jj=0;jj<3;jj=jj+1){
      print(jj);
   }

   int auxiliar;

   if(l==0){
      auxiliar=0;
   }else{
      auxiliar=k/l;
   }
   
   return auxiliar;
}

int main(int x){
   int a,b;
   b=0;
   for(a=5;a<10;a=a+1){
      b=b+1;
   };

   afun(1,4);
   cfun(4,5);

   print(14);
   
   bfun(1,4);
   dfun(4,5);

   return 10;
}
chao