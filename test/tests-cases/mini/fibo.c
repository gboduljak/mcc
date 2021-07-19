int fibonacci_series(int num)
{
   if ( num == 0 )
     return 0;
   else if ( num == 1 )
     return 1;
   else
     return ( fibonacci_series(num-1) + fibonacci_series(num-2) );
}

int main()
{
   int count;
   int c;
   int i;
   
   c = 0;
   printf("Enter number of terms:");
   scanf("%d",&count);
 
   printf("\nFibonacci series:\n");
   for (i = 1; i <= count ; i=i+1 )
   {
      printf("%d\n", fibonacci_series(c));
      c = c+1; 
   }
 
   return 0;
}