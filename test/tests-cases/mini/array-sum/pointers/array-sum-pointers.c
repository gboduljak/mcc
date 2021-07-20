// #include<stdio.h>
int main()
{
   int array[5];
   int i;
   int sum;
   int *ptr;

   printf("\nEnter array elements (5 integer values):");
   for(i=0;i<5;i=i+1)
      scanf("%d",&array[i]);

   /* array is equal to base address
    * array = &array[0] */
   ptr = &array[0];

   for(i=0;i<5;i=i+1) 
   {
      //*ptr refers to the value at address
      sum = sum + *ptr;
      ptr=ptr + 1;
   }

   printf("\nThe sum is: %d",sum);
}