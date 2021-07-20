// #include <stdio.h>

int sum_array_elements(int* arr, int n);

int main()
{
   int array[7];
   array[0] = 1;
   array[1] = 2;
   array[2] = 3;
   array[3] = 4;
   array[4] = 5;
   array[5] = 6;
   array[6] = 7;
   int sum;
   sum = sum_array_elements(&array[0], 6);
   printf("\nSum of array elements is:%d",sum);
   return 0;
}

int sum_array_elements(int* arr, int n) {
   if (n < 0) {
     //base case:
     return 0;
   } else{
     //Recursion: calling itself
     return arr[n] + sum_array_elements(arr, n-1);
    }
}