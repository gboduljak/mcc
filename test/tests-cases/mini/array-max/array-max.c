// #include <stdio.h>
 
/*  This is our function to find the largest
 * element in the array arr[]
 */
int largest_element(int* arr, int num)
{
    int i;
    int max_element;
    
    // Initialization to the first array element
    max_element = arr[0];
 
    /* Here we are comparing max_element with
     * all other elements of array to store the 
     * largest element in the max_element variable
     */
    for (i = 1; i < num; i = i + 1)         
        if (arr[i] > max_element)
            max_element = arr[i];
 
    return max_element;
}
 
int main()
{    
    int n;
    int arr[7];

    arr[0] = 1;
    arr[1] = 24;
    arr[2] = 145;
    arr[3] = 20;
    arr[4] = 8;
    arr[5] = -101;
    arr[6] = 300;

    n = sizeof(arr)/sizeof(arr[0]);
    printf("Largest element of array is %d", largest_element(&arr[0], n));
    return 0;
}