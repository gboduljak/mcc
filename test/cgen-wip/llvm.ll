; ModuleID = 'test'


 


declare external ccc  void @free(i8*)    


declare external ccc  i8* @malloc(i32)    


declare external ccc  void @printf(i8*, ...)    


declare external ccc  void @scanf(i8*, ...)    


@str.0 =  unnamed_addr  constant [3 x i8] c"%f\00"


define external ccc  double @f(double  %x_0)    {
entry_0:
  %0 = alloca double 
  store  double %x_0, double* %0 
  %1 = load  double, double* %0 
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([3 x i8], [3 x i8]* @str.0, i32 0, i32 0), double  %1)  
  %2 = load  double, double* %0 
  ret double %2 
}


@str.1 =  unnamed_addr  constant [4 x i8] c"%f\0a\00"


define external ccc  i32 @main()    {
entry_0:
  %0 = alloca double 
  store  double 1.212000e1, double* %0 
  %1 = alloca double 
  %2 = load  double, double* %0 
  %3 =  call ccc  double  @f(double  %2)  
  store  double %3, double* %1 
  %4 = load  double, double* %1 
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @str.1, i32 0, i32 0), double  %4)  
  ret i32 0 
}