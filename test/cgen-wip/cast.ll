; ModuleID = 'cast.c'


 


declare external ccc  void @free(i8*)    


declare external ccc  i8* @malloc(i32)    


declare external ccc  void @printf(i8*, ...)    


declare external ccc  void @scanf(i8*, ...)    


@str.0 =  unnamed_addr  constant [3 x i8] c"%f\00"


define external ccc  i32 @main()    {
entry_0:
  %0 = alloca i32 
  %1 = alloca i8 
  %2 = alloca i8 
  %3 = alloca i8 
  store  i8 97, i8* %1 
  store  i8 98, i8* %2 
  %4 = load  i8, i8* %1 
  %5 = load  i8, i8* %2 
  %6 = add   i8 %4, %5 
  store  i8 %6, i8* %3 
  %7 = load  i8, i8* %3 
  %8 = zext i8 %7 to i32  
  %9 = sitofp i32 %8 to double 
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([3 x i8], [3 x i8]* @str.0, i32 0, i32 0), double  %9)  
  ret i32 0 
}