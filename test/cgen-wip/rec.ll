; ModuleID = 'rec.c'


 


declare external ccc  void @free(i8*)    


declare external ccc  i8* @malloc(i32)    


declare external ccc  void @printf(i8*, ...)    


declare external ccc  void @scanf(i8*, ...)    


@str.0 =  unnamed_addr  constant [3 x i8] c"%d\00"


define external ccc  i32 @main()    {
entry_0:
  %0 = alloca i32 
  %1 =  call ccc  i32  @rec(i32  1)  
  store  i32 %1, i32* %0 
  %2 = load  i32, i32* %0 
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([3 x i8], [3 x i8]* @str.0, i32 0, i32 0), i32  %2)  
  %3 = alloca i32 
  %4 = load  i32, i32* %3 
  ret i32 %4 
}


@str.1 =  unnamed_addr  constant [10 x i8] c"non base\0a\00"


@str.2 =  unnamed_addr  constant [6 x i8] c"base\0a\00"


define external ccc  i32 @rec(i32  %arg_0)    {
entry_0:
  %0 = alloca i32 
  store  i32 %arg_0, i32* %0 
  %1 = load  i32, i32* %0 
  %2 = icmp ne i32 %1, 0 
  br i1 %2, label %if.conseq_0, label %if.alt_0 
if.conseq_0:
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([10 x i8], [10 x i8]* @str.1, i32 0, i32 0))  
  %3 =  call ccc  i32  @rec(i32  0)  
  ret i32 %3 
if.alt_0:
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([6 x i8], [6 x i8]* @str.2, i32 0, i32 0))  
  ret i32 1 
if.merge_0:
  %4 = alloca i32 
  %5 = load  i32, i32* %4 
  ret i32 %5 
}