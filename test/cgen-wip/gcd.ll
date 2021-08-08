; ModuleID = 'gcd.c'


 


declare external ccc  void @free(i8*)    


declare external ccc  i8* @malloc(i32)    


declare external ccc  void @printf(i8*, ...)    


declare external ccc  void @scanf(i8*, ...)    


define external ccc  i32 @gcd(i32  %a_0, i32  %b_0)    {
entry_0:
  %0 = alloca i32 
  %1 = alloca i32 
  store  i32 %a_0, i32* %1 
  %2 = alloca i32 
  store  i32 %b_0, i32* %2 
  %3 = load  i32, i32* %1 
  %4 = load  i32, i32* %2 
  %5 = icmp ne i32 %3, %4 
  %6 = icmp ne i1 %5, 0 
  br i1 %6, label %if.conseq_0, label %if.alt_0 
if.conseq_0:
  br label %loop.body_0 
loop.body_0:
  %7 = load  i32, i32* %1 
  %8 = load  i32, i32* %2 
  %9 = icmp sgt i32 %7, %8 
  %10 = icmp ne i1 %9, 0 
  br i1 %10, label %if.conseq_1, label %if.alt_1 
if.conseq_1:
  %11 = load  i32, i32* %1 
  %12 = load  i32, i32* %2 
  %13 = sub   i32 %11, %12 
  store  i32 %13, i32* %1 
  br label %if.merge_1 
if.alt_1:
  %14 = load  i32, i32* %2 
  %15 = load  i32, i32* %1 
  %16 = sub   i32 %14, %15 
  store  i32 %16, i32* %2 
  br label %if.merge_1 
if.merge_1:
  %17 = load  i32, i32* %1 
  %18 = load  i32, i32* %2 
  %19 = icmp ne i32 %17, %18 
  %20 = icmp ne i1 %19, 0 
  br i1 %20, label %loop.body_0, label %loop.exit_0 
loop.exit_0:
  br label %if.merge_0 
if.alt_0:
  br label %if.merge_0 
if.merge_0:
  %21 = load  i32, i32* %1 
  ret i32 %21 
}


@str.0 =  unnamed_addr  constant [4 x i8] c"%d\0a\00"


@str.1 =  unnamed_addr  constant [4 x i8] c"%d\0a\00"


@str.2 =  unnamed_addr  constant [4 x i8] c"%d\0a\00"


define external ccc  i32 @main()    {
entry_0:
  %0 = alloca i32 
  %1 =  call ccc  i32  @gcd(i32  2, i32  14)  
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @str.0, i32 0, i32 0), i32  %1)  
  %2 =  call ccc  i32  @gcd(i32  3, i32  15)  
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @str.1, i32 0, i32 0), i32  %2)  
  %3 =  call ccc  i32  @gcd(i32  99, i32  121)  
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @str.2, i32 0, i32 0), i32  %3)  
  ret i32 0 
}