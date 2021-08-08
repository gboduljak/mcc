; ModuleID = 'loop.c'


 


declare external ccc  void @free(i8*)    


declare external ccc  i8* @malloc(i32)    


declare external ccc  void @printf(i8*, ...)    


declare external ccc  void @scanf(i8*, ...)    


@str.0 =  unnamed_addr  constant [4 x i8] c"%d\0a\00"


define external ccc  i32 @main()    {
entry_0:
  %0 = alloca i32 
  %1 = alloca i32 
  store  i32 0, i32* %1 
  %2 = load  i32, i32* %1 
  %3 = icmp slt i32 %2, 10 
  %4 = icmp ne i1 %3, 0 
  br i1 %4, label %if.conseq_0, label %if.alt_0 
if.conseq_0:
  br label %loop.body_0 
loop.body_0:
  %5 = load  i32, i32* %1 
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @str.0, i32 0, i32 0), i32  %5)  
  %6 = load  i32, i32* %1 
  %7 = add   i32 %6, 1 
  store  i32 %7, i32* %1 
  %8 = load  i32, i32* %1 
  %9 = icmp slt i32 %8, 10 
  %10 = icmp ne i1 %9, 0 
  br i1 %10, label %loop.body_0, label %loop.exit_0 
loop.exit_0:
  br label %if.merge_0 
if.alt_0:
  br label %if.merge_0 
if.merge_0:
  %11 = load  i32, i32* %0 
  ret i32 %11 
}