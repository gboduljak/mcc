; ModuleID = 'test'


 


declare external ccc  void @free(i8*)    


declare external ccc  i8* @malloc(i32)    


declare external ccc  void @printf(i8*, ...)    


declare external ccc  void @scanf(i8*, ...)    


@str.0 =  unnamed_addr  constant [5 x i8] c"meme\00"


@str.1 =  unnamed_addr  constant [10 x i8] c"tech lead\00"


define external ccc  i32 @main()    {
entry_0:
  %0 = alloca i32 
  %1 = alloca i32 
  %2 = alloca i32* 
  store  i32 1, i32* %0 
  store  i32 2, i32* %1 
  %3 = icmp ne i32 0, 0 
  br i1 %3, label %if.conseq_0, label %if.alt_0 
if.conseq_0:
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([5 x i8], [5 x i8]* @str.0, i32 0, i32 0))  
  br label %if.merge_0 
if.alt_0:
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([10 x i8], [10 x i8]* @str.1, i32 0, i32 0))  
  br label %if.merge_0 
if.merge_0:
  %4 = alloca i32 
  %5 = load  i32, i32* %4 
  ret i32 %5 
}