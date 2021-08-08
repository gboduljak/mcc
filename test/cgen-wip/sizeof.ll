; ModuleID = 'sizeof.c'


 


declare external ccc  void @free(i8*)    


declare external ccc  i8* @malloc(i32)    


declare external ccc  void @printf(i8*, ...)    


declare external ccc  void @scanf(i8*, ...)    


@str.0 =  unnamed_addr  constant [4 x i8] c"%d\0a\00"


@str.1 =  unnamed_addr  constant [4 x i8] c"%d\0a\00"


define external ccc  i32 @main()    {
entry_0:
  %0 = alloca i32 
  %1 = alloca i32 
  %2 = alloca i32 
  store  i64 300, i32* %1 
  %3 = load  i32, i32* %1 
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @str.0, i32 0, i32 0), i32  %3)  
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @str.1, i32 0, i32 0), i64  8)  
  %4 = load  i32, i32* %0 
  ret i32 %4 
}