; ModuleID = 'if.c'


 


declare external ccc  void @free(i8*)    


declare external ccc  i8* @malloc(i32)    


declare external ccc  void @printf(i8*, ...)    


declare external ccc  void @scanf(i8*, ...)    


@str.0 =  unnamed_addr  constant [3 x i8] c"%d\00"


@str.1 =  unnamed_addr  constant [6 x i8] c"meme\0a\00"


@str.2 =  unnamed_addr  constant [11 x i8] c"tech lead\0a\00"


@str.3 =  unnamed_addr  constant [3 x i8] c"%d\00"


@str.4 =  unnamed_addr  constant [19 x i8] c"not 0 is indeed 1\0a\00"


@str.5 =  unnamed_addr  constant [19 x i8] c"not 1 is indeed 0\0a\00"


@str.6 =  unnamed_addr  constant [29 x i8] c"negative is indeed negative\0a\00"


define external ccc  i32 @main()    {
entry_0:
  %0 = alloca i32 
  %1 = alloca i32 
  %2 = alloca i32 
  %3 = alloca i32* 
  store  i32 1, i32* %1 
  store  i32 2, i32* %2 
  %4 = load  i32, i32* %1 
  %5 = load  i32, i32* %2 
  %6 = add   i32 %4, %5 
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([3 x i8], [3 x i8]* @str.0, i32 0, i32 0), i32  %6)  
  %7 = load  i32, i32* %1 
  %8 = load  i32, i32* %2 
  %9 = add   i32 %7, %8 
  %10 = add   i32 1, 2 
  %11 = icmp sle i32 %9, %10 
  %12 = icmp ne i1 %11, 0 
  br i1 %12, label %if.conseq_0, label %if.alt_0 
if.conseq_0:
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([6 x i8], [6 x i8]* @str.1, i32 0, i32 0))  
  br label %if.merge_0 
if.alt_0:
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([11 x i8], [11 x i8]* @str.2, i32 0, i32 0))  
  br label %if.merge_0 
if.merge_0:
  %13 = alloca double 
  store  double 1.200000e1, double* %13 
  %14 = alloca i32 
  store  i32 0, i32* %14 
  %15 = load  i32, i32* %1 
  %16 = xor i32 %15, 1 
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([3 x i8], [3 x i8]* @str.3, i32 0, i32 0), i32  %16)  
  %17 = load  i32, i32* %14 
  %18 = xor i32 %17, 1 
  %19 = icmp ne i32 %18, 0 
  br i1 %19, label %if.conseq_1, label %if.alt_1 
if.conseq_1:
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([19 x i8], [19 x i8]* @str.4, i32 0, i32 0))  
  br label %if.merge_1 
if.alt_1:
  br label %if.merge_1 
if.merge_1:
  %20 = load  i32, i32* %1 
  %21 = xor i32 %20, 1 
  %22 = icmp eq i32 %21, 0 
  %23 = icmp ne i1 %22, 0 
  br i1 %23, label %if.conseq_2, label %if.alt_2 
if.conseq_2:
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([19 x i8], [19 x i8]* @str.5, i32 0, i32 0))  
  br label %if.merge_2 
if.alt_2:
  br label %if.merge_2 
if.merge_2:
  %24 = load  i32, i32* %1 
  %25 = load  i32, i32* %2 
  %26 = sub   i32 %24, %25 
  %27 = sub   i32 0, %26 
  %28 = icmp sgt i32 %27, 0 
  %29 = icmp ne i1 %28, 0 
  br i1 %29, label %if.conseq_3, label %if.alt_3 
if.conseq_3:
   call ccc  void (i8*, ...) @printf(i8*  getelementptr inbounds ([29 x i8], [29 x i8]* @str.6, i32 0, i32 0))  
  br label %if.merge_3 
if.alt_3:
  br label %if.merge_3 
if.merge_3:
  %30 = load  i32, i32* %0 
  ret i32 %30 
}