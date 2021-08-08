; ModuleID = 'strukt.c'
source_filename = "strukt.c"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx11.0.0"

%struct.moj_strukt = type { [10 x i32] }

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

; Function Attrs: noinline nounwind optnone ssp uwtable
define dso_local void @f(%struct.moj_strukt* noalias sret(%struct.moj_strukt) align 4 %0, %struct.moj_strukt* byval(%struct.moj_strukt) align 8 %1) #0 {
  %3 = bitcast %struct.moj_strukt* %0 to i8*
  %4 = bitcast %struct.moj_strukt* %1 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 4 %3, i8* align 8 %4, i64 40, i1 false)
  ret void
}

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #1

; Function Attrs: noinline nounwind optnone ssp uwtable
define dso_local i32 @main() #0 {
  %1 = alloca %struct.moj_strukt, align 8
  %2 = alloca %struct.moj_strukt, align 4
  %3 = getelementptr inbounds %struct.moj_strukt, %struct.moj_strukt* %1, i32 0, i32 0
  %4 = getelementptr inbounds [10 x i32], [10 x i32]* %3, i64 0, i64 0
  store i32 0, i32* %4, align 4
  %5 = getelementptr inbounds %struct.moj_strukt, %struct.moj_strukt* %1, i32 0, i32 0
  %6 = getelementptr inbounds [10 x i32], [10 x i32]* %5, i64 0, i64 1
  store i32 1, i32* %6, align 4
  %7 = getelementptr inbounds %struct.moj_strukt, %struct.moj_strukt* %1, i32 0, i32 0
  %8 = getelementptr inbounds [10 x i32], [10 x i32]* %7, i64 0, i64 2
  store i32 2, i32* %8, align 4
  call void @f(%struct.moj_strukt* sret(%struct.moj_strukt) align 4 %2, %struct.moj_strukt* byval(%struct.moj_strukt) align 8 %1)
  %9 = getelementptr inbounds %struct.moj_strukt, %struct.moj_strukt* %2, i32 0, i32 0
  %10 = getelementptr inbounds [10 x i32], [10 x i32]* %9, i64 0, i64 0
  %11 = load i32, i32* %10, align 4
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i64 0, i64 0), i32 %11)
  ret i32 0
}

declare i32 @printf(i8*, ...) #2

attributes #0 = { noinline nounwind optnone ssp uwtable "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nofree nosync nounwind willreturn }
attributes #2 = { "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"Homebrew clang version 12.0.1"}
