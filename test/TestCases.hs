module TestCases where

miniPrograms :: [String]
miniPrograms =
  [ "./test/tests-cases/mini/array-max/",
    "./test/tests-cases/mini/array-sum/pointers/",
    "./test/tests-cases/mini/array-sum/recursive/",
    "./test/tests-cases/mini/fibo/",
    "./test/tests-cases/mini/fizzbuzz/",
    "./test/tests-cases/mini/hello/",
    "./test/tests-cases/mini/leap-year/",
    "./test/tests-cases/mini/palindrome/"
  ]

jmoragPrograms :: [String]
jmoragPrograms =
  [ "./test/tests-cases/jmorag/add1/", 
    "./test/tests-cases/jmorag/arith1/", 
    "./test/tests-cases/jmorag/arith2/",
    "./test/tests-cases/jmorag/arith3/",
    "./test/tests-cases/jmorag/bitwise-ops/",
    "./test/tests-cases/jmorag/fib/",
    "./test/tests-cases/jmorag/local2/",
    "./test/tests-cases/jmorag/gcd1/",
    "./test/tests-cases/jmorag/gcd2/",
    "./test/tests-cases/jmorag/arr1/",
    "./test/tests-cases/jmorag/float1/",   
    "./test/tests-cases/jmorag/float2/",
    "./test/tests-cases/jmorag/float3/",
    "./test/tests-cases/jmorag/for1/",
    "./test/tests-cases/jmorag/for2/",
    "./test/tests-cases/jmorag/func1/",
    "./test/tests-cases/jmorag/func2/",
    "./test/tests-cases/jmorag/func3/",
    "./test/tests-cases/jmorag/func4/",
    "./test/tests-cases/jmorag/func5/",
    "./test/tests-cases/jmorag/func6/",
    "./test/tests-cases/jmorag/func7/",
    "./test/tests-cases/jmorag/func8/",
    "./test/tests-cases/jmorag/func9/",
    "./test/tests-cases/jmorag/global1/",
    "./test/tests-cases/jmorag/global2/",
    "./test/tests-cases/jmorag/global3/",
    "./test/tests-cases/jmorag/if1/",
    "./test/tests-cases/jmorag/if2/",
    "./test/tests-cases/jmorag/if3/",
    "./test/tests-cases/jmorag/if4/",
    "./test/tests-cases/jmorag/if5/",
    "./test/tests-cases/jmorag/if6/",
    "./test/tests-cases/jmorag/struct1/",
    "./test/tests-cases/jmorag/var1/",
    "./test/tests-cases/jmorag/var2/",
    "./test/tests-cases/jmorag/while1/",
    "./test/tests-cases/jmorag/while2/",
    "./test/tests-cases/jmorag/while3/",
    "./test/tests-cases/jmorag/str1/",
    "./test/tests-cases/jmorag/ptr1/",
    "./test/tests-cases/jmorag/ptr2/",
    "./test/tests-cases/jmorag/ptr3/",
    "./test/tests-cases/jmorag/linked-list/"
  ]

gabrijelPrograms :: [String]
gabrijelPrograms =
  [ -- "./test/tests-cases/gabrijel/array1/", 
    "./test/tests-cases/gabrijel/ptr-matrix1/" -- commented programs compile but cause test runner to fail :P
    --"./test/tests-cases/gabrijel/sophisticated-linear-algebra/"
  ]

sortingPrograms :: [String]
sortingPrograms =
  [ "./test/tests-cases/sorting/counting-sort/",
    "./test/tests-cases/sorting/insertion-sort/",
    "./test/tests-cases/sorting/merge-sort/",
    "./test/tests-cases/sorting/quick-sort/",
    "./test/tests-cases/sorting/radix-sort/"
  ]

dynamicProgrammingPrograms :: [String]
dynamicProgrammingPrograms =
  ["./test/tests-cases/dynamic-programming/coin-changing/"]