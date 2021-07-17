{-# OPTIONS_GHC -w #-}
module Parser.Generated.Parser where
import qualified Lexer.Lexeme as L
import qualified Parser.Ast as P
import Data.Text (pack)
import Prelude hiding (fst, snd)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24
	= HappyTerminal (L.Lexeme)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,578) ([0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,49152,7,0,64,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,512,0,0,0,0,2048,0,0,0,0,8,0,0,32768,0,0,0,0,2112,0,0,0,1984,1,0,0,0,0,0,0,0,1984,0,0,0,0,0,0,0,0,1984,4,0,0,8192,0,0,0,0,32,0,0,0,0,288,0,0,0,0,0,0,0,8192,0,0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,7,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,36863,3302,4288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2128,49150,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,32831,3072,4288,0,65280,135,49164,16,0,0,0,0,0,0,128,0,0,0,32768,0,0,0,0,128,0,0,0,32831,3072,4288,0,16128,128,49164,16,0,32831,3072,4288,0,16128,128,49164,16,0,32768,0,0,0,65280,135,49164,16,0,0,8,3072,0,0,2048,0,12,0,0,8,3072,0,0,2048,0,12,0,32831,3072,4288,0,16128,128,49164,16,0,32831,3072,4288,0,0,2320,49150,15,0,0,1,0,0,0,2064,49150,15,0,16384,0,0,0,16128,384,49164,16,0,32831,3072,4288,0,0,0,0,0,0,32831,3072,4288,0,16128,128,49164,16,0,32831,3072,4288,0,16128,128,49164,16,0,32831,3072,4288,0,16128,128,49164,16,0,32831,3072,4288,0,16128,128,49164,16,0,32831,3072,4288,0,16128,128,49164,16,0,32831,3072,4288,0,16128,128,49164,16,0,32831,3072,4288,0,16128,128,49164,16,0,32831,3072,4288,0,16128,128,49164,16,0,32831,3072,4288,0,8192,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,36862,12,0,0,65032,3727,0,0,2048,4094,12,0,0,65032,3999,0,0,2048,36862,15,0,0,15880,3072,0,0,2048,62,12,0,0,15880,3072,0,0,2048,62,12,0,0,15880,3087,0,0,2048,3902,12,0,0,8,3072,0,0,2048,0,12,0,0,8,3072,0,0,2048,56,12,0,0,14344,3072,0,0,6160,49150,15,0,4096,65032,4031,0,0,2064,49150,15,0,8192,1,0,0,0,0,0,0,0,0,0,0,0,16128,128,49164,16,0,0,0,0,0,0,64,0,0,0,4096,65033,4031,0,0,2320,49150,15,0,4096,65033,4031,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,36863,3298,4288,0,65280,57999,49164,16,0,32831,3072,4288,0,0,2048,0,12,0,32831,3072,4288,0,0,0,0,0,0,0,0,0,0,0,2064,49150,15,0,16384,0,0,0,0,0,0,0,0,0,256,0,0,65280,57999,49164,16,0,32831,3072,4288,0,0,256,0,0,0,0,0,0,0,65280,57999,49164,16,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","program","construct_list","construct","structdecl","funcdecl","vardecl","vardecl_list","formals","formal","block","block_statements","statement","expr","opt_expr","type","primitive_type","struct_type","stars","array_sizes","actuals","includes","int","double","string","char","null","ident","tint","tdouble","tchar","tvoid","struct","return","'='","','","';'","'('","')'","'{'","'}'","'['","']'","for","while","if","else","'+'","'-'","'*'","'/'","'%'","'=='","'!='","'<'","'<='","'>'","'>='","'&&'","'||'","'!'","'&'","'|'","'^'","'.'","'->'","sizeof","include","eof","%eof"]
        bit_start = st Prelude.* 72
        bit_end = (st Prelude.+ 1) Prelude.* 72
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..71]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (4) = happyGoto action_3
action_0 (24) = happyGoto action_2
action_0 _ = happyReduce_78

action_1 (24) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (70) = happyShift action_5
action_2 (5) = happyGoto action_4
action_2 _ = happyReduce_2

action_3 (72) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (31) = happyShift action_14
action_4 (32) = happyShift action_15
action_4 (33) = happyShift action_16
action_4 (34) = happyShift action_17
action_4 (35) = happyShift action_18
action_4 (71) = happyShift action_19
action_4 (6) = happyGoto action_7
action_4 (7) = happyGoto action_8
action_4 (8) = happyGoto action_9
action_4 (9) = happyGoto action_10
action_4 (18) = happyGoto action_11
action_4 (19) = happyGoto action_12
action_4 (20) = happyGoto action_13
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (27) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_79

action_7 _ = happyReduce_3

action_8 _ = happyReduce_5

action_9 _ = happyReduce_4

action_10 _ = happyReduce_6

action_11 (30) = happyShift action_23
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (21) = happyGoto action_22
action_12 _ = happyReduce_72

action_13 (21) = happyGoto action_21
action_13 _ = happyReduce_72

action_14 _ = happyReduce_67

action_15 _ = happyReduce_68

action_16 _ = happyReduce_69

action_17 _ = happyReduce_70

action_18 (30) = happyShift action_20
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_1

action_20 (42) = happyShift action_27
action_20 _ = happyReduce_71

action_21 (52) = happyShift action_26
action_21 _ = happyReduce_66

action_22 (52) = happyShift action_26
action_22 _ = happyReduce_65

action_23 (40) = happyShift action_25
action_23 (22) = happyGoto action_24
action_23 _ = happyReduce_74

action_24 (39) = happyShift action_36
action_24 (44) = happyShift action_37
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (31) = happyShift action_14
action_25 (32) = happyShift action_15
action_25 (33) = happyShift action_16
action_25 (34) = happyShift action_17
action_25 (35) = happyShift action_31
action_25 (41) = happyShift action_35
action_25 (11) = happyGoto action_32
action_25 (12) = happyGoto action_33
action_25 (18) = happyGoto action_34
action_25 (19) = happyGoto action_12
action_25 (20) = happyGoto action_13
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_73

action_27 (31) = happyShift action_14
action_27 (32) = happyShift action_15
action_27 (33) = happyShift action_16
action_27 (34) = happyShift action_17
action_27 (35) = happyShift action_31
action_27 (9) = happyGoto action_28
action_27 (10) = happyGoto action_29
action_27 (18) = happyGoto action_30
action_27 (19) = happyGoto action_12
action_27 (20) = happyGoto action_13
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_12

action_29 (31) = happyShift action_14
action_29 (32) = happyShift action_15
action_29 (33) = happyShift action_16
action_29 (34) = happyShift action_17
action_29 (35) = happyShift action_31
action_29 (43) = happyShift action_47
action_29 (9) = happyGoto action_46
action_29 (18) = happyGoto action_30
action_29 (19) = happyGoto action_12
action_29 (20) = happyGoto action_13
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (30) = happyShift action_45
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (30) = happyShift action_44
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (38) = happyShift action_42
action_32 (41) = happyShift action_43
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_13

action_34 (30) = happyShift action_41
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (42) = happyShift action_40
action_35 (13) = happyGoto action_39
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_10

action_37 (25) = happyShift action_38
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (45) = happyShift action_52
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_8

action_40 (14) = happyGoto action_51
action_40 _ = happyReduce_17

action_41 _ = happyReduce_15

action_42 (31) = happyShift action_14
action_42 (32) = happyShift action_15
action_42 (33) = happyShift action_16
action_42 (34) = happyShift action_17
action_42 (35) = happyShift action_31
action_42 (12) = happyGoto action_50
action_42 (18) = happyGoto action_34
action_42 (19) = happyGoto action_12
action_42 (20) = happyGoto action_13
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (42) = happyShift action_40
action_43 (13) = happyGoto action_49
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_71

action_45 (22) = happyGoto action_24
action_45 _ = happyReduce_74

action_46 _ = happyReduce_11

action_47 (39) = happyShift action_48
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_7

action_49 _ = happyReduce_9

action_50 _ = happyReduce_14

action_51 (25) = happyShift action_57
action_51 (26) = happyShift action_58
action_51 (27) = happyShift action_59
action_51 (28) = happyShift action_60
action_51 (29) = happyShift action_61
action_51 (30) = happyShift action_62
action_51 (31) = happyShift action_14
action_51 (32) = happyShift action_15
action_51 (33) = happyShift action_16
action_51 (34) = happyShift action_17
action_51 (35) = happyShift action_31
action_51 (36) = happyShift action_63
action_51 (40) = happyShift action_64
action_51 (42) = happyShift action_40
action_51 (43) = happyShift action_65
action_51 (46) = happyShift action_66
action_51 (47) = happyShift action_67
action_51 (48) = happyShift action_68
action_51 (51) = happyShift action_69
action_51 (52) = happyShift action_70
action_51 (63) = happyShift action_71
action_51 (64) = happyShift action_72
action_51 (69) = happyShift action_73
action_51 (9) = happyGoto action_53
action_51 (13) = happyGoto action_54
action_51 (15) = happyGoto action_55
action_51 (16) = happyGoto action_56
action_51 (18) = happyGoto action_30
action_51 (19) = happyGoto action_12
action_51 (20) = happyGoto action_13
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_75

action_53 _ = happyReduce_26

action_54 _ = happyReduce_20

action_55 _ = happyReduce_18

action_56 (37) = happyShift action_87
action_56 (39) = happyShift action_88
action_56 (44) = happyShift action_89
action_56 (50) = happyShift action_90
action_56 (51) = happyShift action_91
action_56 (52) = happyShift action_92
action_56 (53) = happyShift action_93
action_56 (54) = happyShift action_94
action_56 (55) = happyShift action_95
action_56 (56) = happyShift action_96
action_56 (57) = happyShift action_97
action_56 (58) = happyShift action_98
action_56 (59) = happyShift action_99
action_56 (60) = happyShift action_100
action_56 (61) = happyShift action_101
action_56 (62) = happyShift action_102
action_56 (64) = happyShift action_103
action_56 (65) = happyShift action_104
action_56 (66) = happyShift action_105
action_56 (67) = happyShift action_106
action_56 (68) = happyShift action_107
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_27

action_58 _ = happyReduce_28

action_59 _ = happyReduce_30

action_60 _ = happyReduce_29

action_61 _ = happyReduce_31

action_62 (40) = happyShift action_86
action_62 _ = happyReduce_32

action_63 (25) = happyShift action_57
action_63 (26) = happyShift action_58
action_63 (27) = happyShift action_59
action_63 (28) = happyShift action_60
action_63 (29) = happyShift action_61
action_63 (30) = happyShift action_62
action_63 (40) = happyShift action_64
action_63 (51) = happyShift action_69
action_63 (52) = happyShift action_70
action_63 (63) = happyShift action_71
action_63 (64) = happyShift action_72
action_63 (69) = happyShift action_73
action_63 (16) = happyGoto action_84
action_63 (17) = happyGoto action_85
action_63 _ = happyReduce_63

action_64 (25) = happyShift action_57
action_64 (26) = happyShift action_58
action_64 (27) = happyShift action_59
action_64 (28) = happyShift action_60
action_64 (29) = happyShift action_61
action_64 (30) = happyShift action_62
action_64 (31) = happyShift action_14
action_64 (32) = happyShift action_15
action_64 (33) = happyShift action_16
action_64 (34) = happyShift action_17
action_64 (35) = happyShift action_31
action_64 (40) = happyShift action_64
action_64 (51) = happyShift action_69
action_64 (52) = happyShift action_70
action_64 (63) = happyShift action_71
action_64 (64) = happyShift action_72
action_64 (69) = happyShift action_73
action_64 (16) = happyGoto action_82
action_64 (18) = happyGoto action_83
action_64 (19) = happyGoto action_12
action_64 (20) = happyGoto action_13
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_16

action_66 (40) = happyShift action_81
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (40) = happyShift action_80
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (40) = happyShift action_79
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (25) = happyShift action_57
action_69 (26) = happyShift action_58
action_69 (27) = happyShift action_59
action_69 (28) = happyShift action_60
action_69 (29) = happyShift action_61
action_69 (30) = happyShift action_62
action_69 (40) = happyShift action_64
action_69 (51) = happyShift action_69
action_69 (52) = happyShift action_70
action_69 (63) = happyShift action_71
action_69 (64) = happyShift action_72
action_69 (69) = happyShift action_73
action_69 (16) = happyGoto action_78
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (25) = happyShift action_57
action_70 (26) = happyShift action_58
action_70 (27) = happyShift action_59
action_70 (28) = happyShift action_60
action_70 (29) = happyShift action_61
action_70 (30) = happyShift action_62
action_70 (40) = happyShift action_64
action_70 (51) = happyShift action_69
action_70 (52) = happyShift action_70
action_70 (63) = happyShift action_71
action_70 (64) = happyShift action_72
action_70 (69) = happyShift action_73
action_70 (16) = happyGoto action_77
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (25) = happyShift action_57
action_71 (26) = happyShift action_58
action_71 (27) = happyShift action_59
action_71 (28) = happyShift action_60
action_71 (29) = happyShift action_61
action_71 (30) = happyShift action_62
action_71 (40) = happyShift action_64
action_71 (51) = happyShift action_69
action_71 (52) = happyShift action_70
action_71 (63) = happyShift action_71
action_71 (64) = happyShift action_72
action_71 (69) = happyShift action_73
action_71 (16) = happyGoto action_76
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (25) = happyShift action_57
action_72 (26) = happyShift action_58
action_72 (27) = happyShift action_59
action_72 (28) = happyShift action_60
action_72 (29) = happyShift action_61
action_72 (30) = happyShift action_62
action_72 (40) = happyShift action_64
action_72 (51) = happyShift action_69
action_72 (52) = happyShift action_70
action_72 (63) = happyShift action_71
action_72 (64) = happyShift action_72
action_72 (69) = happyShift action_73
action_72 (16) = happyGoto action_75
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (40) = happyShift action_74
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (25) = happyShift action_57
action_74 (26) = happyShift action_58
action_74 (27) = happyShift action_59
action_74 (28) = happyShift action_60
action_74 (29) = happyShift action_61
action_74 (30) = happyShift action_62
action_74 (31) = happyShift action_14
action_74 (32) = happyShift action_15
action_74 (33) = happyShift action_16
action_74 (34) = happyShift action_17
action_74 (35) = happyShift action_31
action_74 (40) = happyShift action_64
action_74 (51) = happyShift action_69
action_74 (52) = happyShift action_70
action_74 (63) = happyShift action_71
action_74 (64) = happyShift action_72
action_74 (69) = happyShift action_73
action_74 (16) = happyGoto action_137
action_74 (18) = happyGoto action_138
action_74 (19) = happyGoto action_12
action_74 (20) = happyGoto action_13
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (44) = happyShift action_89
action_75 (67) = happyShift action_106
action_75 (68) = happyShift action_107
action_75 _ = happyReduce_53

action_76 (44) = happyShift action_89
action_76 (67) = happyShift action_106
action_76 (68) = happyShift action_107
action_76 _ = happyReduce_51

action_77 (44) = happyShift action_89
action_77 (67) = happyShift action_106
action_77 (68) = happyShift action_107
action_77 _ = happyReduce_54

action_78 (44) = happyShift action_89
action_78 (67) = happyShift action_106
action_78 (68) = happyShift action_107
action_78 _ = happyReduce_52

action_79 (25) = happyShift action_57
action_79 (26) = happyShift action_58
action_79 (27) = happyShift action_59
action_79 (28) = happyShift action_60
action_79 (29) = happyShift action_61
action_79 (30) = happyShift action_62
action_79 (40) = happyShift action_64
action_79 (51) = happyShift action_69
action_79 (52) = happyShift action_70
action_79 (63) = happyShift action_71
action_79 (64) = happyShift action_72
action_79 (69) = happyShift action_73
action_79 (16) = happyGoto action_136
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (25) = happyShift action_57
action_80 (26) = happyShift action_58
action_80 (27) = happyShift action_59
action_80 (28) = happyShift action_60
action_80 (29) = happyShift action_61
action_80 (30) = happyShift action_62
action_80 (40) = happyShift action_64
action_80 (51) = happyShift action_69
action_80 (52) = happyShift action_70
action_80 (63) = happyShift action_71
action_80 (64) = happyShift action_72
action_80 (69) = happyShift action_73
action_80 (16) = happyGoto action_135
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (25) = happyShift action_57
action_81 (26) = happyShift action_58
action_81 (27) = happyShift action_59
action_81 (28) = happyShift action_60
action_81 (29) = happyShift action_61
action_81 (30) = happyShift action_62
action_81 (40) = happyShift action_64
action_81 (51) = happyShift action_69
action_81 (52) = happyShift action_70
action_81 (63) = happyShift action_71
action_81 (64) = happyShift action_72
action_81 (69) = happyShift action_73
action_81 (16) = happyGoto action_84
action_81 (17) = happyGoto action_134
action_81 _ = happyReduce_63

action_82 (37) = happyShift action_87
action_82 (41) = happyShift action_133
action_82 (44) = happyShift action_89
action_82 (50) = happyShift action_90
action_82 (51) = happyShift action_91
action_82 (52) = happyShift action_92
action_82 (53) = happyShift action_93
action_82 (54) = happyShift action_94
action_82 (55) = happyShift action_95
action_82 (56) = happyShift action_96
action_82 (57) = happyShift action_97
action_82 (58) = happyShift action_98
action_82 (59) = happyShift action_99
action_82 (60) = happyShift action_100
action_82 (61) = happyShift action_101
action_82 (62) = happyShift action_102
action_82 (64) = happyShift action_103
action_82 (65) = happyShift action_104
action_82 (66) = happyShift action_105
action_82 (67) = happyShift action_106
action_82 (68) = happyShift action_107
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (41) = happyShift action_132
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (37) = happyShift action_87
action_84 (44) = happyShift action_89
action_84 (50) = happyShift action_90
action_84 (51) = happyShift action_91
action_84 (52) = happyShift action_92
action_84 (53) = happyShift action_93
action_84 (54) = happyShift action_94
action_84 (55) = happyShift action_95
action_84 (56) = happyShift action_96
action_84 (57) = happyShift action_97
action_84 (58) = happyShift action_98
action_84 (59) = happyShift action_99
action_84 (60) = happyShift action_100
action_84 (61) = happyShift action_101
action_84 (62) = happyShift action_102
action_84 (64) = happyShift action_103
action_84 (65) = happyShift action_104
action_84 (66) = happyShift action_105
action_84 (67) = happyShift action_106
action_84 (68) = happyShift action_107
action_84 _ = happyReduce_64

action_85 (39) = happyShift action_131
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (25) = happyShift action_57
action_86 (26) = happyShift action_58
action_86 (27) = happyShift action_59
action_86 (28) = happyShift action_60
action_86 (29) = happyShift action_61
action_86 (30) = happyShift action_62
action_86 (40) = happyShift action_64
action_86 (41) = happyShift action_130
action_86 (51) = happyShift action_69
action_86 (52) = happyShift action_70
action_86 (63) = happyShift action_71
action_86 (64) = happyShift action_72
action_86 (69) = happyShift action_73
action_86 (16) = happyGoto action_128
action_86 (23) = happyGoto action_129
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (25) = happyShift action_57
action_87 (26) = happyShift action_58
action_87 (27) = happyShift action_59
action_87 (28) = happyShift action_60
action_87 (29) = happyShift action_61
action_87 (30) = happyShift action_62
action_87 (40) = happyShift action_64
action_87 (51) = happyShift action_69
action_87 (52) = happyShift action_70
action_87 (63) = happyShift action_71
action_87 (64) = happyShift action_72
action_87 (69) = happyShift action_73
action_87 (16) = happyGoto action_127
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_19

action_89 (25) = happyShift action_57
action_89 (26) = happyShift action_58
action_89 (27) = happyShift action_59
action_89 (28) = happyShift action_60
action_89 (29) = happyShift action_61
action_89 (30) = happyShift action_62
action_89 (40) = happyShift action_64
action_89 (51) = happyShift action_69
action_89 (52) = happyShift action_70
action_89 (63) = happyShift action_71
action_89 (64) = happyShift action_72
action_89 (69) = happyShift action_73
action_89 (16) = happyGoto action_126
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (25) = happyShift action_57
action_90 (26) = happyShift action_58
action_90 (27) = happyShift action_59
action_90 (28) = happyShift action_60
action_90 (29) = happyShift action_61
action_90 (30) = happyShift action_62
action_90 (40) = happyShift action_64
action_90 (51) = happyShift action_69
action_90 (52) = happyShift action_70
action_90 (63) = happyShift action_71
action_90 (64) = happyShift action_72
action_90 (69) = happyShift action_73
action_90 (16) = happyGoto action_125
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (25) = happyShift action_57
action_91 (26) = happyShift action_58
action_91 (27) = happyShift action_59
action_91 (28) = happyShift action_60
action_91 (29) = happyShift action_61
action_91 (30) = happyShift action_62
action_91 (40) = happyShift action_64
action_91 (51) = happyShift action_69
action_91 (52) = happyShift action_70
action_91 (63) = happyShift action_71
action_91 (64) = happyShift action_72
action_91 (69) = happyShift action_73
action_91 (16) = happyGoto action_124
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (25) = happyShift action_57
action_92 (26) = happyShift action_58
action_92 (27) = happyShift action_59
action_92 (28) = happyShift action_60
action_92 (29) = happyShift action_61
action_92 (30) = happyShift action_62
action_92 (40) = happyShift action_64
action_92 (51) = happyShift action_69
action_92 (52) = happyShift action_70
action_92 (63) = happyShift action_71
action_92 (64) = happyShift action_72
action_92 (69) = happyShift action_73
action_92 (16) = happyGoto action_123
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (25) = happyShift action_57
action_93 (26) = happyShift action_58
action_93 (27) = happyShift action_59
action_93 (28) = happyShift action_60
action_93 (29) = happyShift action_61
action_93 (30) = happyShift action_62
action_93 (40) = happyShift action_64
action_93 (51) = happyShift action_69
action_93 (52) = happyShift action_70
action_93 (63) = happyShift action_71
action_93 (64) = happyShift action_72
action_93 (69) = happyShift action_73
action_93 (16) = happyGoto action_122
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (25) = happyShift action_57
action_94 (26) = happyShift action_58
action_94 (27) = happyShift action_59
action_94 (28) = happyShift action_60
action_94 (29) = happyShift action_61
action_94 (30) = happyShift action_62
action_94 (40) = happyShift action_64
action_94 (51) = happyShift action_69
action_94 (52) = happyShift action_70
action_94 (63) = happyShift action_71
action_94 (64) = happyShift action_72
action_94 (69) = happyShift action_73
action_94 (16) = happyGoto action_121
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (25) = happyShift action_57
action_95 (26) = happyShift action_58
action_95 (27) = happyShift action_59
action_95 (28) = happyShift action_60
action_95 (29) = happyShift action_61
action_95 (30) = happyShift action_62
action_95 (40) = happyShift action_64
action_95 (51) = happyShift action_69
action_95 (52) = happyShift action_70
action_95 (63) = happyShift action_71
action_95 (64) = happyShift action_72
action_95 (69) = happyShift action_73
action_95 (16) = happyGoto action_120
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (25) = happyShift action_57
action_96 (26) = happyShift action_58
action_96 (27) = happyShift action_59
action_96 (28) = happyShift action_60
action_96 (29) = happyShift action_61
action_96 (30) = happyShift action_62
action_96 (40) = happyShift action_64
action_96 (51) = happyShift action_69
action_96 (52) = happyShift action_70
action_96 (63) = happyShift action_71
action_96 (64) = happyShift action_72
action_96 (69) = happyShift action_73
action_96 (16) = happyGoto action_119
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (25) = happyShift action_57
action_97 (26) = happyShift action_58
action_97 (27) = happyShift action_59
action_97 (28) = happyShift action_60
action_97 (29) = happyShift action_61
action_97 (30) = happyShift action_62
action_97 (40) = happyShift action_64
action_97 (51) = happyShift action_69
action_97 (52) = happyShift action_70
action_97 (63) = happyShift action_71
action_97 (64) = happyShift action_72
action_97 (69) = happyShift action_73
action_97 (16) = happyGoto action_118
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (25) = happyShift action_57
action_98 (26) = happyShift action_58
action_98 (27) = happyShift action_59
action_98 (28) = happyShift action_60
action_98 (29) = happyShift action_61
action_98 (30) = happyShift action_62
action_98 (40) = happyShift action_64
action_98 (51) = happyShift action_69
action_98 (52) = happyShift action_70
action_98 (63) = happyShift action_71
action_98 (64) = happyShift action_72
action_98 (69) = happyShift action_73
action_98 (16) = happyGoto action_117
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (25) = happyShift action_57
action_99 (26) = happyShift action_58
action_99 (27) = happyShift action_59
action_99 (28) = happyShift action_60
action_99 (29) = happyShift action_61
action_99 (30) = happyShift action_62
action_99 (40) = happyShift action_64
action_99 (51) = happyShift action_69
action_99 (52) = happyShift action_70
action_99 (63) = happyShift action_71
action_99 (64) = happyShift action_72
action_99 (69) = happyShift action_73
action_99 (16) = happyGoto action_116
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (25) = happyShift action_57
action_100 (26) = happyShift action_58
action_100 (27) = happyShift action_59
action_100 (28) = happyShift action_60
action_100 (29) = happyShift action_61
action_100 (30) = happyShift action_62
action_100 (40) = happyShift action_64
action_100 (51) = happyShift action_69
action_100 (52) = happyShift action_70
action_100 (63) = happyShift action_71
action_100 (64) = happyShift action_72
action_100 (69) = happyShift action_73
action_100 (16) = happyGoto action_115
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (25) = happyShift action_57
action_101 (26) = happyShift action_58
action_101 (27) = happyShift action_59
action_101 (28) = happyShift action_60
action_101 (29) = happyShift action_61
action_101 (30) = happyShift action_62
action_101 (40) = happyShift action_64
action_101 (51) = happyShift action_69
action_101 (52) = happyShift action_70
action_101 (63) = happyShift action_71
action_101 (64) = happyShift action_72
action_101 (69) = happyShift action_73
action_101 (16) = happyGoto action_114
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (25) = happyShift action_57
action_102 (26) = happyShift action_58
action_102 (27) = happyShift action_59
action_102 (28) = happyShift action_60
action_102 (29) = happyShift action_61
action_102 (30) = happyShift action_62
action_102 (40) = happyShift action_64
action_102 (51) = happyShift action_69
action_102 (52) = happyShift action_70
action_102 (63) = happyShift action_71
action_102 (64) = happyShift action_72
action_102 (69) = happyShift action_73
action_102 (16) = happyGoto action_113
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (25) = happyShift action_57
action_103 (26) = happyShift action_58
action_103 (27) = happyShift action_59
action_103 (28) = happyShift action_60
action_103 (29) = happyShift action_61
action_103 (30) = happyShift action_62
action_103 (40) = happyShift action_64
action_103 (51) = happyShift action_69
action_103 (52) = happyShift action_70
action_103 (63) = happyShift action_71
action_103 (64) = happyShift action_72
action_103 (69) = happyShift action_73
action_103 (16) = happyGoto action_112
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (25) = happyShift action_57
action_104 (26) = happyShift action_58
action_104 (27) = happyShift action_59
action_104 (28) = happyShift action_60
action_104 (29) = happyShift action_61
action_104 (30) = happyShift action_62
action_104 (40) = happyShift action_64
action_104 (51) = happyShift action_69
action_104 (52) = happyShift action_70
action_104 (63) = happyShift action_71
action_104 (64) = happyShift action_72
action_104 (69) = happyShift action_73
action_104 (16) = happyGoto action_111
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (25) = happyShift action_57
action_105 (26) = happyShift action_58
action_105 (27) = happyShift action_59
action_105 (28) = happyShift action_60
action_105 (29) = happyShift action_61
action_105 (30) = happyShift action_62
action_105 (40) = happyShift action_64
action_105 (51) = happyShift action_69
action_105 (52) = happyShift action_70
action_105 (63) = happyShift action_71
action_105 (64) = happyShift action_72
action_105 (69) = happyShift action_73
action_105 (16) = happyGoto action_110
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (30) = happyShift action_109
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (30) = happyShift action_108
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_57

action_109 _ = happyReduce_55

action_110 (44) = happyShift action_89
action_110 (50) = happyShift action_90
action_110 (51) = happyShift action_91
action_110 (52) = happyShift action_92
action_110 (53) = happyShift action_93
action_110 (54) = happyShift action_94
action_110 (55) = happyShift action_95
action_110 (56) = happyShift action_96
action_110 (57) = happyShift action_97
action_110 (58) = happyShift action_98
action_110 (59) = happyShift action_99
action_110 (60) = happyShift action_100
action_110 (64) = happyShift action_103
action_110 (67) = happyShift action_106
action_110 (68) = happyShift action_107
action_110 _ = happyReduce_37

action_111 (44) = happyShift action_89
action_111 (50) = happyShift action_90
action_111 (51) = happyShift action_91
action_111 (52) = happyShift action_92
action_111 (53) = happyShift action_93
action_111 (54) = happyShift action_94
action_111 (55) = happyShift action_95
action_111 (56) = happyShift action_96
action_111 (57) = happyShift action_97
action_111 (58) = happyShift action_98
action_111 (59) = happyShift action_99
action_111 (60) = happyShift action_100
action_111 (64) = happyShift action_103
action_111 (66) = happyShift action_105
action_111 (67) = happyShift action_106
action_111 (68) = happyShift action_107
action_111 _ = happyReduce_36

action_112 (44) = happyShift action_89
action_112 (50) = happyShift action_90
action_112 (51) = happyShift action_91
action_112 (52) = happyShift action_92
action_112 (53) = happyShift action_93
action_112 (54) = happyShift action_94
action_112 (55) = happyShift action_95
action_112 (56) = happyShift action_96
action_112 (57) = happyShift action_97
action_112 (58) = happyShift action_98
action_112 (59) = happyShift action_99
action_112 (60) = happyShift action_100
action_112 (67) = happyShift action_106
action_112 (68) = happyShift action_107
action_112 _ = happyReduce_38

action_113 (44) = happyShift action_89
action_113 (50) = happyShift action_90
action_113 (51) = happyShift action_91
action_113 (52) = happyShift action_92
action_113 (53) = happyShift action_93
action_113 (54) = happyShift action_94
action_113 (55) = happyShift action_95
action_113 (56) = happyShift action_96
action_113 (57) = happyShift action_97
action_113 (58) = happyShift action_98
action_113 (59) = happyShift action_99
action_113 (60) = happyShift action_100
action_113 (61) = happyShift action_101
action_113 (64) = happyShift action_103
action_113 (65) = happyShift action_104
action_113 (66) = happyShift action_105
action_113 (67) = happyShift action_106
action_113 (68) = happyShift action_107
action_113 _ = happyReduce_34

action_114 (44) = happyShift action_89
action_114 (50) = happyShift action_90
action_114 (51) = happyShift action_91
action_114 (52) = happyShift action_92
action_114 (53) = happyShift action_93
action_114 (54) = happyShift action_94
action_114 (55) = happyShift action_95
action_114 (56) = happyShift action_96
action_114 (57) = happyShift action_97
action_114 (58) = happyShift action_98
action_114 (59) = happyShift action_99
action_114 (60) = happyShift action_100
action_114 (64) = happyShift action_103
action_114 (65) = happyShift action_104
action_114 (66) = happyShift action_105
action_114 (67) = happyShift action_106
action_114 (68) = happyShift action_107
action_114 _ = happyReduce_35

action_115 (44) = happyShift action_89
action_115 (50) = happyShift action_90
action_115 (51) = happyShift action_91
action_115 (52) = happyShift action_92
action_115 (53) = happyShift action_93
action_115 (54) = happyShift action_94
action_115 (67) = happyShift action_106
action_115 (68) = happyShift action_107
action_115 _ = happyReduce_41

action_116 (44) = happyShift action_89
action_116 (50) = happyShift action_90
action_116 (51) = happyShift action_91
action_116 (52) = happyShift action_92
action_116 (53) = happyShift action_93
action_116 (54) = happyShift action_94
action_116 (67) = happyShift action_106
action_116 (68) = happyShift action_107
action_116 _ = happyReduce_42

action_117 (44) = happyShift action_89
action_117 (50) = happyShift action_90
action_117 (51) = happyShift action_91
action_117 (52) = happyShift action_92
action_117 (53) = happyShift action_93
action_117 (54) = happyShift action_94
action_117 (67) = happyShift action_106
action_117 (68) = happyShift action_107
action_117 _ = happyReduce_43

action_118 (44) = happyShift action_89
action_118 (50) = happyShift action_90
action_118 (51) = happyShift action_91
action_118 (52) = happyShift action_92
action_118 (53) = happyShift action_93
action_118 (54) = happyShift action_94
action_118 (67) = happyShift action_106
action_118 (68) = happyShift action_107
action_118 _ = happyReduce_44

action_119 (44) = happyShift action_89
action_119 (50) = happyShift action_90
action_119 (51) = happyShift action_91
action_119 (52) = happyShift action_92
action_119 (53) = happyShift action_93
action_119 (54) = happyShift action_94
action_119 (57) = happyShift action_97
action_119 (58) = happyShift action_98
action_119 (59) = happyShift action_99
action_119 (60) = happyShift action_100
action_119 (67) = happyShift action_106
action_119 (68) = happyShift action_107
action_119 _ = happyReduce_40

action_120 (44) = happyShift action_89
action_120 (50) = happyShift action_90
action_120 (51) = happyShift action_91
action_120 (52) = happyShift action_92
action_120 (53) = happyShift action_93
action_120 (54) = happyShift action_94
action_120 (57) = happyShift action_97
action_120 (58) = happyShift action_98
action_120 (59) = happyShift action_99
action_120 (60) = happyShift action_100
action_120 (67) = happyShift action_106
action_120 (68) = happyShift action_107
action_120 _ = happyReduce_39

action_121 (44) = happyShift action_89
action_121 (67) = happyShift action_106
action_121 (68) = happyShift action_107
action_121 _ = happyReduce_49

action_122 (44) = happyShift action_89
action_122 (67) = happyShift action_106
action_122 (68) = happyShift action_107
action_122 _ = happyReduce_48

action_123 (44) = happyShift action_89
action_123 (67) = happyShift action_106
action_123 (68) = happyShift action_107
action_123 _ = happyReduce_47

action_124 (44) = happyShift action_89
action_124 (52) = happyShift action_92
action_124 (53) = happyShift action_93
action_124 (54) = happyShift action_94
action_124 (67) = happyShift action_106
action_124 (68) = happyShift action_107
action_124 _ = happyReduce_46

action_125 (44) = happyShift action_89
action_125 (52) = happyShift action_92
action_125 (53) = happyShift action_93
action_125 (54) = happyShift action_94
action_125 (67) = happyShift action_106
action_125 (68) = happyShift action_107
action_125 _ = happyReduce_45

action_126 (37) = happyShift action_87
action_126 (44) = happyShift action_89
action_126 (45) = happyShift action_147
action_126 (50) = happyShift action_90
action_126 (51) = happyShift action_91
action_126 (52) = happyShift action_92
action_126 (53) = happyShift action_93
action_126 (54) = happyShift action_94
action_126 (55) = happyShift action_95
action_126 (56) = happyShift action_96
action_126 (57) = happyShift action_97
action_126 (58) = happyShift action_98
action_126 (59) = happyShift action_99
action_126 (60) = happyShift action_100
action_126 (61) = happyShift action_101
action_126 (62) = happyShift action_102
action_126 (64) = happyShift action_103
action_126 (65) = happyShift action_104
action_126 (66) = happyShift action_105
action_126 (67) = happyShift action_106
action_126 (68) = happyShift action_107
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (37) = happyShift action_87
action_127 (44) = happyShift action_89
action_127 (50) = happyShift action_90
action_127 (51) = happyShift action_91
action_127 (52) = happyShift action_92
action_127 (53) = happyShift action_93
action_127 (54) = happyShift action_94
action_127 (55) = happyShift action_95
action_127 (56) = happyShift action_96
action_127 (57) = happyShift action_97
action_127 (58) = happyShift action_98
action_127 (59) = happyShift action_99
action_127 (60) = happyShift action_100
action_127 (61) = happyShift action_101
action_127 (62) = happyShift action_102
action_127 (64) = happyShift action_103
action_127 (65) = happyShift action_104
action_127 (66) = happyShift action_105
action_127 (67) = happyShift action_106
action_127 (68) = happyShift action_107
action_127 _ = happyReduce_33

action_128 (37) = happyShift action_87
action_128 (44) = happyShift action_89
action_128 (50) = happyShift action_90
action_128 (51) = happyShift action_91
action_128 (52) = happyShift action_92
action_128 (53) = happyShift action_93
action_128 (54) = happyShift action_94
action_128 (55) = happyShift action_95
action_128 (56) = happyShift action_96
action_128 (57) = happyShift action_97
action_128 (58) = happyShift action_98
action_128 (59) = happyShift action_99
action_128 (60) = happyShift action_100
action_128 (61) = happyShift action_101
action_128 (62) = happyShift action_102
action_128 (64) = happyShift action_103
action_128 (65) = happyShift action_104
action_128 (66) = happyShift action_105
action_128 (67) = happyShift action_106
action_128 (68) = happyShift action_107
action_128 _ = happyReduce_76

action_129 (38) = happyShift action_145
action_129 (41) = happyShift action_146
action_129 _ = happyFail (happyExpListPerState 129)

action_130 _ = happyReduce_62

action_131 _ = happyReduce_25

action_132 (25) = happyShift action_57
action_132 (26) = happyShift action_58
action_132 (27) = happyShift action_59
action_132 (28) = happyShift action_60
action_132 (29) = happyShift action_61
action_132 (30) = happyShift action_62
action_132 (40) = happyShift action_64
action_132 (51) = happyShift action_69
action_132 (52) = happyShift action_70
action_132 (63) = happyShift action_71
action_132 (64) = happyShift action_72
action_132 (69) = happyShift action_73
action_132 (16) = happyGoto action_144
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_50

action_134 (39) = happyShift action_143
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (37) = happyShift action_87
action_135 (41) = happyShift action_142
action_135 (44) = happyShift action_89
action_135 (50) = happyShift action_90
action_135 (51) = happyShift action_91
action_135 (52) = happyShift action_92
action_135 (53) = happyShift action_93
action_135 (54) = happyShift action_94
action_135 (55) = happyShift action_95
action_135 (56) = happyShift action_96
action_135 (57) = happyShift action_97
action_135 (58) = happyShift action_98
action_135 (59) = happyShift action_99
action_135 (60) = happyShift action_100
action_135 (61) = happyShift action_101
action_135 (62) = happyShift action_102
action_135 (64) = happyShift action_103
action_135 (65) = happyShift action_104
action_135 (66) = happyShift action_105
action_135 (67) = happyShift action_106
action_135 (68) = happyShift action_107
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (37) = happyShift action_87
action_136 (41) = happyShift action_141
action_136 (44) = happyShift action_89
action_136 (50) = happyShift action_90
action_136 (51) = happyShift action_91
action_136 (52) = happyShift action_92
action_136 (53) = happyShift action_93
action_136 (54) = happyShift action_94
action_136 (55) = happyShift action_95
action_136 (56) = happyShift action_96
action_136 (57) = happyShift action_97
action_136 (58) = happyShift action_98
action_136 (59) = happyShift action_99
action_136 (60) = happyShift action_100
action_136 (61) = happyShift action_101
action_136 (62) = happyShift action_102
action_136 (64) = happyShift action_103
action_136 (65) = happyShift action_104
action_136 (66) = happyShift action_105
action_136 (67) = happyShift action_106
action_136 (68) = happyShift action_107
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (37) = happyShift action_87
action_137 (41) = happyShift action_140
action_137 (44) = happyShift action_89
action_137 (50) = happyShift action_90
action_137 (51) = happyShift action_91
action_137 (52) = happyShift action_92
action_137 (53) = happyShift action_93
action_137 (54) = happyShift action_94
action_137 (55) = happyShift action_95
action_137 (56) = happyShift action_96
action_137 (57) = happyShift action_97
action_137 (58) = happyShift action_98
action_137 (59) = happyShift action_99
action_137 (60) = happyShift action_100
action_137 (61) = happyShift action_101
action_137 (62) = happyShift action_102
action_137 (64) = happyShift action_103
action_137 (65) = happyShift action_104
action_137 (66) = happyShift action_105
action_137 (67) = happyShift action_106
action_137 (68) = happyShift action_107
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (41) = happyShift action_139
action_138 _ = happyFail (happyExpListPerState 138)

action_139 _ = happyReduce_58

action_140 _ = happyReduce_59

action_141 (25) = happyShift action_57
action_141 (26) = happyShift action_58
action_141 (27) = happyShift action_59
action_141 (28) = happyShift action_60
action_141 (29) = happyShift action_61
action_141 (30) = happyShift action_62
action_141 (31) = happyShift action_14
action_141 (32) = happyShift action_15
action_141 (33) = happyShift action_16
action_141 (34) = happyShift action_17
action_141 (35) = happyShift action_31
action_141 (36) = happyShift action_63
action_141 (40) = happyShift action_64
action_141 (42) = happyShift action_40
action_141 (46) = happyShift action_66
action_141 (47) = happyShift action_67
action_141 (48) = happyShift action_68
action_141 (51) = happyShift action_69
action_141 (52) = happyShift action_70
action_141 (63) = happyShift action_71
action_141 (64) = happyShift action_72
action_141 (69) = happyShift action_73
action_141 (9) = happyGoto action_53
action_141 (13) = happyGoto action_54
action_141 (15) = happyGoto action_151
action_141 (16) = happyGoto action_56
action_141 (18) = happyGoto action_30
action_141 (19) = happyGoto action_12
action_141 (20) = happyGoto action_13
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (25) = happyShift action_57
action_142 (26) = happyShift action_58
action_142 (27) = happyShift action_59
action_142 (28) = happyShift action_60
action_142 (29) = happyShift action_61
action_142 (30) = happyShift action_62
action_142 (31) = happyShift action_14
action_142 (32) = happyShift action_15
action_142 (33) = happyShift action_16
action_142 (34) = happyShift action_17
action_142 (35) = happyShift action_31
action_142 (36) = happyShift action_63
action_142 (40) = happyShift action_64
action_142 (42) = happyShift action_40
action_142 (46) = happyShift action_66
action_142 (47) = happyShift action_67
action_142 (48) = happyShift action_68
action_142 (51) = happyShift action_69
action_142 (52) = happyShift action_70
action_142 (63) = happyShift action_71
action_142 (64) = happyShift action_72
action_142 (69) = happyShift action_73
action_142 (9) = happyGoto action_53
action_142 (13) = happyGoto action_54
action_142 (15) = happyGoto action_150
action_142 (16) = happyGoto action_56
action_142 (18) = happyGoto action_30
action_142 (19) = happyGoto action_12
action_142 (20) = happyGoto action_13
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (25) = happyShift action_57
action_143 (26) = happyShift action_58
action_143 (27) = happyShift action_59
action_143 (28) = happyShift action_60
action_143 (29) = happyShift action_61
action_143 (30) = happyShift action_62
action_143 (40) = happyShift action_64
action_143 (51) = happyShift action_69
action_143 (52) = happyShift action_70
action_143 (63) = happyShift action_71
action_143 (64) = happyShift action_72
action_143 (69) = happyShift action_73
action_143 (16) = happyGoto action_84
action_143 (17) = happyGoto action_149
action_143 _ = happyReduce_63

action_144 (44) = happyShift action_89
action_144 (67) = happyShift action_106
action_144 (68) = happyShift action_107
action_144 _ = happyReduce_60

action_145 (25) = happyShift action_57
action_145 (26) = happyShift action_58
action_145 (27) = happyShift action_59
action_145 (28) = happyShift action_60
action_145 (29) = happyShift action_61
action_145 (30) = happyShift action_62
action_145 (40) = happyShift action_64
action_145 (51) = happyShift action_69
action_145 (52) = happyShift action_70
action_145 (63) = happyShift action_71
action_145 (64) = happyShift action_72
action_145 (69) = happyShift action_73
action_145 (16) = happyGoto action_148
action_145 _ = happyFail (happyExpListPerState 145)

action_146 _ = happyReduce_61

action_147 _ = happyReduce_56

action_148 (37) = happyShift action_87
action_148 (44) = happyShift action_89
action_148 (50) = happyShift action_90
action_148 (51) = happyShift action_91
action_148 (52) = happyShift action_92
action_148 (53) = happyShift action_93
action_148 (54) = happyShift action_94
action_148 (55) = happyShift action_95
action_148 (56) = happyShift action_96
action_148 (57) = happyShift action_97
action_148 (58) = happyShift action_98
action_148 (59) = happyShift action_99
action_148 (60) = happyShift action_100
action_148 (61) = happyShift action_101
action_148 (62) = happyShift action_102
action_148 (64) = happyShift action_103
action_148 (65) = happyShift action_104
action_148 (66) = happyShift action_105
action_148 (67) = happyShift action_106
action_148 (68) = happyShift action_107
action_148 _ = happyReduce_77

action_149 (39) = happyShift action_153
action_149 _ = happyFail (happyExpListPerState 149)

action_150 _ = happyReduce_21

action_151 (49) = happyShift action_152
action_151 _ = happyReduce_24

action_152 (25) = happyShift action_57
action_152 (26) = happyShift action_58
action_152 (27) = happyShift action_59
action_152 (28) = happyShift action_60
action_152 (29) = happyShift action_61
action_152 (30) = happyShift action_62
action_152 (31) = happyShift action_14
action_152 (32) = happyShift action_15
action_152 (33) = happyShift action_16
action_152 (34) = happyShift action_17
action_152 (35) = happyShift action_31
action_152 (36) = happyShift action_63
action_152 (40) = happyShift action_64
action_152 (42) = happyShift action_40
action_152 (46) = happyShift action_66
action_152 (47) = happyShift action_67
action_152 (48) = happyShift action_68
action_152 (51) = happyShift action_69
action_152 (52) = happyShift action_70
action_152 (63) = happyShift action_71
action_152 (64) = happyShift action_72
action_152 (69) = happyShift action_73
action_152 (9) = happyGoto action_53
action_152 (13) = happyGoto action_54
action_152 (15) = happyGoto action_155
action_152 (16) = happyGoto action_56
action_152 (18) = happyGoto action_30
action_152 (19) = happyGoto action_12
action_152 (20) = happyGoto action_13
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (25) = happyShift action_57
action_153 (26) = happyShift action_58
action_153 (27) = happyShift action_59
action_153 (28) = happyShift action_60
action_153 (29) = happyShift action_61
action_153 (30) = happyShift action_62
action_153 (40) = happyShift action_64
action_153 (51) = happyShift action_69
action_153 (52) = happyShift action_70
action_153 (63) = happyShift action_71
action_153 (64) = happyShift action_72
action_153 (69) = happyShift action_73
action_153 (16) = happyGoto action_84
action_153 (17) = happyGoto action_154
action_153 _ = happyReduce_63

action_154 (41) = happyShift action_156
action_154 _ = happyFail (happyExpListPerState 154)

action_155 _ = happyReduce_23

action_156 (25) = happyShift action_57
action_156 (26) = happyShift action_58
action_156 (27) = happyShift action_59
action_156 (28) = happyShift action_60
action_156 (29) = happyShift action_61
action_156 (30) = happyShift action_62
action_156 (31) = happyShift action_14
action_156 (32) = happyShift action_15
action_156 (33) = happyShift action_16
action_156 (34) = happyShift action_17
action_156 (35) = happyShift action_31
action_156 (36) = happyShift action_63
action_156 (40) = happyShift action_64
action_156 (42) = happyShift action_40
action_156 (46) = happyShift action_66
action_156 (47) = happyShift action_67
action_156 (48) = happyShift action_68
action_156 (51) = happyShift action_69
action_156 (52) = happyShift action_70
action_156 (63) = happyShift action_71
action_156 (64) = happyShift action_72
action_156 (69) = happyShift action_73
action_156 (9) = happyGoto action_53
action_156 (13) = happyGoto action_54
action_156 (15) = happyGoto action_157
action_156 (16) = happyGoto action_56
action_156 (18) = happyGoto action_30
action_156 (19) = happyGoto action_12
action_156 (20) = happyGoto action_13
action_156 _ = happyFail (happyExpListPerState 156)

action_157 _ = happyReduce_22

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (P.Program (reverse happy_var_2)
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 ((happy_var_2 : happy_var_1)
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (P.FuncDecl happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (P.StructDecl happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (P.VarDecl happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happyReduce 6 7 happyReduction_7
happyReduction_7 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (P.Struct happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 5 8 happyReduction_8
happyReduction_8 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_2)) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (P.Func happy_var_1 happy_var_2 [] happy_var_5
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 6 8 happyReduction_9
happyReduction_9 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_2)) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (P.Func happy_var_1 happy_var_2 (reverse happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 4 9 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	(HappyTerminal (L.Ident happy_var_2)) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (P.Var happy_var_1 happy_var_2 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 : happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3 : happy_var_1
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  12 happyReduction_15
happyReduction_15 (HappyTerminal (L.Ident happy_var_2))
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn12
		 (P.Formal happy_var_1 happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (P.Block (reverse happy_var_2)
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  14 happyReduction_17
happyReduction_17  =  HappyAbsSyn14
		 ([]
	)

happyReduce_18 = happySpecReduce_2  14 happyReduction_18
happyReduction_18 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_2 : happy_var_1
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  15 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (P.Expr happy_var_1
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  15 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn15
		 (P.BlockStatement happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 5 15 happyReduction_21
happyReduction_21 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (P.While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 9 15 happyReduction_22
happyReduction_22 ((HappyAbsSyn15  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (P.For happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 7 15 happyReduction_23
happyReduction_23 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (P.If happy_var_3 happy_var_5 (Just happy_var_7)
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 5 15 happyReduction_24
happyReduction_24 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (P.If happy_var_3 happy_var_5 Nothing
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  15 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (P.Return happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  15 happyReduction_26
happyReduction_26 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn15
		 (P.VarDeclStatement happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyTerminal (L.LitInt happy_var_1))
	 =  HappyAbsSyn16
		 (P.LitInt happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyTerminal (L.LitDouble happy_var_1))
	 =  HappyAbsSyn16
		 (P.LitDouble happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  16 happyReduction_29
happyReduction_29 (HappyTerminal (L.LitChar happy_var_1))
	 =  HappyAbsSyn16
		 (P.LitChar happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  16 happyReduction_30
happyReduction_30 (HappyTerminal (L.LitString happy_var_1))
	 =  HappyAbsSyn16
		 (P.LitString happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  16 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn16
		 (P.Null
	)

happyReduce_32 = happySpecReduce_1  16 happyReduction_32
happyReduction_32 (HappyTerminal (L.Ident happy_var_1))
	 =  HappyAbsSyn16
		 (P.Ident happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  16 happyReduction_33
happyReduction_33 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Assign happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Or happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  16 happyReduction_35
happyReduction_35 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.And happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  16 happyReduction_36
happyReduction_36 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.BitwiseOr happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  16 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.BitwiseXor happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  16 happyReduction_38
happyReduction_38 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.BitwiseAnd happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  16 happyReduction_39
happyReduction_39 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Equal happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  16 happyReduction_40
happyReduction_40 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Neq happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  16 happyReduction_41
happyReduction_41 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Geq happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  16 happyReduction_42
happyReduction_42 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Greater happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  16 happyReduction_43
happyReduction_43 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Leq happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  16 happyReduction_44
happyReduction_44 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Less happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  16 happyReduction_45
happyReduction_45 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Add happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  16 happyReduction_46
happyReduction_46 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Sub happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  16 happyReduction_47
happyReduction_47 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Mul happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  16 happyReduction_48
happyReduction_48 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Div happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  16 happyReduction_49
happyReduction_49 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Mod happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  16 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.Nested happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  16 happyReduction_51
happyReduction_51 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.Negative happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  16 happyReduction_52
happyReduction_52 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.Negate happy_var_2
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  16 happyReduction_53
happyReduction_53 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.AddressOf happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  16 happyReduction_54
happyReduction_54 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.Deref happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  16 happyReduction_55
happyReduction_55 (HappyTerminal (L.Ident happy_var_3))
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.FieldAccess happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happyReduce 4 16 happyReduction_56
happyReduction_56 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.ArrayAccess happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_57 = happySpecReduce_3  16 happyReduction_57
happyReduction_57 (HappyTerminal (L.Ident happy_var_3))
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Indirect happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happyReduce 4 16 happyReduction_58
happyReduction_58 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.Sizeof (Left happy_var_3)
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 4 16 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.Sizeof (Right happy_var_3)
	) `HappyStk` happyRest

happyReduce_60 = happyReduce 4 16 happyReduction_60
happyReduction_60 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.Typecast happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_61 = happyReduce 4 16 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.Call happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_3  16 happyReduction_62
happyReduction_62 _
	_
	(HappyTerminal (L.Ident happy_var_1))
	 =  HappyAbsSyn16
		 (P.Call happy_var_1 []
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_0  17 happyReduction_63
happyReduction_63  =  HappyAbsSyn17
		 (Nothing
	)

happyReduce_64 = happySpecReduce_1  17 happyReduction_64
happyReduction_64 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (Just happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  18 happyReduction_65
happyReduction_65 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  18 happyReduction_66
happyReduction_66 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 happy_var_2
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  19 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn19
		 (P.PrimitiveType L.Int
	)

happyReduce_68 = happySpecReduce_1  19 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn19
		 (P.PrimitiveType L.Double
	)

happyReduce_69 = happySpecReduce_1  19 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn19
		 (P.PrimitiveType L.Char
	)

happyReduce_70 = happySpecReduce_1  19 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn19
		 (P.PrimitiveType L.Void
	)

happyReduce_71 = happySpecReduce_2  20 happyReduction_71
happyReduction_71 (HappyTerminal (L.Ident happy_var_2))
	_
	 =  HappyAbsSyn20
		 (P.StructType happy_var_2
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_0  21 happyReduction_72
happyReduction_72  =  HappyAbsSyn21
		 (0
	)

happyReduce_73 = happySpecReduce_2  21 happyReduction_73
happyReduction_73 _
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 + 1
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_0  22 happyReduction_74
happyReduction_74  =  HappyAbsSyn22
		 ([]
	)

happyReduce_75 = happyReduce 4 22 happyReduction_75
happyReduction_75 (_ `HappyStk`
	(HappyTerminal (L.LitInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (happy_var_3 : happy_var_1
	) `HappyStk` happyRest

happyReduce_76 = happySpecReduce_1  23 happyReduction_76
happyReduction_76 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  23 happyReduction_77
happyReduction_77 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_3 : happy_var_1
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_0  24 happyReduction_78
happyReduction_78  =  HappyAbsSyn24
		 (
	)

happyReduce_79 = happySpecReduce_3  24 happyReduction_79
happyReduction_79 _
	_
	_
	 =  HappyAbsSyn24
		 (
	)

happyNewToken action sts stk [] =
	action 72 72 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.LitInt happy_dollar_dollar -> cont 25;
	L.LitDouble happy_dollar_dollar -> cont 26;
	L.LitString happy_dollar_dollar -> cont 27;
	L.LitChar happy_dollar_dollar -> cont 28;
	L.LitNull -> cont 29;
	L.Ident happy_dollar_dollar -> cont 30;
	L.Type L.Int -> cont 31;
	L.Type L.Double -> cont 32;
	L.Type L.Char -> cont 33;
	L.Type L.Void -> cont 34;
	L.Struct -> cont 35;
	L.Return -> cont 36;
	L.Assign -> cont 37;
	L.Comma -> cont 38;
	L.Semi -> cont 39;
	L.LParen -> cont 40;
	L.RParen -> cont 41;
	L.LBrace -> cont 42;
	L.RBrace -> cont 43;
	L.LBrack -> cont 44;
	L.RBrack -> cont 45;
	L.For -> cont 46;
	L.While -> cont 47;
	L.If -> cont 48;
	L.Else -> cont 49;
	L.Plus -> cont 50;
	L.Minus -> cont 51;
	L.Asterisk -> cont 52;
	L.Div -> cont 53;
	L.Mod -> cont 54;
	L.Equal -> cont 55;
	L.Neq -> cont 56;
	L.Less -> cont 57;
	L.Leq -> cont 58;
	L.Greater -> cont 59;
	L.Geq -> cont 60;
	L.And -> cont 61;
	L.Or -> cont 62;
	L.Not -> cont 63;
	L.Ampers -> cont 64;
	L.Bar -> cont 65;
	L.Caret -> cont 66;
	L.Dot -> cont 67;
	L.Arrow -> cont 68;
	L.Sizeof -> cont 69;
	L.Include -> cont 70;
	L.Eof -> cont 71;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 72 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(L.Lexeme)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError _ = error "Unable to parse tokens"
fst (a, _, _) = a
snd (_, b, _) = b
thd (_, _, c) = c
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
