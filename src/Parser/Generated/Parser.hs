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

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25
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
	| HappyAbsSyn25 t25

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,546) ([0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,248,0,2048,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,2,0,0,0,1024,0,0,0,128,0,0,0,32768,16,0,0,0,1055,0,0,0,0,0,0,0,0,124,0,0,0,0,0,0,0,0,496,1,0,0,4096,0,0,0,0,32,0,0,0,0,576,0,0,0,0,0,0,0,0,1,0,0,0,0,32,0,0,0,0,0,0,0,64,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15872,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,57344,53759,412,536,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8512,65528,62,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,32256,256,32792,33,0,8188,12290,17152,0,0,0,0,0,0,0,8,0,0,0,4096,0,0,0,0,32,0,0,32768,16415,1536,2144,0,16128,128,49164,16,0,126,6145,8576,0,64512,512,48,67,0,0,4,0,0,0,0,1,0,0,0,0,0,0,0,496,0,0,0,0,1024,0,6,0,0,8,3072,0,0,4096,0,24,0,0,32,12288,0,63488,1025,96,134,0,1008,49160,3072,1,57344,4103,384,536,0,0,33348,61439,3,0,0,0,0,0,0,2064,49150,15,0,32768,0,0,0,64512,1536,48,67,0,504,24580,34304,0,0,0,0,0,0,2016,32784,6145,2,49152,8207,768,1072,0,8064,64,24582,8,0,32831,3072,4288,0,32256,256,32792,33,0,252,12290,17152,0,63488,1025,96,134,0,1008,49160,3072,1,57344,4103,384,536,0,4032,32,12291,4,32768,16415,1536,2144,0,16128,128,49164,16,0,126,6145,8576,0,64512,512,48,67,0,504,24580,34304,0,61440,2051,192,268,0,2016,32784,6145,2,0,8,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63520,12863,0,0,16384,32752,116,0,0,57472,49407,0,0,0,65473,499,0,0,33280,58367,3,0,0,7940,1536,0,0,2048,62,12,0,0,31760,6144,0,0,8192,248,48,0,0,61504,24697,0,0,32768,62432,192,0,0,256,32768,1,0,0,2,768,0,0,1024,0,6,0,0,14344,3072,0,0,4096,112,24,0,16384,63584,16127,0,0,16512,65520,125,0,0,57473,64511,0,0,9216,0,0,0,0,0,0,0,0,0,0,0,0,0,2304,0,0,0,0,0,0,0,0,256,0,0,0,32768,61512,32255,0,0,37120,65504,251,0,0,32,0,0,0,0,0,0,0,65408,28999,24582,8,0,36863,3298,4288,0,32256,256,32792,33,0,252,12290,17152,0,63488,1025,96,134,0,0,0,0,0,0,0,0,0,0,0,33284,61439,3,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,4,0,0,32760,26388,34304,0,61440,2051,192,268,0,0,32,0,0,0,0,0,0,0,65408,28999,24582,8,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","program","construct_list","construct","structdecl","funcdecl","vardecl","vardecl_list","formals","formal","block","block_statements","statement","expr","opt_expr","type","primitive_type","struct_type","stars","array_sizes","actuals","brackets","includes","int","double","string","char","null","ident","tint","tdouble","tchar","tvoid","struct","return","'='","','","';'","'('","')'","'{'","'}'","'['","']'","for","while","if","else","'+'","'-'","'*'","'/'","'%'","'=='","'!='","'<'","'<='","'>'","'>='","'&&'","'||'","'!'","'&'","'|'","'^'","'.'","'->'","sizeof","include","eof","%eof"]
        bit_start = st Prelude.* 73
        bit_end = (st Prelude.+ 1) Prelude.* 73
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..72]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (4) = happyGoto action_3
action_0 (25) = happyGoto action_2
action_0 _ = happyReduce_79

action_1 (25) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (71) = happyShift action_5
action_2 (5) = happyGoto action_4
action_2 _ = happyReduce_2

action_3 (73) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (32) = happyShift action_14
action_4 (33) = happyShift action_15
action_4 (34) = happyShift action_16
action_4 (35) = happyShift action_17
action_4 (36) = happyShift action_18
action_4 (72) = happyShift action_19
action_4 (6) = happyGoto action_7
action_4 (7) = happyGoto action_8
action_4 (8) = happyGoto action_9
action_4 (9) = happyGoto action_10
action_4 (18) = happyGoto action_11
action_4 (19) = happyGoto action_12
action_4 (20) = happyGoto action_13
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (28) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_80

action_7 _ = happyReduce_3

action_8 _ = happyReduce_5

action_9 _ = happyReduce_4

action_10 _ = happyReduce_6

action_11 (31) = happyShift action_23
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (21) = happyGoto action_22
action_12 _ = happyReduce_71

action_13 (21) = happyGoto action_21
action_13 _ = happyReduce_71

action_14 _ = happyReduce_66

action_15 _ = happyReduce_67

action_16 _ = happyReduce_68

action_17 _ = happyReduce_69

action_18 (31) = happyShift action_20
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_1

action_20 (43) = happyShift action_27
action_20 _ = happyReduce_70

action_21 (53) = happyShift action_26
action_21 _ = happyReduce_65

action_22 (53) = happyShift action_26
action_22 _ = happyReduce_64

action_23 (41) = happyShift action_25
action_23 (22) = happyGoto action_24
action_23 _ = happyReduce_73

action_24 (40) = happyShift action_36
action_24 (45) = happyShift action_37
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (32) = happyShift action_14
action_25 (33) = happyShift action_15
action_25 (34) = happyShift action_16
action_25 (35) = happyShift action_17
action_25 (36) = happyShift action_31
action_25 (42) = happyShift action_35
action_25 (11) = happyGoto action_32
action_25 (12) = happyGoto action_33
action_25 (18) = happyGoto action_34
action_25 (19) = happyGoto action_12
action_25 (20) = happyGoto action_13
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_72

action_27 (32) = happyShift action_14
action_27 (33) = happyShift action_15
action_27 (34) = happyShift action_16
action_27 (35) = happyShift action_17
action_27 (36) = happyShift action_31
action_27 (9) = happyGoto action_28
action_27 (10) = happyGoto action_29
action_27 (18) = happyGoto action_30
action_27 (19) = happyGoto action_12
action_27 (20) = happyGoto action_13
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_12

action_29 (32) = happyShift action_14
action_29 (33) = happyShift action_15
action_29 (34) = happyShift action_16
action_29 (35) = happyShift action_17
action_29 (36) = happyShift action_31
action_29 (44) = happyShift action_47
action_29 (9) = happyGoto action_46
action_29 (18) = happyGoto action_30
action_29 (19) = happyGoto action_12
action_29 (20) = happyGoto action_13
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (31) = happyShift action_45
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (31) = happyShift action_44
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (39) = happyShift action_42
action_32 (42) = happyShift action_43
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_13

action_34 (31) = happyShift action_41
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (43) = happyShift action_40
action_35 (13) = happyGoto action_39
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_10

action_37 (26) = happyShift action_38
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (46) = happyShift action_53
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_8

action_40 (14) = happyGoto action_52
action_40 _ = happyReduce_17

action_41 (24) = happyGoto action_51
action_41 _ = happyReduce_77

action_42 (32) = happyShift action_14
action_42 (33) = happyShift action_15
action_42 (34) = happyShift action_16
action_42 (35) = happyShift action_17
action_42 (36) = happyShift action_31
action_42 (12) = happyGoto action_50
action_42 (18) = happyGoto action_34
action_42 (19) = happyGoto action_12
action_42 (20) = happyGoto action_13
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (43) = happyShift action_40
action_43 (13) = happyGoto action_49
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_70

action_45 (22) = happyGoto action_24
action_45 _ = happyReduce_73

action_46 _ = happyReduce_11

action_47 (40) = happyShift action_48
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_7

action_49 _ = happyReduce_9

action_50 _ = happyReduce_14

action_51 (45) = happyShift action_75
action_51 _ = happyReduce_15

action_52 (26) = happyShift action_58
action_52 (27) = happyShift action_59
action_52 (28) = happyShift action_60
action_52 (29) = happyShift action_61
action_52 (30) = happyShift action_62
action_52 (31) = happyShift action_63
action_52 (32) = happyShift action_14
action_52 (33) = happyShift action_15
action_52 (34) = happyShift action_16
action_52 (35) = happyShift action_17
action_52 (36) = happyShift action_31
action_52 (37) = happyShift action_64
action_52 (41) = happyShift action_65
action_52 (43) = happyShift action_40
action_52 (44) = happyShift action_66
action_52 (47) = happyShift action_67
action_52 (48) = happyShift action_68
action_52 (49) = happyShift action_69
action_52 (52) = happyShift action_70
action_52 (53) = happyShift action_71
action_52 (64) = happyShift action_72
action_52 (65) = happyShift action_73
action_52 (70) = happyShift action_74
action_52 (9) = happyGoto action_54
action_52 (13) = happyGoto action_55
action_52 (15) = happyGoto action_56
action_52 (16) = happyGoto action_57
action_52 (18) = happyGoto action_30
action_52 (19) = happyGoto action_12
action_52 (20) = happyGoto action_13
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_74

action_54 _ = happyReduce_26

action_55 _ = happyReduce_20

action_56 _ = happyReduce_18

action_57 (38) = happyShift action_90
action_57 (40) = happyShift action_91
action_57 (45) = happyShift action_92
action_57 (51) = happyShift action_93
action_57 (52) = happyShift action_94
action_57 (53) = happyShift action_95
action_57 (54) = happyShift action_96
action_57 (55) = happyShift action_97
action_57 (56) = happyShift action_98
action_57 (57) = happyShift action_99
action_57 (58) = happyShift action_100
action_57 (59) = happyShift action_101
action_57 (60) = happyShift action_102
action_57 (61) = happyShift action_103
action_57 (62) = happyShift action_104
action_57 (63) = happyShift action_105
action_57 (65) = happyShift action_106
action_57 (66) = happyShift action_107
action_57 (67) = happyShift action_108
action_57 (68) = happyShift action_109
action_57 (69) = happyShift action_110
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_27

action_59 _ = happyReduce_28

action_60 _ = happyReduce_30

action_61 _ = happyReduce_29

action_62 _ = happyReduce_31

action_63 (41) = happyShift action_89
action_63 _ = happyReduce_32

action_64 (26) = happyShift action_58
action_64 (27) = happyShift action_59
action_64 (28) = happyShift action_60
action_64 (29) = happyShift action_61
action_64 (30) = happyShift action_62
action_64 (31) = happyShift action_63
action_64 (41) = happyShift action_65
action_64 (52) = happyShift action_70
action_64 (53) = happyShift action_71
action_64 (64) = happyShift action_72
action_64 (65) = happyShift action_73
action_64 (70) = happyShift action_74
action_64 (16) = happyGoto action_87
action_64 (17) = happyGoto action_88
action_64 _ = happyReduce_62

action_65 (26) = happyShift action_58
action_65 (27) = happyShift action_59
action_65 (28) = happyShift action_60
action_65 (29) = happyShift action_61
action_65 (30) = happyShift action_62
action_65 (31) = happyShift action_63
action_65 (32) = happyShift action_14
action_65 (33) = happyShift action_15
action_65 (34) = happyShift action_16
action_65 (35) = happyShift action_17
action_65 (36) = happyShift action_31
action_65 (41) = happyShift action_65
action_65 (52) = happyShift action_70
action_65 (53) = happyShift action_71
action_65 (64) = happyShift action_72
action_65 (65) = happyShift action_73
action_65 (70) = happyShift action_74
action_65 (16) = happyGoto action_85
action_65 (18) = happyGoto action_86
action_65 (19) = happyGoto action_12
action_65 (20) = happyGoto action_13
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_16

action_67 (41) = happyShift action_84
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (41) = happyShift action_83
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (41) = happyShift action_82
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (26) = happyShift action_58
action_70 (27) = happyShift action_59
action_70 (28) = happyShift action_60
action_70 (29) = happyShift action_61
action_70 (30) = happyShift action_62
action_70 (31) = happyShift action_63
action_70 (41) = happyShift action_65
action_70 (52) = happyShift action_70
action_70 (53) = happyShift action_71
action_70 (64) = happyShift action_72
action_70 (65) = happyShift action_73
action_70 (70) = happyShift action_74
action_70 (16) = happyGoto action_81
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (26) = happyShift action_58
action_71 (27) = happyShift action_59
action_71 (28) = happyShift action_60
action_71 (29) = happyShift action_61
action_71 (30) = happyShift action_62
action_71 (31) = happyShift action_63
action_71 (41) = happyShift action_65
action_71 (52) = happyShift action_70
action_71 (53) = happyShift action_71
action_71 (64) = happyShift action_72
action_71 (65) = happyShift action_73
action_71 (70) = happyShift action_74
action_71 (16) = happyGoto action_80
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (26) = happyShift action_58
action_72 (27) = happyShift action_59
action_72 (28) = happyShift action_60
action_72 (29) = happyShift action_61
action_72 (30) = happyShift action_62
action_72 (31) = happyShift action_63
action_72 (41) = happyShift action_65
action_72 (52) = happyShift action_70
action_72 (53) = happyShift action_71
action_72 (64) = happyShift action_72
action_72 (65) = happyShift action_73
action_72 (70) = happyShift action_74
action_72 (16) = happyGoto action_79
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (26) = happyShift action_58
action_73 (27) = happyShift action_59
action_73 (28) = happyShift action_60
action_73 (29) = happyShift action_61
action_73 (30) = happyShift action_62
action_73 (31) = happyShift action_63
action_73 (41) = happyShift action_65
action_73 (52) = happyShift action_70
action_73 (53) = happyShift action_71
action_73 (64) = happyShift action_72
action_73 (65) = happyShift action_73
action_73 (70) = happyShift action_74
action_73 (16) = happyGoto action_78
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (41) = happyShift action_77
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (46) = happyShift action_76
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_78

action_77 (32) = happyShift action_14
action_77 (33) = happyShift action_15
action_77 (34) = happyShift action_16
action_77 (35) = happyShift action_17
action_77 (36) = happyShift action_31
action_77 (18) = happyGoto action_140
action_77 (19) = happyGoto action_12
action_77 (20) = happyGoto action_13
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (45) = happyShift action_92
action_78 (68) = happyShift action_109
action_78 (69) = happyShift action_110
action_78 _ = happyReduce_52

action_79 (45) = happyShift action_92
action_79 (68) = happyShift action_109
action_79 (69) = happyShift action_110
action_79 _ = happyReduce_50

action_80 (45) = happyShift action_92
action_80 (68) = happyShift action_109
action_80 (69) = happyShift action_110
action_80 _ = happyReduce_53

action_81 (45) = happyShift action_92
action_81 (68) = happyShift action_109
action_81 (69) = happyShift action_110
action_81 _ = happyReduce_51

action_82 (26) = happyShift action_58
action_82 (27) = happyShift action_59
action_82 (28) = happyShift action_60
action_82 (29) = happyShift action_61
action_82 (30) = happyShift action_62
action_82 (31) = happyShift action_63
action_82 (41) = happyShift action_65
action_82 (52) = happyShift action_70
action_82 (53) = happyShift action_71
action_82 (64) = happyShift action_72
action_82 (65) = happyShift action_73
action_82 (70) = happyShift action_74
action_82 (16) = happyGoto action_139
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (26) = happyShift action_58
action_83 (27) = happyShift action_59
action_83 (28) = happyShift action_60
action_83 (29) = happyShift action_61
action_83 (30) = happyShift action_62
action_83 (31) = happyShift action_63
action_83 (41) = happyShift action_65
action_83 (52) = happyShift action_70
action_83 (53) = happyShift action_71
action_83 (64) = happyShift action_72
action_83 (65) = happyShift action_73
action_83 (70) = happyShift action_74
action_83 (16) = happyGoto action_138
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (26) = happyShift action_58
action_84 (27) = happyShift action_59
action_84 (28) = happyShift action_60
action_84 (29) = happyShift action_61
action_84 (30) = happyShift action_62
action_84 (31) = happyShift action_63
action_84 (41) = happyShift action_65
action_84 (52) = happyShift action_70
action_84 (53) = happyShift action_71
action_84 (64) = happyShift action_72
action_84 (65) = happyShift action_73
action_84 (70) = happyShift action_74
action_84 (16) = happyGoto action_87
action_84 (17) = happyGoto action_137
action_84 _ = happyReduce_62

action_85 (38) = happyShift action_90
action_85 (42) = happyShift action_136
action_85 (45) = happyShift action_92
action_85 (51) = happyShift action_93
action_85 (52) = happyShift action_94
action_85 (53) = happyShift action_95
action_85 (54) = happyShift action_96
action_85 (55) = happyShift action_97
action_85 (56) = happyShift action_98
action_85 (57) = happyShift action_99
action_85 (58) = happyShift action_100
action_85 (59) = happyShift action_101
action_85 (60) = happyShift action_102
action_85 (61) = happyShift action_103
action_85 (62) = happyShift action_104
action_85 (63) = happyShift action_105
action_85 (65) = happyShift action_106
action_85 (66) = happyShift action_107
action_85 (67) = happyShift action_108
action_85 (68) = happyShift action_109
action_85 (69) = happyShift action_110
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (22) = happyGoto action_135
action_86 _ = happyReduce_73

action_87 (38) = happyShift action_90
action_87 (45) = happyShift action_92
action_87 (51) = happyShift action_93
action_87 (52) = happyShift action_94
action_87 (53) = happyShift action_95
action_87 (54) = happyShift action_96
action_87 (55) = happyShift action_97
action_87 (56) = happyShift action_98
action_87 (57) = happyShift action_99
action_87 (58) = happyShift action_100
action_87 (59) = happyShift action_101
action_87 (60) = happyShift action_102
action_87 (61) = happyShift action_103
action_87 (62) = happyShift action_104
action_87 (63) = happyShift action_105
action_87 (65) = happyShift action_106
action_87 (66) = happyShift action_107
action_87 (67) = happyShift action_108
action_87 (68) = happyShift action_109
action_87 (69) = happyShift action_110
action_87 _ = happyReduce_63

action_88 (40) = happyShift action_134
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (26) = happyShift action_58
action_89 (27) = happyShift action_59
action_89 (28) = happyShift action_60
action_89 (29) = happyShift action_61
action_89 (30) = happyShift action_62
action_89 (31) = happyShift action_63
action_89 (41) = happyShift action_65
action_89 (42) = happyShift action_133
action_89 (52) = happyShift action_70
action_89 (53) = happyShift action_71
action_89 (64) = happyShift action_72
action_89 (65) = happyShift action_73
action_89 (70) = happyShift action_74
action_89 (16) = happyGoto action_131
action_89 (23) = happyGoto action_132
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (26) = happyShift action_58
action_90 (27) = happyShift action_59
action_90 (28) = happyShift action_60
action_90 (29) = happyShift action_61
action_90 (30) = happyShift action_62
action_90 (31) = happyShift action_63
action_90 (41) = happyShift action_65
action_90 (52) = happyShift action_70
action_90 (53) = happyShift action_71
action_90 (64) = happyShift action_72
action_90 (65) = happyShift action_73
action_90 (70) = happyShift action_74
action_90 (16) = happyGoto action_130
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_19

action_92 (26) = happyShift action_58
action_92 (27) = happyShift action_59
action_92 (28) = happyShift action_60
action_92 (29) = happyShift action_61
action_92 (30) = happyShift action_62
action_92 (31) = happyShift action_63
action_92 (41) = happyShift action_65
action_92 (52) = happyShift action_70
action_92 (53) = happyShift action_71
action_92 (64) = happyShift action_72
action_92 (65) = happyShift action_73
action_92 (70) = happyShift action_74
action_92 (16) = happyGoto action_129
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (26) = happyShift action_58
action_93 (27) = happyShift action_59
action_93 (28) = happyShift action_60
action_93 (29) = happyShift action_61
action_93 (30) = happyShift action_62
action_93 (31) = happyShift action_63
action_93 (41) = happyShift action_65
action_93 (52) = happyShift action_70
action_93 (53) = happyShift action_71
action_93 (64) = happyShift action_72
action_93 (65) = happyShift action_73
action_93 (70) = happyShift action_74
action_93 (16) = happyGoto action_128
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (26) = happyShift action_58
action_94 (27) = happyShift action_59
action_94 (28) = happyShift action_60
action_94 (29) = happyShift action_61
action_94 (30) = happyShift action_62
action_94 (31) = happyShift action_63
action_94 (41) = happyShift action_65
action_94 (52) = happyShift action_70
action_94 (53) = happyShift action_71
action_94 (64) = happyShift action_72
action_94 (65) = happyShift action_73
action_94 (70) = happyShift action_74
action_94 (16) = happyGoto action_127
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (26) = happyShift action_58
action_95 (27) = happyShift action_59
action_95 (28) = happyShift action_60
action_95 (29) = happyShift action_61
action_95 (30) = happyShift action_62
action_95 (31) = happyShift action_63
action_95 (41) = happyShift action_65
action_95 (52) = happyShift action_70
action_95 (53) = happyShift action_71
action_95 (64) = happyShift action_72
action_95 (65) = happyShift action_73
action_95 (70) = happyShift action_74
action_95 (16) = happyGoto action_126
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (26) = happyShift action_58
action_96 (27) = happyShift action_59
action_96 (28) = happyShift action_60
action_96 (29) = happyShift action_61
action_96 (30) = happyShift action_62
action_96 (31) = happyShift action_63
action_96 (41) = happyShift action_65
action_96 (52) = happyShift action_70
action_96 (53) = happyShift action_71
action_96 (64) = happyShift action_72
action_96 (65) = happyShift action_73
action_96 (70) = happyShift action_74
action_96 (16) = happyGoto action_125
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (26) = happyShift action_58
action_97 (27) = happyShift action_59
action_97 (28) = happyShift action_60
action_97 (29) = happyShift action_61
action_97 (30) = happyShift action_62
action_97 (31) = happyShift action_63
action_97 (41) = happyShift action_65
action_97 (52) = happyShift action_70
action_97 (53) = happyShift action_71
action_97 (64) = happyShift action_72
action_97 (65) = happyShift action_73
action_97 (70) = happyShift action_74
action_97 (16) = happyGoto action_124
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (26) = happyShift action_58
action_98 (27) = happyShift action_59
action_98 (28) = happyShift action_60
action_98 (29) = happyShift action_61
action_98 (30) = happyShift action_62
action_98 (31) = happyShift action_63
action_98 (41) = happyShift action_65
action_98 (52) = happyShift action_70
action_98 (53) = happyShift action_71
action_98 (64) = happyShift action_72
action_98 (65) = happyShift action_73
action_98 (70) = happyShift action_74
action_98 (16) = happyGoto action_123
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (26) = happyShift action_58
action_99 (27) = happyShift action_59
action_99 (28) = happyShift action_60
action_99 (29) = happyShift action_61
action_99 (30) = happyShift action_62
action_99 (31) = happyShift action_63
action_99 (41) = happyShift action_65
action_99 (52) = happyShift action_70
action_99 (53) = happyShift action_71
action_99 (64) = happyShift action_72
action_99 (65) = happyShift action_73
action_99 (70) = happyShift action_74
action_99 (16) = happyGoto action_122
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (26) = happyShift action_58
action_100 (27) = happyShift action_59
action_100 (28) = happyShift action_60
action_100 (29) = happyShift action_61
action_100 (30) = happyShift action_62
action_100 (31) = happyShift action_63
action_100 (41) = happyShift action_65
action_100 (52) = happyShift action_70
action_100 (53) = happyShift action_71
action_100 (64) = happyShift action_72
action_100 (65) = happyShift action_73
action_100 (70) = happyShift action_74
action_100 (16) = happyGoto action_121
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (26) = happyShift action_58
action_101 (27) = happyShift action_59
action_101 (28) = happyShift action_60
action_101 (29) = happyShift action_61
action_101 (30) = happyShift action_62
action_101 (31) = happyShift action_63
action_101 (41) = happyShift action_65
action_101 (52) = happyShift action_70
action_101 (53) = happyShift action_71
action_101 (64) = happyShift action_72
action_101 (65) = happyShift action_73
action_101 (70) = happyShift action_74
action_101 (16) = happyGoto action_120
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (26) = happyShift action_58
action_102 (27) = happyShift action_59
action_102 (28) = happyShift action_60
action_102 (29) = happyShift action_61
action_102 (30) = happyShift action_62
action_102 (31) = happyShift action_63
action_102 (41) = happyShift action_65
action_102 (52) = happyShift action_70
action_102 (53) = happyShift action_71
action_102 (64) = happyShift action_72
action_102 (65) = happyShift action_73
action_102 (70) = happyShift action_74
action_102 (16) = happyGoto action_119
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (26) = happyShift action_58
action_103 (27) = happyShift action_59
action_103 (28) = happyShift action_60
action_103 (29) = happyShift action_61
action_103 (30) = happyShift action_62
action_103 (31) = happyShift action_63
action_103 (41) = happyShift action_65
action_103 (52) = happyShift action_70
action_103 (53) = happyShift action_71
action_103 (64) = happyShift action_72
action_103 (65) = happyShift action_73
action_103 (70) = happyShift action_74
action_103 (16) = happyGoto action_118
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (26) = happyShift action_58
action_104 (27) = happyShift action_59
action_104 (28) = happyShift action_60
action_104 (29) = happyShift action_61
action_104 (30) = happyShift action_62
action_104 (31) = happyShift action_63
action_104 (41) = happyShift action_65
action_104 (52) = happyShift action_70
action_104 (53) = happyShift action_71
action_104 (64) = happyShift action_72
action_104 (65) = happyShift action_73
action_104 (70) = happyShift action_74
action_104 (16) = happyGoto action_117
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (26) = happyShift action_58
action_105 (27) = happyShift action_59
action_105 (28) = happyShift action_60
action_105 (29) = happyShift action_61
action_105 (30) = happyShift action_62
action_105 (31) = happyShift action_63
action_105 (41) = happyShift action_65
action_105 (52) = happyShift action_70
action_105 (53) = happyShift action_71
action_105 (64) = happyShift action_72
action_105 (65) = happyShift action_73
action_105 (70) = happyShift action_74
action_105 (16) = happyGoto action_116
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (26) = happyShift action_58
action_106 (27) = happyShift action_59
action_106 (28) = happyShift action_60
action_106 (29) = happyShift action_61
action_106 (30) = happyShift action_62
action_106 (31) = happyShift action_63
action_106 (41) = happyShift action_65
action_106 (52) = happyShift action_70
action_106 (53) = happyShift action_71
action_106 (64) = happyShift action_72
action_106 (65) = happyShift action_73
action_106 (70) = happyShift action_74
action_106 (16) = happyGoto action_115
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (26) = happyShift action_58
action_107 (27) = happyShift action_59
action_107 (28) = happyShift action_60
action_107 (29) = happyShift action_61
action_107 (30) = happyShift action_62
action_107 (31) = happyShift action_63
action_107 (41) = happyShift action_65
action_107 (52) = happyShift action_70
action_107 (53) = happyShift action_71
action_107 (64) = happyShift action_72
action_107 (65) = happyShift action_73
action_107 (70) = happyShift action_74
action_107 (16) = happyGoto action_114
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (26) = happyShift action_58
action_108 (27) = happyShift action_59
action_108 (28) = happyShift action_60
action_108 (29) = happyShift action_61
action_108 (30) = happyShift action_62
action_108 (31) = happyShift action_63
action_108 (41) = happyShift action_65
action_108 (52) = happyShift action_70
action_108 (53) = happyShift action_71
action_108 (64) = happyShift action_72
action_108 (65) = happyShift action_73
action_108 (70) = happyShift action_74
action_108 (16) = happyGoto action_113
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (31) = happyShift action_112
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (31) = happyShift action_111
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_56

action_112 _ = happyReduce_54

action_113 (45) = happyShift action_92
action_113 (51) = happyShift action_93
action_113 (52) = happyShift action_94
action_113 (53) = happyShift action_95
action_113 (54) = happyShift action_96
action_113 (55) = happyShift action_97
action_113 (56) = happyShift action_98
action_113 (57) = happyShift action_99
action_113 (58) = happyShift action_100
action_113 (59) = happyShift action_101
action_113 (60) = happyShift action_102
action_113 (61) = happyShift action_103
action_113 (65) = happyShift action_106
action_113 (68) = happyShift action_109
action_113 (69) = happyShift action_110
action_113 _ = happyReduce_36

action_114 (45) = happyShift action_92
action_114 (51) = happyShift action_93
action_114 (52) = happyShift action_94
action_114 (53) = happyShift action_95
action_114 (54) = happyShift action_96
action_114 (55) = happyShift action_97
action_114 (56) = happyShift action_98
action_114 (57) = happyShift action_99
action_114 (58) = happyShift action_100
action_114 (59) = happyShift action_101
action_114 (60) = happyShift action_102
action_114 (61) = happyShift action_103
action_114 (65) = happyShift action_106
action_114 (67) = happyShift action_108
action_114 (68) = happyShift action_109
action_114 (69) = happyShift action_110
action_114 _ = happyReduce_35

action_115 (45) = happyShift action_92
action_115 (51) = happyShift action_93
action_115 (52) = happyShift action_94
action_115 (53) = happyShift action_95
action_115 (54) = happyShift action_96
action_115 (55) = happyShift action_97
action_115 (56) = happyShift action_98
action_115 (57) = happyShift action_99
action_115 (58) = happyShift action_100
action_115 (59) = happyShift action_101
action_115 (60) = happyShift action_102
action_115 (61) = happyShift action_103
action_115 (68) = happyShift action_109
action_115 (69) = happyShift action_110
action_115 _ = happyReduce_37

action_116 (45) = happyShift action_92
action_116 (51) = happyShift action_93
action_116 (52) = happyShift action_94
action_116 (53) = happyShift action_95
action_116 (54) = happyShift action_96
action_116 (55) = happyShift action_97
action_116 (56) = happyShift action_98
action_116 (57) = happyShift action_99
action_116 (58) = happyShift action_100
action_116 (59) = happyShift action_101
action_116 (60) = happyShift action_102
action_116 (61) = happyShift action_103
action_116 (62) = happyShift action_104
action_116 (65) = happyShift action_106
action_116 (66) = happyShift action_107
action_116 (67) = happyShift action_108
action_116 (68) = happyShift action_109
action_116 (69) = happyShift action_110
action_116 _ = happyReduce_33

action_117 (45) = happyShift action_92
action_117 (51) = happyShift action_93
action_117 (52) = happyShift action_94
action_117 (53) = happyShift action_95
action_117 (54) = happyShift action_96
action_117 (55) = happyShift action_97
action_117 (56) = happyShift action_98
action_117 (57) = happyShift action_99
action_117 (58) = happyShift action_100
action_117 (59) = happyShift action_101
action_117 (60) = happyShift action_102
action_117 (61) = happyShift action_103
action_117 (65) = happyShift action_106
action_117 (66) = happyShift action_107
action_117 (67) = happyShift action_108
action_117 (68) = happyShift action_109
action_117 (69) = happyShift action_110
action_117 _ = happyReduce_34

action_118 (45) = happyShift action_92
action_118 (51) = happyShift action_93
action_118 (52) = happyShift action_94
action_118 (53) = happyShift action_95
action_118 (54) = happyShift action_96
action_118 (55) = happyShift action_97
action_118 (68) = happyShift action_109
action_118 (69) = happyShift action_110
action_118 _ = happyReduce_40

action_119 (45) = happyShift action_92
action_119 (51) = happyShift action_93
action_119 (52) = happyShift action_94
action_119 (53) = happyShift action_95
action_119 (54) = happyShift action_96
action_119 (55) = happyShift action_97
action_119 (68) = happyShift action_109
action_119 (69) = happyShift action_110
action_119 _ = happyReduce_41

action_120 (45) = happyShift action_92
action_120 (51) = happyShift action_93
action_120 (52) = happyShift action_94
action_120 (53) = happyShift action_95
action_120 (54) = happyShift action_96
action_120 (55) = happyShift action_97
action_120 (68) = happyShift action_109
action_120 (69) = happyShift action_110
action_120 _ = happyReduce_42

action_121 (45) = happyShift action_92
action_121 (51) = happyShift action_93
action_121 (52) = happyShift action_94
action_121 (53) = happyShift action_95
action_121 (54) = happyShift action_96
action_121 (55) = happyShift action_97
action_121 (68) = happyShift action_109
action_121 (69) = happyShift action_110
action_121 _ = happyReduce_43

action_122 (45) = happyShift action_92
action_122 (51) = happyShift action_93
action_122 (52) = happyShift action_94
action_122 (53) = happyShift action_95
action_122 (54) = happyShift action_96
action_122 (55) = happyShift action_97
action_122 (58) = happyShift action_100
action_122 (59) = happyShift action_101
action_122 (60) = happyShift action_102
action_122 (61) = happyShift action_103
action_122 (68) = happyShift action_109
action_122 (69) = happyShift action_110
action_122 _ = happyReduce_39

action_123 (45) = happyShift action_92
action_123 (51) = happyShift action_93
action_123 (52) = happyShift action_94
action_123 (53) = happyShift action_95
action_123 (54) = happyShift action_96
action_123 (55) = happyShift action_97
action_123 (58) = happyShift action_100
action_123 (59) = happyShift action_101
action_123 (60) = happyShift action_102
action_123 (61) = happyShift action_103
action_123 (68) = happyShift action_109
action_123 (69) = happyShift action_110
action_123 _ = happyReduce_38

action_124 (45) = happyShift action_92
action_124 (68) = happyShift action_109
action_124 (69) = happyShift action_110
action_124 _ = happyReduce_48

action_125 (45) = happyShift action_92
action_125 (68) = happyShift action_109
action_125 (69) = happyShift action_110
action_125 _ = happyReduce_47

action_126 (45) = happyShift action_92
action_126 (68) = happyShift action_109
action_126 (69) = happyShift action_110
action_126 _ = happyReduce_46

action_127 (45) = happyShift action_92
action_127 (53) = happyShift action_95
action_127 (54) = happyShift action_96
action_127 (55) = happyShift action_97
action_127 (68) = happyShift action_109
action_127 (69) = happyShift action_110
action_127 _ = happyReduce_45

action_128 (45) = happyShift action_92
action_128 (53) = happyShift action_95
action_128 (54) = happyShift action_96
action_128 (55) = happyShift action_97
action_128 (68) = happyShift action_109
action_128 (69) = happyShift action_110
action_128 _ = happyReduce_44

action_129 (38) = happyShift action_90
action_129 (45) = happyShift action_92
action_129 (46) = happyShift action_148
action_129 (51) = happyShift action_93
action_129 (52) = happyShift action_94
action_129 (53) = happyShift action_95
action_129 (54) = happyShift action_96
action_129 (55) = happyShift action_97
action_129 (56) = happyShift action_98
action_129 (57) = happyShift action_99
action_129 (58) = happyShift action_100
action_129 (59) = happyShift action_101
action_129 (60) = happyShift action_102
action_129 (61) = happyShift action_103
action_129 (62) = happyShift action_104
action_129 (63) = happyShift action_105
action_129 (65) = happyShift action_106
action_129 (66) = happyShift action_107
action_129 (67) = happyShift action_108
action_129 (68) = happyShift action_109
action_129 (69) = happyShift action_110
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (38) = happyShift action_90
action_130 (45) = happyShift action_92
action_130 (51) = happyShift action_93
action_130 (52) = happyShift action_94
action_130 (53) = happyShift action_95
action_130 (54) = happyShift action_96
action_130 (55) = happyShift action_97
action_130 (56) = happyShift action_98
action_130 (57) = happyShift action_99
action_130 (58) = happyShift action_100
action_130 (59) = happyShift action_101
action_130 (60) = happyShift action_102
action_130 (61) = happyShift action_103
action_130 (62) = happyShift action_104
action_130 (63) = happyShift action_105
action_130 (65) = happyShift action_106
action_130 (66) = happyShift action_107
action_130 (67) = happyShift action_108
action_130 (68) = happyShift action_109
action_130 (69) = happyShift action_110
action_130 _ = happyReduce_61

action_131 (38) = happyShift action_90
action_131 (45) = happyShift action_92
action_131 (51) = happyShift action_93
action_131 (52) = happyShift action_94
action_131 (53) = happyShift action_95
action_131 (54) = happyShift action_96
action_131 (55) = happyShift action_97
action_131 (56) = happyShift action_98
action_131 (57) = happyShift action_99
action_131 (58) = happyShift action_100
action_131 (59) = happyShift action_101
action_131 (60) = happyShift action_102
action_131 (61) = happyShift action_103
action_131 (62) = happyShift action_104
action_131 (63) = happyShift action_105
action_131 (65) = happyShift action_106
action_131 (66) = happyShift action_107
action_131 (67) = happyShift action_108
action_131 (68) = happyShift action_109
action_131 (69) = happyShift action_110
action_131 _ = happyReduce_75

action_132 (39) = happyShift action_146
action_132 (42) = happyShift action_147
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_60

action_134 _ = happyReduce_25

action_135 (42) = happyShift action_145
action_135 (45) = happyShift action_37
action_135 _ = happyFail (happyExpListPerState 135)

action_136 _ = happyReduce_49

action_137 (40) = happyShift action_144
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (38) = happyShift action_90
action_138 (42) = happyShift action_143
action_138 (45) = happyShift action_92
action_138 (51) = happyShift action_93
action_138 (52) = happyShift action_94
action_138 (53) = happyShift action_95
action_138 (54) = happyShift action_96
action_138 (55) = happyShift action_97
action_138 (56) = happyShift action_98
action_138 (57) = happyShift action_99
action_138 (58) = happyShift action_100
action_138 (59) = happyShift action_101
action_138 (60) = happyShift action_102
action_138 (61) = happyShift action_103
action_138 (62) = happyShift action_104
action_138 (63) = happyShift action_105
action_138 (65) = happyShift action_106
action_138 (66) = happyShift action_107
action_138 (67) = happyShift action_108
action_138 (68) = happyShift action_109
action_138 (69) = happyShift action_110
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (38) = happyShift action_90
action_139 (42) = happyShift action_142
action_139 (45) = happyShift action_92
action_139 (51) = happyShift action_93
action_139 (52) = happyShift action_94
action_139 (53) = happyShift action_95
action_139 (54) = happyShift action_96
action_139 (55) = happyShift action_97
action_139 (56) = happyShift action_98
action_139 (57) = happyShift action_99
action_139 (58) = happyShift action_100
action_139 (59) = happyShift action_101
action_139 (60) = happyShift action_102
action_139 (61) = happyShift action_103
action_139 (62) = happyShift action_104
action_139 (63) = happyShift action_105
action_139 (65) = happyShift action_106
action_139 (66) = happyShift action_107
action_139 (67) = happyShift action_108
action_139 (68) = happyShift action_109
action_139 (69) = happyShift action_110
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (42) = happyShift action_141
action_140 _ = happyFail (happyExpListPerState 140)

action_141 _ = happyReduce_57

action_142 (26) = happyShift action_58
action_142 (27) = happyShift action_59
action_142 (28) = happyShift action_60
action_142 (29) = happyShift action_61
action_142 (30) = happyShift action_62
action_142 (31) = happyShift action_63
action_142 (32) = happyShift action_14
action_142 (33) = happyShift action_15
action_142 (34) = happyShift action_16
action_142 (35) = happyShift action_17
action_142 (36) = happyShift action_31
action_142 (37) = happyShift action_64
action_142 (41) = happyShift action_65
action_142 (43) = happyShift action_40
action_142 (47) = happyShift action_67
action_142 (48) = happyShift action_68
action_142 (49) = happyShift action_69
action_142 (52) = happyShift action_70
action_142 (53) = happyShift action_71
action_142 (64) = happyShift action_72
action_142 (65) = happyShift action_73
action_142 (70) = happyShift action_74
action_142 (9) = happyGoto action_54
action_142 (13) = happyGoto action_55
action_142 (15) = happyGoto action_153
action_142 (16) = happyGoto action_57
action_142 (18) = happyGoto action_30
action_142 (19) = happyGoto action_12
action_142 (20) = happyGoto action_13
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (26) = happyShift action_58
action_143 (27) = happyShift action_59
action_143 (28) = happyShift action_60
action_143 (29) = happyShift action_61
action_143 (30) = happyShift action_62
action_143 (31) = happyShift action_63
action_143 (32) = happyShift action_14
action_143 (33) = happyShift action_15
action_143 (34) = happyShift action_16
action_143 (35) = happyShift action_17
action_143 (36) = happyShift action_31
action_143 (37) = happyShift action_64
action_143 (41) = happyShift action_65
action_143 (43) = happyShift action_40
action_143 (47) = happyShift action_67
action_143 (48) = happyShift action_68
action_143 (49) = happyShift action_69
action_143 (52) = happyShift action_70
action_143 (53) = happyShift action_71
action_143 (64) = happyShift action_72
action_143 (65) = happyShift action_73
action_143 (70) = happyShift action_74
action_143 (9) = happyGoto action_54
action_143 (13) = happyGoto action_55
action_143 (15) = happyGoto action_152
action_143 (16) = happyGoto action_57
action_143 (18) = happyGoto action_30
action_143 (19) = happyGoto action_12
action_143 (20) = happyGoto action_13
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (26) = happyShift action_58
action_144 (27) = happyShift action_59
action_144 (28) = happyShift action_60
action_144 (29) = happyShift action_61
action_144 (30) = happyShift action_62
action_144 (31) = happyShift action_63
action_144 (41) = happyShift action_65
action_144 (52) = happyShift action_70
action_144 (53) = happyShift action_71
action_144 (64) = happyShift action_72
action_144 (65) = happyShift action_73
action_144 (70) = happyShift action_74
action_144 (16) = happyGoto action_87
action_144 (17) = happyGoto action_151
action_144 _ = happyReduce_62

action_145 (26) = happyShift action_58
action_145 (27) = happyShift action_59
action_145 (28) = happyShift action_60
action_145 (29) = happyShift action_61
action_145 (30) = happyShift action_62
action_145 (31) = happyShift action_63
action_145 (41) = happyShift action_65
action_145 (52) = happyShift action_70
action_145 (53) = happyShift action_71
action_145 (64) = happyShift action_72
action_145 (65) = happyShift action_73
action_145 (70) = happyShift action_74
action_145 (16) = happyGoto action_150
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (26) = happyShift action_58
action_146 (27) = happyShift action_59
action_146 (28) = happyShift action_60
action_146 (29) = happyShift action_61
action_146 (30) = happyShift action_62
action_146 (31) = happyShift action_63
action_146 (41) = happyShift action_65
action_146 (52) = happyShift action_70
action_146 (53) = happyShift action_71
action_146 (64) = happyShift action_72
action_146 (65) = happyShift action_73
action_146 (70) = happyShift action_74
action_146 (16) = happyGoto action_149
action_146 _ = happyFail (happyExpListPerState 146)

action_147 _ = happyReduce_59

action_148 _ = happyReduce_55

action_149 (38) = happyShift action_90
action_149 (45) = happyShift action_92
action_149 (51) = happyShift action_93
action_149 (52) = happyShift action_94
action_149 (53) = happyShift action_95
action_149 (54) = happyShift action_96
action_149 (55) = happyShift action_97
action_149 (56) = happyShift action_98
action_149 (57) = happyShift action_99
action_149 (58) = happyShift action_100
action_149 (59) = happyShift action_101
action_149 (60) = happyShift action_102
action_149 (61) = happyShift action_103
action_149 (62) = happyShift action_104
action_149 (63) = happyShift action_105
action_149 (65) = happyShift action_106
action_149 (66) = happyShift action_107
action_149 (67) = happyShift action_108
action_149 (68) = happyShift action_109
action_149 (69) = happyShift action_110
action_149 _ = happyReduce_76

action_150 (38) = happyShift action_90
action_150 (45) = happyShift action_92
action_150 (51) = happyShift action_93
action_150 (52) = happyShift action_94
action_150 (53) = happyShift action_95
action_150 (54) = happyShift action_96
action_150 (55) = happyShift action_97
action_150 (56) = happyShift action_98
action_150 (57) = happyShift action_99
action_150 (58) = happyShift action_100
action_150 (59) = happyShift action_101
action_150 (60) = happyShift action_102
action_150 (61) = happyShift action_103
action_150 (62) = happyShift action_104
action_150 (63) = happyShift action_105
action_150 (65) = happyShift action_106
action_150 (66) = happyShift action_107
action_150 (67) = happyShift action_108
action_150 (68) = happyShift action_109
action_150 (69) = happyShift action_110
action_150 _ = happyReduce_58

action_151 (40) = happyShift action_155
action_151 _ = happyFail (happyExpListPerState 151)

action_152 _ = happyReduce_21

action_153 (50) = happyShift action_154
action_153 _ = happyReduce_24

action_154 (26) = happyShift action_58
action_154 (27) = happyShift action_59
action_154 (28) = happyShift action_60
action_154 (29) = happyShift action_61
action_154 (30) = happyShift action_62
action_154 (31) = happyShift action_63
action_154 (32) = happyShift action_14
action_154 (33) = happyShift action_15
action_154 (34) = happyShift action_16
action_154 (35) = happyShift action_17
action_154 (36) = happyShift action_31
action_154 (37) = happyShift action_64
action_154 (41) = happyShift action_65
action_154 (43) = happyShift action_40
action_154 (47) = happyShift action_67
action_154 (48) = happyShift action_68
action_154 (49) = happyShift action_69
action_154 (52) = happyShift action_70
action_154 (53) = happyShift action_71
action_154 (64) = happyShift action_72
action_154 (65) = happyShift action_73
action_154 (70) = happyShift action_74
action_154 (9) = happyGoto action_54
action_154 (13) = happyGoto action_55
action_154 (15) = happyGoto action_157
action_154 (16) = happyGoto action_57
action_154 (18) = happyGoto action_30
action_154 (19) = happyGoto action_12
action_154 (20) = happyGoto action_13
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (26) = happyShift action_58
action_155 (27) = happyShift action_59
action_155 (28) = happyShift action_60
action_155 (29) = happyShift action_61
action_155 (30) = happyShift action_62
action_155 (31) = happyShift action_63
action_155 (41) = happyShift action_65
action_155 (52) = happyShift action_70
action_155 (53) = happyShift action_71
action_155 (64) = happyShift action_72
action_155 (65) = happyShift action_73
action_155 (70) = happyShift action_74
action_155 (16) = happyGoto action_87
action_155 (17) = happyGoto action_156
action_155 _ = happyReduce_62

action_156 (42) = happyShift action_158
action_156 _ = happyFail (happyExpListPerState 156)

action_157 _ = happyReduce_23

action_158 (26) = happyShift action_58
action_158 (27) = happyShift action_59
action_158 (28) = happyShift action_60
action_158 (29) = happyShift action_61
action_158 (30) = happyShift action_62
action_158 (31) = happyShift action_63
action_158 (32) = happyShift action_14
action_158 (33) = happyShift action_15
action_158 (34) = happyShift action_16
action_158 (35) = happyShift action_17
action_158 (36) = happyShift action_31
action_158 (37) = happyShift action_64
action_158 (41) = happyShift action_65
action_158 (43) = happyShift action_40
action_158 (47) = happyShift action_67
action_158 (48) = happyShift action_68
action_158 (49) = happyShift action_69
action_158 (52) = happyShift action_70
action_158 (53) = happyShift action_71
action_158 (64) = happyShift action_72
action_158 (65) = happyShift action_73
action_158 (70) = happyShift action_74
action_158 (9) = happyGoto action_54
action_158 (13) = happyGoto action_55
action_158 (15) = happyGoto action_159
action_158 (16) = happyGoto action_57
action_158 (18) = happyGoto action_30
action_158 (19) = happyGoto action_12
action_158 (20) = happyGoto action_13
action_158 _ = happyFail (happyExpListPerState 158)

action_159 _ = happyReduce_22

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

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyAbsSyn24  happy_var_3)
	(HappyTerminal (L.Ident happy_var_2))
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn12
		 (P.Formal happy_var_1 happy_var_3 happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

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
		 (P.Binop happy_var_1 P.Or happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.And happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  16 happyReduction_35
happyReduction_35 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.BitwiseOr happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  16 happyReduction_36
happyReduction_36 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.BitwiseXor happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  16 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.BitwiseAnd happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  16 happyReduction_38
happyReduction_38 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Equal happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  16 happyReduction_39
happyReduction_39 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Neq happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  16 happyReduction_40
happyReduction_40 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Geq happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  16 happyReduction_41
happyReduction_41 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Greater happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  16 happyReduction_42
happyReduction_42 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Leq happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  16 happyReduction_43
happyReduction_43 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Less happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  16 happyReduction_44
happyReduction_44 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Add happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  16 happyReduction_45
happyReduction_45 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Sub happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  16 happyReduction_46
happyReduction_46 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Mul happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  16 happyReduction_47
happyReduction_47 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Div happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  16 happyReduction_48
happyReduction_48 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Binop happy_var_1 P.Mod happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  16 happyReduction_49
happyReduction_49 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.Nested happy_var_2
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  16 happyReduction_50
happyReduction_50 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.Negative happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  16 happyReduction_51
happyReduction_51 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.Negate happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  16 happyReduction_52
happyReduction_52 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.AddressOf happy_var_2
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  16 happyReduction_53
happyReduction_53 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.Deref happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  16 happyReduction_54
happyReduction_54 (HappyTerminal (L.Ident happy_var_3))
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.FieldAccess happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happyReduce 4 16 happyReduction_55
happyReduction_55 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.ArrayAccess happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_3  16 happyReduction_56
happyReduction_56 (HappyTerminal (L.Ident happy_var_3))
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Indirect happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happyReduce 4 16 happyReduction_57
happyReduction_57 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.Sizeof happy_var_3
	) `HappyStk` happyRest

happyReduce_58 = happyReduce 5 16 happyReduction_58
happyReduction_58 ((HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.Typecast happy_var_2 (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 4 16 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.Call happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_3  16 happyReduction_60
happyReduction_60 _
	_
	(HappyTerminal (L.Ident happy_var_1))
	 =  HappyAbsSyn16
		 (P.Call happy_var_1 []
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  16 happyReduction_61
happyReduction_61 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Assign happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_0  17 happyReduction_62
happyReduction_62  =  HappyAbsSyn17
		 (Nothing
	)

happyReduce_63 = happySpecReduce_1  17 happyReduction_63
happyReduction_63 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (Just happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  18 happyReduction_64
happyReduction_64 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  18 happyReduction_65
happyReduction_65 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  19 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn19
		 (P.PrimitiveType L.Int
	)

happyReduce_67 = happySpecReduce_1  19 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn19
		 (P.PrimitiveType L.Double
	)

happyReduce_68 = happySpecReduce_1  19 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn19
		 (P.PrimitiveType L.Char
	)

happyReduce_69 = happySpecReduce_1  19 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn19
		 (P.PrimitiveType L.Void
	)

happyReduce_70 = happySpecReduce_2  20 happyReduction_70
happyReduction_70 (HappyTerminal (L.Ident happy_var_2))
	_
	 =  HappyAbsSyn20
		 (P.StructType happy_var_2
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_0  21 happyReduction_71
happyReduction_71  =  HappyAbsSyn21
		 (0
	)

happyReduce_72 = happySpecReduce_2  21 happyReduction_72
happyReduction_72 _
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 + 1
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_0  22 happyReduction_73
happyReduction_73  =  HappyAbsSyn22
		 ([]
	)

happyReduce_74 = happyReduce 4 22 happyReduction_74
happyReduction_74 (_ `HappyStk`
	(HappyTerminal (L.LitInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (happy_var_3 : happy_var_1
	) `HappyStk` happyRest

happyReduce_75 = happySpecReduce_1  23 happyReduction_75
happyReduction_75 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  23 happyReduction_76
happyReduction_76 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_3 : happy_var_1
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_0  24 happyReduction_77
happyReduction_77  =  HappyAbsSyn24
		 (0
	)

happyReduce_78 = happySpecReduce_3  24 happyReduction_78
happyReduction_78 _
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 + 1
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_0  25 happyReduction_79
happyReduction_79  =  HappyAbsSyn25
		 (
	)

happyReduce_80 = happySpecReduce_3  25 happyReduction_80
happyReduction_80 _
	_
	_
	 =  HappyAbsSyn25
		 (
	)

happyNewToken action sts stk [] =
	action 73 73 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.LitInt happy_dollar_dollar -> cont 26;
	L.LitDouble happy_dollar_dollar -> cont 27;
	L.LitString happy_dollar_dollar -> cont 28;
	L.LitChar happy_dollar_dollar -> cont 29;
	L.LitNull -> cont 30;
	L.Ident happy_dollar_dollar -> cont 31;
	L.Type L.Int -> cont 32;
	L.Type L.Double -> cont 33;
	L.Type L.Char -> cont 34;
	L.Type L.Void -> cont 35;
	L.Struct -> cont 36;
	L.Return -> cont 37;
	L.Assign -> cont 38;
	L.Comma -> cont 39;
	L.Semi -> cont 40;
	L.LParen -> cont 41;
	L.RParen -> cont 42;
	L.LBrace -> cont 43;
	L.RBrace -> cont 44;
	L.LBrack -> cont 45;
	L.RBrack -> cont 46;
	L.For -> cont 47;
	L.While -> cont 48;
	L.If -> cont 49;
	L.Else -> cont 50;
	L.Plus -> cont 51;
	L.Minus -> cont 52;
	L.Asterisk -> cont 53;
	L.Div -> cont 54;
	L.Mod -> cont 55;
	L.Equal -> cont 56;
	L.Neq -> cont 57;
	L.Less -> cont 58;
	L.Leq -> cont 59;
	L.Greater -> cont 60;
	L.Geq -> cont 61;
	L.And -> cont 62;
	L.Or -> cont 63;
	L.Not -> cont 64;
	L.Ampers -> cont 65;
	L.Bar -> cont 66;
	L.Caret -> cont 67;
	L.Dot -> cont 68;
	L.Arrow -> cont 69;
	L.Sizeof -> cont 70;
	L.Include -> cont 71;
	L.Eof -> cont 72;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 73 tk tks = happyError' (tks, explist)
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
