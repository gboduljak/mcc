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
happyExpList = Happy_Data_Array.listArray (0,578) ([0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,248,0,2048,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,128,0,0,0,0,1024,0,0,0,0,8,0,0,0,1,0,0,0,8448,0,0,0,15872,8,0,0,0,0,0,0,0,63488,0,0,0,0,0,0,0,0,57344,515,0,0,0,32,0,0,0,16384,0,0,0,0,32768,4,0,0,0,0,0,0,0,512,0,0,0,0,18432,0,0,0,0,0,0,0,32768,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63488,0,0,0,0,36864,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,18431,1651,2144,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34048,65504,251,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,63488,1025,96,134,0,32752,49160,3072,1,0,0,0,0,0,0,32,0,0,0,16384,0,0,0,0,128,0,0,0,126,6145,8576,0,64512,512,48,67,0,504,24580,34304,0,61440,2051,192,268,0,0,16,0,0,49152,8703,768,1072,0,0,1024,0,6,0,0,8,3072,0,0,4096,0,24,0,0,32,12288,0,63488,1025,96,134,0,1008,49160,3072,1,57344,4103,384,536,0,0,33348,61439,3,0,32768,0,0,0,0,2064,49150,15,0,32768,0,0,0,64512,1536,48,67,0,504,24580,34304,0,0,0,0,0,0,2016,32784,6145,2,49152,8207,768,1072,0,8064,64,24582,8,0,32831,3072,4288,0,32256,256,32792,33,0,252,12290,17152,0,63488,1025,96,134,0,1008,49160,3072,1,57344,4103,384,536,0,4032,32,12291,4,32768,16415,1536,2144,0,16128,128,49164,16,0,126,6145,8576,0,64512,512,48,67,0,504,24580,34304,0,61440,2051,192,268,0,2016,32784,6145,2,0,8,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63520,12863,0,0,16384,32752,116,0,0,57472,49407,0,0,0,65473,499,0,0,33280,58367,3,0,0,7940,1536,0,0,2048,62,12,0,0,31760,6144,0,0,8192,248,48,0,0,61504,24697,0,0,32768,62432,192,0,0,256,32768,1,0,0,2,768,0,0,1024,0,6,0,0,14344,3072,0,0,4096,112,24,0,16384,63584,16127,0,0,16512,65520,125,0,0,57473,64511,0,0,9216,0,0,0,0,0,0,0,0,0,0,0,0,16128,128,49164,16,0,0,0,0,0,0,256,0,0,0,32768,61512,32255,0,0,37120,65504,251,0,0,49442,63487,1,0,0,0,0,0,0,1152,0,0,0,0,0,0,0,65024,50463,32793,33,0,16380,13194,17152,0,63488,1025,96,134,0,0,128,49152,0,57344,4103,384,536,0,0,0,0,0,0,0,0,0,0,0,2064,49150,15,0,32768,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,65504,40017,6145,2,49152,8207,768,1072,0,0,128,0,0,0,0,0,0,0,65024,50463,32793,33,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","program","construct_list","construct","structdecl","funcdecl","funcdefn","vardecl","vardecl_list","formals","formal","block","block_statements","statement","expr","opt_expr","type","primitive_type","struct_type","stars","array_sizes","actuals","includes","int","double","string","char","null","ident","tint","tdouble","tchar","tvoid","struct","return","'='","','","';'","'('","')'","'{'","'}'","'['","']'","for","while","if","else","'+'","'-'","'*'","'/'","'%'","'=='","'!='","'<'","'<='","'>'","'>='","'&&'","'||'","'!'","'&'","'|'","'^'","'.'","'->'","sizeof","include","eof","%eof"]
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
action_0 _ = happyReduce_81

action_1 (25) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (71) = happyShift action_5
action_2 (5) = happyGoto action_4
action_2 _ = happyReduce_2

action_3 (73) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (32) = happyShift action_15
action_4 (33) = happyShift action_16
action_4 (34) = happyShift action_17
action_4 (35) = happyShift action_18
action_4 (36) = happyShift action_19
action_4 (72) = happyShift action_20
action_4 (6) = happyGoto action_7
action_4 (7) = happyGoto action_8
action_4 (8) = happyGoto action_9
action_4 (9) = happyGoto action_10
action_4 (10) = happyGoto action_11
action_4 (19) = happyGoto action_12
action_4 (20) = happyGoto action_13
action_4 (21) = happyGoto action_14
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (28) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_82

action_7 _ = happyReduce_3

action_8 _ = happyReduce_6

action_9 _ = happyReduce_4

action_10 _ = happyReduce_5

action_11 _ = happyReduce_7

action_12 (31) = happyShift action_24
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (22) = happyGoto action_23
action_13 _ = happyReduce_75

action_14 (22) = happyGoto action_22
action_14 _ = happyReduce_75

action_15 _ = happyReduce_70

action_16 _ = happyReduce_71

action_17 _ = happyReduce_72

action_18 _ = happyReduce_73

action_19 (31) = happyShift action_21
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_1

action_21 (43) = happyShift action_28
action_21 _ = happyReduce_74

action_22 (53) = happyShift action_27
action_22 _ = happyReduce_69

action_23 (53) = happyShift action_27
action_23 _ = happyReduce_68

action_24 (41) = happyShift action_26
action_24 (23) = happyGoto action_25
action_24 _ = happyReduce_77

action_25 (40) = happyShift action_37
action_25 (45) = happyShift action_38
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (32) = happyShift action_15
action_26 (33) = happyShift action_16
action_26 (34) = happyShift action_17
action_26 (35) = happyShift action_18
action_26 (36) = happyShift action_32
action_26 (42) = happyShift action_36
action_26 (12) = happyGoto action_33
action_26 (13) = happyGoto action_34
action_26 (19) = happyGoto action_35
action_26 (20) = happyGoto action_13
action_26 (21) = happyGoto action_14
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_76

action_28 (32) = happyShift action_15
action_28 (33) = happyShift action_16
action_28 (34) = happyShift action_17
action_28 (35) = happyShift action_18
action_28 (36) = happyShift action_32
action_28 (10) = happyGoto action_29
action_28 (11) = happyGoto action_30
action_28 (19) = happyGoto action_31
action_28 (20) = happyGoto action_13
action_28 (21) = happyGoto action_14
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_15

action_30 (32) = happyShift action_15
action_30 (33) = happyShift action_16
action_30 (34) = happyShift action_17
action_30 (35) = happyShift action_18
action_30 (36) = happyShift action_32
action_30 (44) = happyShift action_49
action_30 (10) = happyGoto action_48
action_30 (19) = happyGoto action_31
action_30 (20) = happyGoto action_13
action_30 (21) = happyGoto action_14
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (31) = happyShift action_47
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (31) = happyShift action_46
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (39) = happyShift action_44
action_33 (42) = happyShift action_45
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_16

action_35 (31) = happyShift action_43
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (40) = happyShift action_41
action_36 (43) = happyShift action_42
action_36 (14) = happyGoto action_40
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_13

action_38 (26) = happyShift action_39
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (46) = happyShift action_55
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_11

action_41 _ = happyReduce_9

action_42 (15) = happyGoto action_54
action_42 _ = happyReduce_20

action_43 _ = happyReduce_18

action_44 (32) = happyShift action_15
action_44 (33) = happyShift action_16
action_44 (34) = happyShift action_17
action_44 (35) = happyShift action_18
action_44 (36) = happyShift action_32
action_44 (13) = happyGoto action_53
action_44 (19) = happyGoto action_35
action_44 (20) = happyGoto action_13
action_44 (21) = happyGoto action_14
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (40) = happyShift action_52
action_45 (43) = happyShift action_42
action_45 (14) = happyGoto action_51
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_74

action_47 (23) = happyGoto action_25
action_47 _ = happyReduce_77

action_48 _ = happyReduce_14

action_49 (40) = happyShift action_50
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_8

action_51 _ = happyReduce_12

action_52 _ = happyReduce_10

action_53 _ = happyReduce_17

action_54 (26) = happyShift action_60
action_54 (27) = happyShift action_61
action_54 (28) = happyShift action_62
action_54 (29) = happyShift action_63
action_54 (30) = happyShift action_64
action_54 (31) = happyShift action_65
action_54 (32) = happyShift action_15
action_54 (33) = happyShift action_16
action_54 (34) = happyShift action_17
action_54 (35) = happyShift action_18
action_54 (36) = happyShift action_32
action_54 (37) = happyShift action_66
action_54 (41) = happyShift action_67
action_54 (43) = happyShift action_42
action_54 (44) = happyShift action_68
action_54 (47) = happyShift action_69
action_54 (48) = happyShift action_70
action_54 (49) = happyShift action_71
action_54 (52) = happyShift action_72
action_54 (53) = happyShift action_73
action_54 (64) = happyShift action_74
action_54 (65) = happyShift action_75
action_54 (70) = happyShift action_76
action_54 (10) = happyGoto action_56
action_54 (14) = happyGoto action_57
action_54 (16) = happyGoto action_58
action_54 (17) = happyGoto action_59
action_54 (19) = happyGoto action_31
action_54 (20) = happyGoto action_13
action_54 (21) = happyGoto action_14
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_78

action_56 _ = happyReduce_29

action_57 _ = happyReduce_23

action_58 _ = happyReduce_21

action_59 (38) = happyShift action_90
action_59 (40) = happyShift action_91
action_59 (45) = happyShift action_92
action_59 (51) = happyShift action_93
action_59 (52) = happyShift action_94
action_59 (53) = happyShift action_95
action_59 (54) = happyShift action_96
action_59 (55) = happyShift action_97
action_59 (56) = happyShift action_98
action_59 (57) = happyShift action_99
action_59 (58) = happyShift action_100
action_59 (59) = happyShift action_101
action_59 (60) = happyShift action_102
action_59 (61) = happyShift action_103
action_59 (62) = happyShift action_104
action_59 (63) = happyShift action_105
action_59 (65) = happyShift action_106
action_59 (66) = happyShift action_107
action_59 (67) = happyShift action_108
action_59 (68) = happyShift action_109
action_59 (69) = happyShift action_110
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_30

action_61 _ = happyReduce_31

action_62 _ = happyReduce_33

action_63 _ = happyReduce_32

action_64 _ = happyReduce_34

action_65 (41) = happyShift action_89
action_65 _ = happyReduce_35

action_66 (26) = happyShift action_60
action_66 (27) = happyShift action_61
action_66 (28) = happyShift action_62
action_66 (29) = happyShift action_63
action_66 (30) = happyShift action_64
action_66 (31) = happyShift action_65
action_66 (41) = happyShift action_67
action_66 (52) = happyShift action_72
action_66 (53) = happyShift action_73
action_66 (64) = happyShift action_74
action_66 (65) = happyShift action_75
action_66 (70) = happyShift action_76
action_66 (17) = happyGoto action_87
action_66 (18) = happyGoto action_88
action_66 _ = happyReduce_66

action_67 (26) = happyShift action_60
action_67 (27) = happyShift action_61
action_67 (28) = happyShift action_62
action_67 (29) = happyShift action_63
action_67 (30) = happyShift action_64
action_67 (31) = happyShift action_65
action_67 (32) = happyShift action_15
action_67 (33) = happyShift action_16
action_67 (34) = happyShift action_17
action_67 (35) = happyShift action_18
action_67 (36) = happyShift action_32
action_67 (41) = happyShift action_67
action_67 (52) = happyShift action_72
action_67 (53) = happyShift action_73
action_67 (64) = happyShift action_74
action_67 (65) = happyShift action_75
action_67 (70) = happyShift action_76
action_67 (17) = happyGoto action_85
action_67 (19) = happyGoto action_86
action_67 (20) = happyGoto action_13
action_67 (21) = happyGoto action_14
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_19

action_69 (41) = happyShift action_84
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (41) = happyShift action_83
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (41) = happyShift action_82
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (26) = happyShift action_60
action_72 (27) = happyShift action_61
action_72 (28) = happyShift action_62
action_72 (29) = happyShift action_63
action_72 (30) = happyShift action_64
action_72 (31) = happyShift action_65
action_72 (41) = happyShift action_67
action_72 (52) = happyShift action_72
action_72 (53) = happyShift action_73
action_72 (64) = happyShift action_74
action_72 (65) = happyShift action_75
action_72 (70) = happyShift action_76
action_72 (17) = happyGoto action_81
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (26) = happyShift action_60
action_73 (27) = happyShift action_61
action_73 (28) = happyShift action_62
action_73 (29) = happyShift action_63
action_73 (30) = happyShift action_64
action_73 (31) = happyShift action_65
action_73 (41) = happyShift action_67
action_73 (52) = happyShift action_72
action_73 (53) = happyShift action_73
action_73 (64) = happyShift action_74
action_73 (65) = happyShift action_75
action_73 (70) = happyShift action_76
action_73 (17) = happyGoto action_80
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (26) = happyShift action_60
action_74 (27) = happyShift action_61
action_74 (28) = happyShift action_62
action_74 (29) = happyShift action_63
action_74 (30) = happyShift action_64
action_74 (31) = happyShift action_65
action_74 (41) = happyShift action_67
action_74 (52) = happyShift action_72
action_74 (53) = happyShift action_73
action_74 (64) = happyShift action_74
action_74 (65) = happyShift action_75
action_74 (70) = happyShift action_76
action_74 (17) = happyGoto action_79
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (26) = happyShift action_60
action_75 (27) = happyShift action_61
action_75 (28) = happyShift action_62
action_75 (29) = happyShift action_63
action_75 (30) = happyShift action_64
action_75 (31) = happyShift action_65
action_75 (41) = happyShift action_67
action_75 (52) = happyShift action_72
action_75 (53) = happyShift action_73
action_75 (64) = happyShift action_74
action_75 (65) = happyShift action_75
action_75 (70) = happyShift action_76
action_75 (17) = happyGoto action_78
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (41) = happyShift action_77
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (26) = happyShift action_60
action_77 (27) = happyShift action_61
action_77 (28) = happyShift action_62
action_77 (29) = happyShift action_63
action_77 (30) = happyShift action_64
action_77 (31) = happyShift action_65
action_77 (32) = happyShift action_15
action_77 (33) = happyShift action_16
action_77 (34) = happyShift action_17
action_77 (35) = happyShift action_18
action_77 (36) = happyShift action_32
action_77 (41) = happyShift action_67
action_77 (52) = happyShift action_72
action_77 (53) = happyShift action_73
action_77 (64) = happyShift action_74
action_77 (65) = happyShift action_75
action_77 (70) = happyShift action_76
action_77 (17) = happyGoto action_140
action_77 (19) = happyGoto action_141
action_77 (20) = happyGoto action_13
action_77 (21) = happyGoto action_14
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (45) = happyShift action_92
action_78 (68) = happyShift action_109
action_78 (69) = happyShift action_110
action_78 _ = happyReduce_56

action_79 (45) = happyShift action_92
action_79 (68) = happyShift action_109
action_79 (69) = happyShift action_110
action_79 _ = happyReduce_54

action_80 (45) = happyShift action_92
action_80 (68) = happyShift action_109
action_80 (69) = happyShift action_110
action_80 _ = happyReduce_57

action_81 (45) = happyShift action_92
action_81 (68) = happyShift action_109
action_81 (69) = happyShift action_110
action_81 _ = happyReduce_55

action_82 (26) = happyShift action_60
action_82 (27) = happyShift action_61
action_82 (28) = happyShift action_62
action_82 (29) = happyShift action_63
action_82 (30) = happyShift action_64
action_82 (31) = happyShift action_65
action_82 (41) = happyShift action_67
action_82 (52) = happyShift action_72
action_82 (53) = happyShift action_73
action_82 (64) = happyShift action_74
action_82 (65) = happyShift action_75
action_82 (70) = happyShift action_76
action_82 (17) = happyGoto action_139
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (26) = happyShift action_60
action_83 (27) = happyShift action_61
action_83 (28) = happyShift action_62
action_83 (29) = happyShift action_63
action_83 (30) = happyShift action_64
action_83 (31) = happyShift action_65
action_83 (41) = happyShift action_67
action_83 (52) = happyShift action_72
action_83 (53) = happyShift action_73
action_83 (64) = happyShift action_74
action_83 (65) = happyShift action_75
action_83 (70) = happyShift action_76
action_83 (17) = happyGoto action_138
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (26) = happyShift action_60
action_84 (27) = happyShift action_61
action_84 (28) = happyShift action_62
action_84 (29) = happyShift action_63
action_84 (30) = happyShift action_64
action_84 (31) = happyShift action_65
action_84 (41) = happyShift action_67
action_84 (52) = happyShift action_72
action_84 (53) = happyShift action_73
action_84 (64) = happyShift action_74
action_84 (65) = happyShift action_75
action_84 (70) = happyShift action_76
action_84 (17) = happyGoto action_87
action_84 (18) = happyGoto action_137
action_84 _ = happyReduce_66

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

action_86 (42) = happyShift action_135
action_86 _ = happyFail (happyExpListPerState 86)

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
action_87 _ = happyReduce_67

action_88 (40) = happyShift action_134
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (26) = happyShift action_60
action_89 (27) = happyShift action_61
action_89 (28) = happyShift action_62
action_89 (29) = happyShift action_63
action_89 (30) = happyShift action_64
action_89 (31) = happyShift action_65
action_89 (41) = happyShift action_67
action_89 (42) = happyShift action_133
action_89 (52) = happyShift action_72
action_89 (53) = happyShift action_73
action_89 (64) = happyShift action_74
action_89 (65) = happyShift action_75
action_89 (70) = happyShift action_76
action_89 (17) = happyGoto action_131
action_89 (24) = happyGoto action_132
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (26) = happyShift action_60
action_90 (27) = happyShift action_61
action_90 (28) = happyShift action_62
action_90 (29) = happyShift action_63
action_90 (30) = happyShift action_64
action_90 (31) = happyShift action_65
action_90 (41) = happyShift action_67
action_90 (52) = happyShift action_72
action_90 (53) = happyShift action_73
action_90 (64) = happyShift action_74
action_90 (65) = happyShift action_75
action_90 (70) = happyShift action_76
action_90 (17) = happyGoto action_130
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_22

action_92 (26) = happyShift action_60
action_92 (27) = happyShift action_61
action_92 (28) = happyShift action_62
action_92 (29) = happyShift action_63
action_92 (30) = happyShift action_64
action_92 (31) = happyShift action_65
action_92 (41) = happyShift action_67
action_92 (52) = happyShift action_72
action_92 (53) = happyShift action_73
action_92 (64) = happyShift action_74
action_92 (65) = happyShift action_75
action_92 (70) = happyShift action_76
action_92 (17) = happyGoto action_129
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (26) = happyShift action_60
action_93 (27) = happyShift action_61
action_93 (28) = happyShift action_62
action_93 (29) = happyShift action_63
action_93 (30) = happyShift action_64
action_93 (31) = happyShift action_65
action_93 (41) = happyShift action_67
action_93 (52) = happyShift action_72
action_93 (53) = happyShift action_73
action_93 (64) = happyShift action_74
action_93 (65) = happyShift action_75
action_93 (70) = happyShift action_76
action_93 (17) = happyGoto action_128
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (26) = happyShift action_60
action_94 (27) = happyShift action_61
action_94 (28) = happyShift action_62
action_94 (29) = happyShift action_63
action_94 (30) = happyShift action_64
action_94 (31) = happyShift action_65
action_94 (41) = happyShift action_67
action_94 (52) = happyShift action_72
action_94 (53) = happyShift action_73
action_94 (64) = happyShift action_74
action_94 (65) = happyShift action_75
action_94 (70) = happyShift action_76
action_94 (17) = happyGoto action_127
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (26) = happyShift action_60
action_95 (27) = happyShift action_61
action_95 (28) = happyShift action_62
action_95 (29) = happyShift action_63
action_95 (30) = happyShift action_64
action_95 (31) = happyShift action_65
action_95 (41) = happyShift action_67
action_95 (52) = happyShift action_72
action_95 (53) = happyShift action_73
action_95 (64) = happyShift action_74
action_95 (65) = happyShift action_75
action_95 (70) = happyShift action_76
action_95 (17) = happyGoto action_126
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (26) = happyShift action_60
action_96 (27) = happyShift action_61
action_96 (28) = happyShift action_62
action_96 (29) = happyShift action_63
action_96 (30) = happyShift action_64
action_96 (31) = happyShift action_65
action_96 (41) = happyShift action_67
action_96 (52) = happyShift action_72
action_96 (53) = happyShift action_73
action_96 (64) = happyShift action_74
action_96 (65) = happyShift action_75
action_96 (70) = happyShift action_76
action_96 (17) = happyGoto action_125
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (26) = happyShift action_60
action_97 (27) = happyShift action_61
action_97 (28) = happyShift action_62
action_97 (29) = happyShift action_63
action_97 (30) = happyShift action_64
action_97 (31) = happyShift action_65
action_97 (41) = happyShift action_67
action_97 (52) = happyShift action_72
action_97 (53) = happyShift action_73
action_97 (64) = happyShift action_74
action_97 (65) = happyShift action_75
action_97 (70) = happyShift action_76
action_97 (17) = happyGoto action_124
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (26) = happyShift action_60
action_98 (27) = happyShift action_61
action_98 (28) = happyShift action_62
action_98 (29) = happyShift action_63
action_98 (30) = happyShift action_64
action_98 (31) = happyShift action_65
action_98 (41) = happyShift action_67
action_98 (52) = happyShift action_72
action_98 (53) = happyShift action_73
action_98 (64) = happyShift action_74
action_98 (65) = happyShift action_75
action_98 (70) = happyShift action_76
action_98 (17) = happyGoto action_123
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (26) = happyShift action_60
action_99 (27) = happyShift action_61
action_99 (28) = happyShift action_62
action_99 (29) = happyShift action_63
action_99 (30) = happyShift action_64
action_99 (31) = happyShift action_65
action_99 (41) = happyShift action_67
action_99 (52) = happyShift action_72
action_99 (53) = happyShift action_73
action_99 (64) = happyShift action_74
action_99 (65) = happyShift action_75
action_99 (70) = happyShift action_76
action_99 (17) = happyGoto action_122
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (26) = happyShift action_60
action_100 (27) = happyShift action_61
action_100 (28) = happyShift action_62
action_100 (29) = happyShift action_63
action_100 (30) = happyShift action_64
action_100 (31) = happyShift action_65
action_100 (41) = happyShift action_67
action_100 (52) = happyShift action_72
action_100 (53) = happyShift action_73
action_100 (64) = happyShift action_74
action_100 (65) = happyShift action_75
action_100 (70) = happyShift action_76
action_100 (17) = happyGoto action_121
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (26) = happyShift action_60
action_101 (27) = happyShift action_61
action_101 (28) = happyShift action_62
action_101 (29) = happyShift action_63
action_101 (30) = happyShift action_64
action_101 (31) = happyShift action_65
action_101 (41) = happyShift action_67
action_101 (52) = happyShift action_72
action_101 (53) = happyShift action_73
action_101 (64) = happyShift action_74
action_101 (65) = happyShift action_75
action_101 (70) = happyShift action_76
action_101 (17) = happyGoto action_120
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (26) = happyShift action_60
action_102 (27) = happyShift action_61
action_102 (28) = happyShift action_62
action_102 (29) = happyShift action_63
action_102 (30) = happyShift action_64
action_102 (31) = happyShift action_65
action_102 (41) = happyShift action_67
action_102 (52) = happyShift action_72
action_102 (53) = happyShift action_73
action_102 (64) = happyShift action_74
action_102 (65) = happyShift action_75
action_102 (70) = happyShift action_76
action_102 (17) = happyGoto action_119
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (26) = happyShift action_60
action_103 (27) = happyShift action_61
action_103 (28) = happyShift action_62
action_103 (29) = happyShift action_63
action_103 (30) = happyShift action_64
action_103 (31) = happyShift action_65
action_103 (41) = happyShift action_67
action_103 (52) = happyShift action_72
action_103 (53) = happyShift action_73
action_103 (64) = happyShift action_74
action_103 (65) = happyShift action_75
action_103 (70) = happyShift action_76
action_103 (17) = happyGoto action_118
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (26) = happyShift action_60
action_104 (27) = happyShift action_61
action_104 (28) = happyShift action_62
action_104 (29) = happyShift action_63
action_104 (30) = happyShift action_64
action_104 (31) = happyShift action_65
action_104 (41) = happyShift action_67
action_104 (52) = happyShift action_72
action_104 (53) = happyShift action_73
action_104 (64) = happyShift action_74
action_104 (65) = happyShift action_75
action_104 (70) = happyShift action_76
action_104 (17) = happyGoto action_117
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (26) = happyShift action_60
action_105 (27) = happyShift action_61
action_105 (28) = happyShift action_62
action_105 (29) = happyShift action_63
action_105 (30) = happyShift action_64
action_105 (31) = happyShift action_65
action_105 (41) = happyShift action_67
action_105 (52) = happyShift action_72
action_105 (53) = happyShift action_73
action_105 (64) = happyShift action_74
action_105 (65) = happyShift action_75
action_105 (70) = happyShift action_76
action_105 (17) = happyGoto action_116
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (26) = happyShift action_60
action_106 (27) = happyShift action_61
action_106 (28) = happyShift action_62
action_106 (29) = happyShift action_63
action_106 (30) = happyShift action_64
action_106 (31) = happyShift action_65
action_106 (41) = happyShift action_67
action_106 (52) = happyShift action_72
action_106 (53) = happyShift action_73
action_106 (64) = happyShift action_74
action_106 (65) = happyShift action_75
action_106 (70) = happyShift action_76
action_106 (17) = happyGoto action_115
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (26) = happyShift action_60
action_107 (27) = happyShift action_61
action_107 (28) = happyShift action_62
action_107 (29) = happyShift action_63
action_107 (30) = happyShift action_64
action_107 (31) = happyShift action_65
action_107 (41) = happyShift action_67
action_107 (52) = happyShift action_72
action_107 (53) = happyShift action_73
action_107 (64) = happyShift action_74
action_107 (65) = happyShift action_75
action_107 (70) = happyShift action_76
action_107 (17) = happyGoto action_114
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (26) = happyShift action_60
action_108 (27) = happyShift action_61
action_108 (28) = happyShift action_62
action_108 (29) = happyShift action_63
action_108 (30) = happyShift action_64
action_108 (31) = happyShift action_65
action_108 (41) = happyShift action_67
action_108 (52) = happyShift action_72
action_108 (53) = happyShift action_73
action_108 (64) = happyShift action_74
action_108 (65) = happyShift action_75
action_108 (70) = happyShift action_76
action_108 (17) = happyGoto action_113
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (31) = happyShift action_112
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (31) = happyShift action_111
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_60

action_112 _ = happyReduce_58

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
action_113 _ = happyReduce_40

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
action_114 _ = happyReduce_39

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
action_115 _ = happyReduce_41

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
action_116 _ = happyReduce_37

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
action_117 _ = happyReduce_38

action_118 (45) = happyShift action_92
action_118 (51) = happyShift action_93
action_118 (52) = happyShift action_94
action_118 (53) = happyShift action_95
action_118 (54) = happyShift action_96
action_118 (55) = happyShift action_97
action_118 (68) = happyShift action_109
action_118 (69) = happyShift action_110
action_118 _ = happyReduce_44

action_119 (45) = happyShift action_92
action_119 (51) = happyShift action_93
action_119 (52) = happyShift action_94
action_119 (53) = happyShift action_95
action_119 (54) = happyShift action_96
action_119 (55) = happyShift action_97
action_119 (68) = happyShift action_109
action_119 (69) = happyShift action_110
action_119 _ = happyReduce_45

action_120 (45) = happyShift action_92
action_120 (51) = happyShift action_93
action_120 (52) = happyShift action_94
action_120 (53) = happyShift action_95
action_120 (54) = happyShift action_96
action_120 (55) = happyShift action_97
action_120 (68) = happyShift action_109
action_120 (69) = happyShift action_110
action_120 _ = happyReduce_46

action_121 (45) = happyShift action_92
action_121 (51) = happyShift action_93
action_121 (52) = happyShift action_94
action_121 (53) = happyShift action_95
action_121 (54) = happyShift action_96
action_121 (55) = happyShift action_97
action_121 (68) = happyShift action_109
action_121 (69) = happyShift action_110
action_121 _ = happyReduce_47

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
action_122 _ = happyReduce_43

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
action_123 _ = happyReduce_42

action_124 (45) = happyShift action_92
action_124 (68) = happyShift action_109
action_124 (69) = happyShift action_110
action_124 _ = happyReduce_52

action_125 (45) = happyShift action_92
action_125 (68) = happyShift action_109
action_125 (69) = happyShift action_110
action_125 _ = happyReduce_51

action_126 (45) = happyShift action_92
action_126 (68) = happyShift action_109
action_126 (69) = happyShift action_110
action_126 _ = happyReduce_50

action_127 (45) = happyShift action_92
action_127 (53) = happyShift action_95
action_127 (54) = happyShift action_96
action_127 (55) = happyShift action_97
action_127 (68) = happyShift action_109
action_127 (69) = happyShift action_110
action_127 _ = happyReduce_49

action_128 (45) = happyShift action_92
action_128 (53) = happyShift action_95
action_128 (54) = happyShift action_96
action_128 (55) = happyShift action_97
action_128 (68) = happyShift action_109
action_128 (69) = happyShift action_110
action_128 _ = happyReduce_48

action_129 (38) = happyShift action_90
action_129 (45) = happyShift action_92
action_129 (46) = happyShift action_150
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
action_130 _ = happyReduce_36

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
action_131 _ = happyReduce_79

action_132 (39) = happyShift action_148
action_132 (42) = happyShift action_149
action_132 _ = happyFail (happyExpListPerState 132)

action_133 _ = happyReduce_65

action_134 _ = happyReduce_28

action_135 (26) = happyShift action_60
action_135 (27) = happyShift action_61
action_135 (28) = happyShift action_62
action_135 (29) = happyShift action_63
action_135 (30) = happyShift action_64
action_135 (31) = happyShift action_65
action_135 (41) = happyShift action_67
action_135 (52) = happyShift action_72
action_135 (53) = happyShift action_73
action_135 (64) = happyShift action_74
action_135 (65) = happyShift action_75
action_135 (70) = happyShift action_76
action_135 (17) = happyGoto action_147
action_135 _ = happyFail (happyExpListPerState 135)

action_136 _ = happyReduce_53

action_137 (40) = happyShift action_146
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (38) = happyShift action_90
action_138 (42) = happyShift action_145
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
action_139 (42) = happyShift action_144
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

action_140 (38) = happyShift action_90
action_140 (42) = happyShift action_143
action_140 (45) = happyShift action_92
action_140 (51) = happyShift action_93
action_140 (52) = happyShift action_94
action_140 (53) = happyShift action_95
action_140 (54) = happyShift action_96
action_140 (55) = happyShift action_97
action_140 (56) = happyShift action_98
action_140 (57) = happyShift action_99
action_140 (58) = happyShift action_100
action_140 (59) = happyShift action_101
action_140 (60) = happyShift action_102
action_140 (61) = happyShift action_103
action_140 (62) = happyShift action_104
action_140 (63) = happyShift action_105
action_140 (65) = happyShift action_106
action_140 (66) = happyShift action_107
action_140 (67) = happyShift action_108
action_140 (68) = happyShift action_109
action_140 (69) = happyShift action_110
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (23) = happyGoto action_142
action_141 _ = happyReduce_77

action_142 (42) = happyShift action_155
action_142 (45) = happyShift action_38
action_142 _ = happyFail (happyExpListPerState 142)

action_143 _ = happyReduce_62

action_144 (26) = happyShift action_60
action_144 (27) = happyShift action_61
action_144 (28) = happyShift action_62
action_144 (29) = happyShift action_63
action_144 (30) = happyShift action_64
action_144 (31) = happyShift action_65
action_144 (32) = happyShift action_15
action_144 (33) = happyShift action_16
action_144 (34) = happyShift action_17
action_144 (35) = happyShift action_18
action_144 (36) = happyShift action_32
action_144 (37) = happyShift action_66
action_144 (41) = happyShift action_67
action_144 (43) = happyShift action_42
action_144 (47) = happyShift action_69
action_144 (48) = happyShift action_70
action_144 (49) = happyShift action_71
action_144 (52) = happyShift action_72
action_144 (53) = happyShift action_73
action_144 (64) = happyShift action_74
action_144 (65) = happyShift action_75
action_144 (70) = happyShift action_76
action_144 (10) = happyGoto action_56
action_144 (14) = happyGoto action_57
action_144 (16) = happyGoto action_154
action_144 (17) = happyGoto action_59
action_144 (19) = happyGoto action_31
action_144 (20) = happyGoto action_13
action_144 (21) = happyGoto action_14
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (26) = happyShift action_60
action_145 (27) = happyShift action_61
action_145 (28) = happyShift action_62
action_145 (29) = happyShift action_63
action_145 (30) = happyShift action_64
action_145 (31) = happyShift action_65
action_145 (32) = happyShift action_15
action_145 (33) = happyShift action_16
action_145 (34) = happyShift action_17
action_145 (35) = happyShift action_18
action_145 (36) = happyShift action_32
action_145 (37) = happyShift action_66
action_145 (41) = happyShift action_67
action_145 (43) = happyShift action_42
action_145 (47) = happyShift action_69
action_145 (48) = happyShift action_70
action_145 (49) = happyShift action_71
action_145 (52) = happyShift action_72
action_145 (53) = happyShift action_73
action_145 (64) = happyShift action_74
action_145 (65) = happyShift action_75
action_145 (70) = happyShift action_76
action_145 (10) = happyGoto action_56
action_145 (14) = happyGoto action_57
action_145 (16) = happyGoto action_153
action_145 (17) = happyGoto action_59
action_145 (19) = happyGoto action_31
action_145 (20) = happyGoto action_13
action_145 (21) = happyGoto action_14
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (26) = happyShift action_60
action_146 (27) = happyShift action_61
action_146 (28) = happyShift action_62
action_146 (29) = happyShift action_63
action_146 (30) = happyShift action_64
action_146 (31) = happyShift action_65
action_146 (41) = happyShift action_67
action_146 (52) = happyShift action_72
action_146 (53) = happyShift action_73
action_146 (64) = happyShift action_74
action_146 (65) = happyShift action_75
action_146 (70) = happyShift action_76
action_146 (17) = happyGoto action_87
action_146 (18) = happyGoto action_152
action_146 _ = happyReduce_66

action_147 (45) = happyShift action_92
action_147 (68) = happyShift action_109
action_147 (69) = happyShift action_110
action_147 _ = happyReduce_63

action_148 (26) = happyShift action_60
action_148 (27) = happyShift action_61
action_148 (28) = happyShift action_62
action_148 (29) = happyShift action_63
action_148 (30) = happyShift action_64
action_148 (31) = happyShift action_65
action_148 (41) = happyShift action_67
action_148 (52) = happyShift action_72
action_148 (53) = happyShift action_73
action_148 (64) = happyShift action_74
action_148 (65) = happyShift action_75
action_148 (70) = happyShift action_76
action_148 (17) = happyGoto action_151
action_148 _ = happyFail (happyExpListPerState 148)

action_149 _ = happyReduce_64

action_150 _ = happyReduce_59

action_151 (38) = happyShift action_90
action_151 (45) = happyShift action_92
action_151 (51) = happyShift action_93
action_151 (52) = happyShift action_94
action_151 (53) = happyShift action_95
action_151 (54) = happyShift action_96
action_151 (55) = happyShift action_97
action_151 (56) = happyShift action_98
action_151 (57) = happyShift action_99
action_151 (58) = happyShift action_100
action_151 (59) = happyShift action_101
action_151 (60) = happyShift action_102
action_151 (61) = happyShift action_103
action_151 (62) = happyShift action_104
action_151 (63) = happyShift action_105
action_151 (65) = happyShift action_106
action_151 (66) = happyShift action_107
action_151 (67) = happyShift action_108
action_151 (68) = happyShift action_109
action_151 (69) = happyShift action_110
action_151 _ = happyReduce_80

action_152 (40) = happyShift action_157
action_152 _ = happyFail (happyExpListPerState 152)

action_153 _ = happyReduce_24

action_154 (50) = happyShift action_156
action_154 _ = happyReduce_27

action_155 _ = happyReduce_61

action_156 (26) = happyShift action_60
action_156 (27) = happyShift action_61
action_156 (28) = happyShift action_62
action_156 (29) = happyShift action_63
action_156 (30) = happyShift action_64
action_156 (31) = happyShift action_65
action_156 (32) = happyShift action_15
action_156 (33) = happyShift action_16
action_156 (34) = happyShift action_17
action_156 (35) = happyShift action_18
action_156 (36) = happyShift action_32
action_156 (37) = happyShift action_66
action_156 (41) = happyShift action_67
action_156 (43) = happyShift action_42
action_156 (47) = happyShift action_69
action_156 (48) = happyShift action_70
action_156 (49) = happyShift action_71
action_156 (52) = happyShift action_72
action_156 (53) = happyShift action_73
action_156 (64) = happyShift action_74
action_156 (65) = happyShift action_75
action_156 (70) = happyShift action_76
action_156 (10) = happyGoto action_56
action_156 (14) = happyGoto action_57
action_156 (16) = happyGoto action_159
action_156 (17) = happyGoto action_59
action_156 (19) = happyGoto action_31
action_156 (20) = happyGoto action_13
action_156 (21) = happyGoto action_14
action_156 _ = happyFail (happyExpListPerState 156)

action_157 (26) = happyShift action_60
action_157 (27) = happyShift action_61
action_157 (28) = happyShift action_62
action_157 (29) = happyShift action_63
action_157 (30) = happyShift action_64
action_157 (31) = happyShift action_65
action_157 (41) = happyShift action_67
action_157 (52) = happyShift action_72
action_157 (53) = happyShift action_73
action_157 (64) = happyShift action_74
action_157 (65) = happyShift action_75
action_157 (70) = happyShift action_76
action_157 (17) = happyGoto action_87
action_157 (18) = happyGoto action_158
action_157 _ = happyReduce_66

action_158 (42) = happyShift action_160
action_158 _ = happyFail (happyExpListPerState 158)

action_159 _ = happyReduce_26

action_160 (26) = happyShift action_60
action_160 (27) = happyShift action_61
action_160 (28) = happyShift action_62
action_160 (29) = happyShift action_63
action_160 (30) = happyShift action_64
action_160 (31) = happyShift action_65
action_160 (32) = happyShift action_15
action_160 (33) = happyShift action_16
action_160 (34) = happyShift action_17
action_160 (35) = happyShift action_18
action_160 (36) = happyShift action_32
action_160 (37) = happyShift action_66
action_160 (41) = happyShift action_67
action_160 (43) = happyShift action_42
action_160 (47) = happyShift action_69
action_160 (48) = happyShift action_70
action_160 (49) = happyShift action_71
action_160 (52) = happyShift action_72
action_160 (53) = happyShift action_73
action_160 (64) = happyShift action_74
action_160 (65) = happyShift action_75
action_160 (70) = happyShift action_76
action_160 (10) = happyGoto action_56
action_160 (14) = happyGoto action_57
action_160 (16) = happyGoto action_161
action_160 (17) = happyGoto action_59
action_160 (19) = happyGoto action_31
action_160 (20) = happyGoto action_13
action_160 (21) = happyGoto action_14
action_160 _ = happyFail (happyExpListPerState 160)

action_161 _ = happyReduce_25

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn4
		 (P.Program (reverse happy_var_1) (reverse happy_var_2)
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
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (P.FuncDefn happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (P.StructDecl happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn6
		 (P.VarDecl happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (P.Struct happy_var_2 (reverse happy_var_4) 0
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 5 8 happyReduction_9
happyReduction_9 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_2)) `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (P.Func happy_var_1 happy_var_2 [] 0
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 6 8 happyReduction_10
happyReduction_10 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_2)) `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (P.Func happy_var_1 happy_var_2 (reverse happy_var_4) 0
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 9 happyReduction_11
happyReduction_11 ((HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_2)) `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (P.FuncDef happy_var_1 happy_var_2 [] happy_var_5 0
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 6 9 happyReduction_12
happyReduction_12 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_2)) `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (P.FuncDef happy_var_1 happy_var_2 (reverse happy_var_4) happy_var_6 0
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 10 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyTerminal (L.Ident happy_var_2)) `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (P.Var happy_var_1 happy_var_2 (reverse happy_var_3) 0
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_2  11 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_1
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  12 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_3 : happy_var_1
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  13 happyReduction_18
happyReduction_18 (HappyTerminal (L.Ident happy_var_2))
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn13
		 (P.Formal happy_var_1 happy_var_2 0
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  14 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (P.Block (reverse happy_var_2) 0
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  15 happyReduction_20
happyReduction_20  =  HappyAbsSyn15
		 ([]
	)

happyReduce_21 = happySpecReduce_2  15 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_2 : happy_var_1
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  16 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (P.Expr happy_var_1 0
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  16 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn16
		 (P.BlockStatement happy_var_1 0
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happyReduce 5 16 happyReduction_24
happyReduction_24 ((HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.While happy_var_3 happy_var_5 0
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 9 16 happyReduction_25
happyReduction_25 ((HappyAbsSyn16  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.For happy_var_3 happy_var_5 happy_var_7 happy_var_9 0
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 7 16 happyReduction_26
happyReduction_26 ((HappyAbsSyn16  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.If happy_var_3 happy_var_5 (Just happy_var_7) 0
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 5 16 happyReduction_27
happyReduction_27 ((HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (P.If happy_var_3 happy_var_5 Nothing 0
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (P.Return happy_var_2 0
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  16 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn16
		 (P.VarDeclStatement happy_var_1 0
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyTerminal (L.LitInt happy_var_1))
	 =  HappyAbsSyn17
		 (P.LitInt happy_var_1 0
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 (HappyTerminal (L.LitDouble happy_var_1))
	 =  HappyAbsSyn17
		 (P.LitDouble happy_var_1 0
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 (HappyTerminal (L.LitChar happy_var_1))
	 =  HappyAbsSyn17
		 (P.LitChar happy_var_1 0
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 (HappyTerminal (L.LitString happy_var_1))
	 =  HappyAbsSyn17
		 (P.LitString happy_var_1 0
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  17 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn17
		 (P.Null 0
	)

happyReduce_35 = happySpecReduce_1  17 happyReduction_35
happyReduction_35 (HappyTerminal (L.Ident happy_var_1))
	 =  HappyAbsSyn17
		 (P.Ident happy_var_1 0
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Assign happy_var_1 happy_var_3 0
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  17 happyReduction_37
happyReduction_37 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Or happy_var_3 0
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  17 happyReduction_38
happyReduction_38 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.And happy_var_3 0
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  17 happyReduction_39
happyReduction_39 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.BitwiseOr happy_var_3 0
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  17 happyReduction_40
happyReduction_40 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.BitwiseXor happy_var_3 0
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  17 happyReduction_41
happyReduction_41 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.BitwiseAnd happy_var_3 0
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  17 happyReduction_42
happyReduction_42 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Equal happy_var_3 0
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  17 happyReduction_43
happyReduction_43 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Neq happy_var_3 0
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  17 happyReduction_44
happyReduction_44 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Geq happy_var_3 0
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  17 happyReduction_45
happyReduction_45 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Greater happy_var_3 0
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  17 happyReduction_46
happyReduction_46 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Leq happy_var_3 0
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  17 happyReduction_47
happyReduction_47 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Less happy_var_3 0
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  17 happyReduction_48
happyReduction_48 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Add happy_var_3 0
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  17 happyReduction_49
happyReduction_49 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Sub happy_var_3 0
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  17 happyReduction_50
happyReduction_50 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Mul happy_var_3 0
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  17 happyReduction_51
happyReduction_51 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Div happy_var_3 0
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  17 happyReduction_52
happyReduction_52 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Binop happy_var_1 P.Mod happy_var_3 0
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  17 happyReduction_53
happyReduction_53 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (P.Nested happy_var_2 0
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  17 happyReduction_54
happyReduction_54 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (P.Negate happy_var_2 0
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  17 happyReduction_55
happyReduction_55 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (P.Negative happy_var_2 0
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  17 happyReduction_56
happyReduction_56 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (P.AddressOf happy_var_2 0
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  17 happyReduction_57
happyReduction_57 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (P.Deref happy_var_2 0
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  17 happyReduction_58
happyReduction_58 (HappyTerminal (L.Ident happy_var_3))
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.FieldAccess happy_var_1 happy_var_3 0
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happyReduce 4 17 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (P.ArrayAccess happy_var_1 happy_var_3 0
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_3  17 happyReduction_60
happyReduction_60 (HappyTerminal (L.Ident happy_var_3))
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (P.Indirect happy_var_1 happy_var_3 0
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 5 17 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (P.Sizeof (Left (P.SizeofType happy_var_3 happy_var_4)) 0
	) `HappyStk` happyRest

happyReduce_62 = happyReduce 4 17 happyReduction_62
happyReduction_62 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (P.Sizeof (Right happy_var_3) 0
	) `HappyStk` happyRest

happyReduce_63 = happyReduce 4 17 happyReduction_63
happyReduction_63 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (P.Typecast happy_var_2 happy_var_4 0
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 4 17 happyReduction_64
happyReduction_64 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Ident happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (P.Call happy_var_1 (reverse happy_var_3) 0
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_3  17 happyReduction_65
happyReduction_65 _
	_
	(HappyTerminal (L.Ident happy_var_1))
	 =  HappyAbsSyn17
		 (P.Call happy_var_1 [] 0
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_0  18 happyReduction_66
happyReduction_66  =  HappyAbsSyn18
		 (Nothing
	)

happyReduce_67 = happySpecReduce_1  18 happyReduction_67
happyReduction_67 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (Just happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  19 happyReduction_68
happyReduction_68 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 happy_var_2
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  19 happyReduction_69
happyReduction_69 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 happy_var_2
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  20 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn20
		 (P.PrimitiveType L.Int
	)

happyReduce_71 = happySpecReduce_1  20 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn20
		 (P.PrimitiveType L.Double
	)

happyReduce_72 = happySpecReduce_1  20 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn20
		 (P.PrimitiveType L.Char
	)

happyReduce_73 = happySpecReduce_1  20 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn20
		 (P.PrimitiveType L.Void
	)

happyReduce_74 = happySpecReduce_2  21 happyReduction_74
happyReduction_74 (HappyTerminal (L.Ident happy_var_2))
	_
	 =  HappyAbsSyn21
		 (P.StructType happy_var_2
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_0  22 happyReduction_75
happyReduction_75  =  HappyAbsSyn22
		 (0
	)

happyReduce_76 = happySpecReduce_2  22 happyReduction_76
happyReduction_76 _
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 + 1
	)
happyReduction_76 _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_0  23 happyReduction_77
happyReduction_77  =  HappyAbsSyn23
		 ([]
	)

happyReduce_78 = happyReduce 4 23 happyReduction_78
happyReduction_78 (_ `HappyStk`
	(HappyTerminal (L.LitInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (happy_var_3 : happy_var_1
	) `HappyStk` happyRest

happyReduce_79 = happySpecReduce_1  24 happyReduction_79
happyReduction_79 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  24 happyReduction_80
happyReduction_80 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_3 : happy_var_1
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_0  25 happyReduction_81
happyReduction_81  =  HappyAbsSyn25
		 ([]
	)

happyReduce_82 = happySpecReduce_3  25 happyReduction_82
happyReduction_82 (HappyTerminal (L.LitString happy_var_3))
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 ((P.Include happy_var_3) : happy_var_1
	)
happyReduction_82 _ _ _  = notHappyAtAll 

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
