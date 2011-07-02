module Purecoin.Core.Script
       ( OP( OP_1NEGATE
           , OP_1, OP_2, OP_3, OP_4, OP_5, OP_6, OP_7, OP_8
           , OP_9, OP_10, OP_11, OP_12, OP_13, OP_14, OP_15, OP_16
           , OP_NOP
           , OP_IF
           , OP_NOTIF
           , OP_ELSE
           , OP_ENDIF
           , OP_VERIFY
           , OP_RETURN
           , OP_TOALTSTACK
           , OP_FROMALTSTACK
           , OP_IFDUP
           , OP_DEPTH
           , OP_DROP
           , OP_DUP
           , OP_NIP
           , OP_OVER
           , OP_PICK
           , OP_ROLL
           , OP_ROT
           , OP_SWAP
           , OP_TUCK
           , OP_2DROP
           , OP_2DUP
           , OP_3DUP
           , OP_2OVER
           , OP_2ROT
           , OP_2SWAP
           , OP_CAT
           , OP_SUBSTR
           , OP_LEFT
           , OP_RIGHT
           , OP_SIZE
           , OP_INVERT
           , OP_AND
           , OP_OR
           , OP_XOR
           , OP_EQUAL
           , OP_EQUALVERIFY
           , OP_1ADD
           , OP_1SUB
           , OP_2MUL
           , OP_2DIV
           , OP_NEGATE
           , OP_ABS
           , OP_NOT
           , OP_0NOTEQUAL
           , OP_ADD
           , OP_SUB
           , OP_MUL
           , OP_DIV
           , OP_MOD
           , OP_LSHIFT
           , OP_RSHIFT
           , OP_BOOLAND
           , OP_BOOLOR
           , OP_NUMEQUAL
           , OP_NUMEQUALVERIFY
           , OP_NUMNOTEQUAL
           , OP_LESSTHAN
           , OP_GREATERTHAN
           , OP_LESSTHANOREQUAL
           , OP_GREATERTHANOREQUAL
           , OP_MIN
           , OP_MAX
           , OP_WITHIN
           , OP_RIPEMD160
           , OP_SHA1
           , OP_SHA256
           , OP_HASH160
           , OP_HASH256
           , OP_CODESEPARATOR
           , OP_CHECKSIG
           , OP_CHECKSIGVERIFY
           , OP_CHECKMULTISIG
           , OP_CHECKMULTISIGVERIFY
           , OP_NOP1, OP_NOP2, OP_NOP3, OP_NOP4, OP_NOP5
           , OP_NOP6, OP_NOP7, OP_NOP8, OP_NOP9, OP_NOP10
           ), opPushData, opView
       ) where

import Data.Serialize ( Serialize
                      , get, getWord8, getWord16be, getWord32be, getBytes
                      , put, putWord8, putWord16be, putWord32be, putByteString
                      )
import qualified Purecoin.WordArray as WS

{- Normally one would merge all the OP_PUSHDATA into one consturctor; howenver
   OP_PUSHDATA x and OP_PUSHDATA1 x, etc all serial to different values, and these
   different values can affect the hashes produced; so we must keep all four
   constructors around.
   
   For saftety these four OP_PUSHDATA constructors are not exported.
   Use opPushData to construct constants.
   Use opView to deconstruct these constants.
-}
data OP = OP_PUSHDATA !WS.Word8s  -- length must be less than 76
        | OP_PUSHDATA1 !WS.Word8s -- length must be less than 2^8
        | OP_PUSHDATA2 !WS.Word8s -- length must be less than 2^16
        | OP_PUSHDATA4 !WS.Word8s -- length must be less than 2^32
        | OP_1NEGATE
        | OP_1 | OP_2 | OP_3 | OP_4 | OP_5 | OP_6 | OP_7 | OP_8
        | OP_9 | OP_10 | OP_11 | OP_12 | OP_13 | OP_14 | OP_15 | OP_16
        | OP_NOP
        | OP_IF
        | OP_NOTIF
        | OP_ELSE
        | OP_ENDIF
        | OP_VERIFY
        | OP_RETURN
        | OP_TOALTSTACK
        | OP_FROMALTSTACK
        | OP_IFDUP
        | OP_DEPTH
        | OP_DROP
        | OP_DUP
        | OP_NIP
        | OP_OVER
        | OP_PICK
        | OP_ROLL
        | OP_ROT
        | OP_SWAP
        | OP_TUCK
        | OP_2DROP
        | OP_2DUP
        | OP_3DUP
        | OP_2OVER
        | OP_2ROT
        | OP_2SWAP
        | OP_CAT
        | OP_SUBSTR
        | OP_LEFT
        | OP_RIGHT
        | OP_SIZE
        | OP_INVERT
        | OP_AND
        | OP_OR
        | OP_XOR
        | OP_EQUAL
        | OP_EQUALVERIFY
        | OP_1ADD
        | OP_1SUB
        | OP_2MUL
        | OP_2DIV
        | OP_NEGATE
        | OP_ABS
        | OP_NOT
        | OP_0NOTEQUAL
        | OP_ADD
        | OP_SUB
        | OP_MUL
        | OP_DIV
        | OP_MOD
        | OP_LSHIFT
        | OP_RSHIFT
        | OP_BOOLAND
        | OP_BOOLOR
        | OP_NUMEQUAL
        | OP_NUMEQUALVERIFY
        | OP_NUMNOTEQUAL
        | OP_LESSTHAN
        | OP_GREATERTHAN
        | OP_LESSTHANOREQUAL
        | OP_GREATERTHANOREQUAL
        | OP_MIN
        | OP_MAX
        | OP_WITHIN
        | OP_RIPEMD160
        | OP_SHA1
        | OP_SHA256
        | OP_HASH160
        | OP_HASH256
        | OP_CODESEPARATOR
        | OP_CHECKSIG
        | OP_CHECKSIGVERIFY
        | OP_CHECKMULTISIG
        | OP_CHECKMULTISIGVERIFY
        | OP_NOP1 | OP_NOP2 | OP_NOP3 | OP_NOP4 | OP_NOP5
        | OP_NOP6 | OP_NOP7 | OP_NOP8 | OP_NOP9 | OP_NOP10
        deriving (Eq, Show)

instance Serialize OP where
  get = code =<< getWord8
   where
    code x | x <= 75 = getBytes (fromIntegral x) >>= return . OP_PUSHDATA . WS.fromByteString
    code 76 = do l <- getWord8
                 d <- getBytes (fromIntegral l)
                 return $ OP_PUSHDATA1 (WS.fromByteString d)
    code 77 = do l <- getWord16be
                 d <- getBytes (fromIntegral l)
                 return $ OP_PUSHDATA2 (WS.fromByteString d)
    code 78 = do l <- getWord32be
                 d <- getBytes (fromIntegral l)
                 return $ OP_PUSHDATA4 (WS.fromByteString d)
    code 81 = return OP_1
    code 82 = return OP_2
    code 83 = return OP_3
    code 84 = return OP_4
    code 85 = return OP_5
    code 86 = return OP_6
    code 87 = return OP_7
    code 88 = return OP_8
    code 89 = return OP_9
    code 90 = return OP_10
    code 91 = return OP_11
    code 92 = return OP_12
    code 93 = return OP_13
    code 94 = return OP_14
    code 95 = return OP_15
    code 96 = return OP_16
    code 97 = return OP_NOP
    code 99 = return OP_IF
    code 100 = return OP_NOTIF
    code 103 = return OP_ELSE
    code 104 = return OP_ENDIF
    code 105 = return OP_VERIFY
    code 106 = return OP_RETURN
    code 107 = return OP_TOALTSTACK
    code 108 = return OP_FROMALTSTACK
    code 115 = return OP_IFDUP
    code 116 = return OP_DEPTH
    code 117 = return OP_DROP
    code 118 = return OP_DUP
    code 119 = return OP_NIP
    code 120 = return OP_OVER
    code 121 = return OP_PICK
    code 122 = return OP_ROLL
    code 123 = return OP_ROT
    code 124 = return OP_SWAP
    code 125 = return OP_TUCK
    code 109 = return OP_2DROP
    code 110 = return OP_2DUP
    code 111 = return OP_3DUP
    code 112 = return OP_2OVER
    code 113 = return OP_2ROT
    code 114 = return OP_2SWAP
    code 126 = return OP_CAT
    code 127 = return OP_SUBSTR
    code 128 = return OP_LEFT
    code 129 = return OP_RIGHT
    code 130 = return OP_SIZE
    code 131 = return OP_INVERT
    code 132 = return OP_AND
    code 133 = return OP_OR
    code 134 = return OP_XOR
    code 135 = return OP_EQUAL
    code 136 = return OP_EQUALVERIFY
    code 139 = return OP_1ADD
    code 140 = return OP_1SUB
    code 141 = return OP_2MUL
    code 142 = return OP_2DIV
    code 143 = return OP_NEGATE
    code 144 = return OP_ABS
    code 145 = return OP_NOT
    code 146 = return OP_0NOTEQUAL
    code 147 = return OP_ADD
    code 148 = return OP_SUB
    code 149 = return OP_MUL
    code 150 = return OP_DIV
    code 151 = return OP_MOD
    code 152 = return OP_LSHIFT
    code 153 = return OP_RSHIFT
    code 154 = return OP_BOOLAND
    code 155 = return OP_BOOLOR
    code 156 = return OP_NUMEQUAL
    code 157 = return OP_NUMEQUALVERIFY
    code 158 = return OP_NUMNOTEQUAL
    code 159 = return OP_LESSTHAN
    code 160 = return OP_GREATERTHAN
    code 161 = return OP_LESSTHANOREQUAL
    code 162 = return OP_GREATERTHANOREQUAL
    code 163 = return OP_MIN
    code 164 = return OP_MAX
    code 165 = return OP_WITHIN
    code 166 = return OP_RIPEMD160
    code 167 = return OP_SHA1
    code 168 = return OP_SHA256
    code 169 = return OP_HASH160
    code 170 = return OP_HASH256
    code 171 = return OP_CODESEPARATOR
    code 172 = return OP_CHECKSIG
    code 173 = return OP_CHECKSIGVERIFY
    code 174 = return OP_CHECKMULTISIG
    code 175 = return OP_CHECKMULTISIGVERIFY
    code 176 = return OP_NOP1
    code 177 = return OP_NOP2
    code 178 = return OP_NOP3
    code 179 = return OP_NOP4
    code 180 = return OP_NOP5
    code 181 = return OP_NOP6
    code 182 = return OP_NOP7
    code 183 = return OP_NOP8
    code 184 = return OP_NOP9
    code 185 = return OP_NOP10
    code x = fail $ "Unknown OP code: " ++ show x

  put (OP_PUSHDATA x)  | WS.length x <= 75 =
    putWord8 (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
  put (OP_PUSHDATA1 x) | WS.length x < 2^8 =
    putWord8 76 >> putWord8 (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
  put (OP_PUSHDATA2 x) | WS.length x < 2^16 =
    putWord8 77 >> putWord16be (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
  put (OP_PUSHDATA4 x) | WS.length x < 2^32 =
    putWord8 78 >> putWord32be (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
  put OP_1 = putWord8 81
  put OP_2 = putWord8 82
  put OP_3 = putWord8 83
  put OP_4 = putWord8 84
  put OP_5 = putWord8 85
  put OP_6 = putWord8 86
  put OP_7 = putWord8 87
  put OP_8 = putWord8 88
  put OP_9 = putWord8 89
  put OP_10 = putWord8 90
  put OP_11 = putWord8 91
  put OP_12 = putWord8 92
  put OP_13 = putWord8 93
  put OP_14 = putWord8 94
  put OP_15 = putWord8 95
  put OP_16 = putWord8 96
  put OP_NOP = putWord8 97
  put OP_IF = putWord8 99
  put OP_NOTIF = putWord8 100
  put OP_ELSE = putWord8 103
  put OP_ENDIF = putWord8 104
  put OP_VERIFY = putWord8 105
  put OP_RETURN = putWord8 106
  put OP_TOALTSTACK = putWord8 107
  put OP_FROMALTSTACK = putWord8 108
  put OP_IFDUP = putWord8 115
  put OP_DEPTH = putWord8 116
  put OP_DROP = putWord8 117
  put OP_DUP = putWord8 118
  put OP_NIP = putWord8 119
  put OP_OVER = putWord8 120
  put OP_PICK = putWord8 121
  put OP_ROLL = putWord8 122
  put OP_ROT = putWord8 123
  put OP_SWAP = putWord8 124
  put OP_TUCK = putWord8 125
  put OP_2DROP = putWord8 109
  put OP_2DUP = putWord8 110
  put OP_3DUP = putWord8 111
  put OP_2OVER = putWord8 112
  put OP_2ROT = putWord8 113
  put OP_2SWAP = putWord8 114
  put OP_CAT = putWord8 126
  put OP_SUBSTR = putWord8 127
  put OP_LEFT = putWord8 128
  put OP_RIGHT = putWord8 129
  put OP_SIZE = putWord8 130
  put OP_INVERT = putWord8 131
  put OP_AND = putWord8 132
  put OP_OR = putWord8 133
  put OP_XOR = putWord8 134
  put OP_EQUAL = putWord8 135
  put OP_EQUALVERIFY = putWord8 136
  put OP_1ADD = putWord8 139
  put OP_1SUB = putWord8 140
  put OP_2MUL = putWord8 141
  put OP_2DIV = putWord8 142
  put OP_NEGATE = putWord8 143
  put OP_ABS = putWord8 144
  put OP_NOT = putWord8 145
  put OP_0NOTEQUAL = putWord8 146
  put OP_ADD = putWord8 147
  put OP_SUB = putWord8 148
  put OP_MUL = putWord8 149
  put OP_DIV = putWord8 150
  put OP_MOD = putWord8 151
  put OP_LSHIFT = putWord8 152
  put OP_RSHIFT = putWord8 153
  put OP_BOOLAND = putWord8 154
  put OP_BOOLOR = putWord8 155
  put OP_NUMEQUAL = putWord8 156
  put OP_NUMEQUALVERIFY = putWord8 157
  put OP_NUMNOTEQUAL = putWord8 158
  put OP_LESSTHAN = putWord8 159
  put OP_GREATERTHAN = putWord8 160
  put OP_LESSTHANOREQUAL = putWord8 161
  put OP_GREATERTHANOREQUAL = putWord8 162
  put OP_MIN = putWord8 163
  put OP_MAX = putWord8 164
  put OP_WITHIN = putWord8 165
  put OP_RIPEMD160 = putWord8 166
  put OP_SHA1 = putWord8 167
  put OP_SHA256 = putWord8 168
  put OP_HASH160 = putWord8 169
  put OP_HASH256 = putWord8 170
  put OP_CODESEPARATOR = putWord8 171
  put OP_CHECKSIG = putWord8 172
  put OP_CHECKSIGVERIFY = putWord8 173
  put OP_CHECKMULTISIG = putWord8 174
  put OP_CHECKMULTISIGVERIFY = putWord8 175
  put OP_NOP1 = putWord8 176
  put OP_NOP2 = putWord8 177
  put OP_NOP3 = putWord8 178
  put OP_NOP4 = putWord8 179
  put OP_NOP5 = putWord8 180
  put OP_NOP6 = putWord8 181
  put OP_NOP7 = putWord8 182
  put OP_NOP8 = putWord8 183
  put OP_NOP9 = putWord8 184
  put OP_NOP10 = putWord8 185

opPushData :: WS.Word8s -> OP
opPushData ws | len <= 75  = OP_PUSHDATA ws
              | len < 2^8  = OP_PUSHDATA1 ws
              | len < 2^16 = OP_PUSHDATA2 ws
              | len < 2^32 = OP_PUSHDATA4 ws
              | otherwise  = error "op_PushData: Too Large"
 where
  len = WS.length ws

opView :: OP -> Either WS.Word8s OP
opView (OP_PUSHDATA ws)  = Left ws
opView (OP_PUSHDATA1 ws) = Left ws
opView (OP_PUSHDATA2 ws) = Left ws
opView (OP_PUSHDATA4 ws) = Left ws
opView x                 = Right x