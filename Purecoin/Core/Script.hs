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
           , OP_CHECKSIG
           , OP_CHECKSIGVERIFY
           , OP_CHECKMULTISIG
           , OP_CHECKMULTISIGVERIFY
           , OP_NOP1, OP_NOP2, OP_NOP3, OP_NOP4, OP_NOP5
           , OP_NOP6, OP_NOP7, OP_NOP8, OP_NOP9, OP_NOP10
           ), opPushData, opView
       , Script, scriptOps, opsScript
       ) where

import Data.List (intercalate)
import Control.Applicative ((<$>),(<*>))
import Purecoin.Core.Serialize
       ( Serialize, Get, Put
       , get, getWord8, getWord16be, getWord32be, getVarByteString, getBytes
       , put, putWord8, putWord16be, putWord32be, putVarByteString, putByteString
       , runGet, runPut
       , isEmpty
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
        | OP_CHECKSIG
        | OP_CHECKSIGVERIFY
        | OP_CHECKMULTISIG
        | OP_CHECKMULTISIGVERIFY
        | OP_NOP1 | OP_NOP2 | OP_NOP3 | OP_NOP4 | OP_NOP5
        | OP_NOP6 | OP_NOP7 | OP_NOP8 | OP_NOP9 | OP_NOP10
        deriving (Eq, Show)

data OPSep = OP OP
           | OP_CODESEPARATOR

getOpSep :: Get OPSep
getOpSep = code =<< getWord8
 where
  -- TODO: make WS.getBytes
  code x | x <= 75 = getBytes (fromIntegral x) >>= return . OP . OP_PUSHDATA . WS.fromByteString
  code 76 = do l <- getWord8
               d <- getBytes (fromIntegral l)
               return . OP . OP_PUSHDATA1 . WS.fromByteString $ d
  code 77 = do l <- getWord16be
               d <- getBytes (fromIntegral l)
               return . OP . OP_PUSHDATA2 . WS.fromByteString $ d
  code 78 = do l <- getWord32be
               d <- getBytes (fromIntegral l)
               return . OP . OP_PUSHDATA4 . WS.fromByteString $ d
  code 81 = return $ OP OP_1
  code 82 = return $ OP OP_2
  code 83 = return $ OP OP_3
  code 84 = return $ OP OP_4
  code 85 = return $ OP OP_5
  code 86 = return $ OP OP_6
  code 87 = return $ OP OP_7
  code 88 = return $ OP OP_8
  code 89 = return $ OP OP_9
  code 90 = return $ OP OP_10
  code 91 = return $ OP OP_11
  code 92 = return $ OP OP_12
  code 93 = return $ OP OP_13
  code 94 = return $ OP OP_14
  code 95 = return $ OP OP_15
  code 96 = return $ OP OP_16
  code 97 = return $ OP OP_NOP
  code 99 = return $ OP OP_IF
  code 100 = return $ OP OP_NOTIF
  code 103 = return $ OP OP_ELSE
  code 104 = return $ OP OP_ENDIF
  code 105 = return $ OP OP_VERIFY
  code 106 = return $ OP OP_RETURN
  code 107 = return $ OP OP_TOALTSTACK
  code 108 = return $ OP OP_FROMALTSTACK
  code 115 = return $ OP OP_IFDUP
  code 116 = return $ OP OP_DEPTH
  code 117 = return $ OP OP_DROP
  code 118 = return $ OP OP_DUP
  code 119 = return $ OP OP_NIP
  code 120 = return $ OP OP_OVER
  code 121 = return $ OP OP_PICK
  code 122 = return $ OP OP_ROLL
  code 123 = return $ OP OP_ROT
  code 124 = return $ OP OP_SWAP
  code 125 = return $ OP OP_TUCK
  code 109 = return $ OP OP_2DROP
  code 110 = return $ OP OP_2DUP
  code 111 = return $ OP OP_3DUP
  code 112 = return $ OP OP_2OVER
  code 113 = return $ OP OP_2ROT
  code 114 = return $ OP OP_2SWAP
  code 126 = return $ OP OP_CAT
  code 127 = return $ OP OP_SUBSTR
  code 128 = return $ OP OP_LEFT
  code 129 = return $ OP OP_RIGHT
  code 130 = return $ OP OP_SIZE
  code 131 = return $ OP OP_INVERT
  code 132 = return $ OP OP_AND
  code 133 = return $ OP OP_OR
  code 134 = return $ OP OP_XOR
  code 135 = return $ OP OP_EQUAL
  code 136 = return $ OP OP_EQUALVERIFY
  code 139 = return $ OP OP_1ADD
  code 140 = return $ OP OP_1SUB
  code 141 = return $ OP OP_2MUL
  code 142 = return $ OP OP_2DIV
  code 143 = return $ OP OP_NEGATE
  code 144 = return $ OP OP_ABS
  code 145 = return $ OP OP_NOT
  code 146 = return $ OP OP_0NOTEQUAL
  code 147 = return $ OP OP_ADD
  code 148 = return $ OP OP_SUB
  code 149 = return $ OP OP_MUL
  code 150 = return $ OP OP_DIV
  code 151 = return $ OP OP_MOD
  code 152 = return $ OP OP_LSHIFT
  code 153 = return $ OP OP_RSHIFT
  code 154 = return $ OP OP_BOOLAND
  code 155 = return $ OP OP_BOOLOR
  code 156 = return $ OP OP_NUMEQUAL
  code 157 = return $ OP OP_NUMEQUALVERIFY
  code 158 = return $ OP OP_NUMNOTEQUAL
  code 159 = return $ OP OP_LESSTHAN
  code 160 = return $ OP OP_GREATERTHAN
  code 161 = return $ OP OP_LESSTHANOREQUAL
  code 162 = return $ OP OP_GREATERTHANOREQUAL
  code 163 = return $ OP OP_MIN
  code 164 = return $ OP OP_MAX
  code 165 = return $ OP OP_WITHIN
  code 166 = return $ OP OP_RIPEMD160
  code 167 = return $ OP OP_SHA1
  code 168 = return $ OP OP_SHA256
  code 169 = return $ OP OP_HASH160
  code 170 = return $ OP OP_HASH256
  code 171 = return OP_CODESEPARATOR
  code 172 = return $ OP OP_CHECKSIG
  code 173 = return $ OP OP_CHECKSIGVERIFY
  code 174 = return $ OP OP_CHECKMULTISIG
  code 175 = return $ OP OP_CHECKMULTISIGVERIFY
  code 176 = return $ OP OP_NOP1
  code 177 = return $ OP OP_NOP2
  code 178 = return $ OP OP_NOP3
  code 179 = return $ OP OP_NOP4
  code 180 = return $ OP OP_NOP5
  code 181 = return $ OP OP_NOP6
  code 182 = return $ OP OP_NOP7
  code 183 = return $ OP OP_NOP8
  code 184 = return $ OP OP_NOP9
  code 185 = return $ OP OP_NOP10
  code x = fail $ "Unknown OP code: " ++ show x

putOpSep :: OPSep -> Put
putOpSep (OP (OP_PUSHDATA x))  | WS.length x <= 75 =
    putWord8 (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
putOpSep (OP (OP_PUSHDATA1 x)) | WS.length x < 2^8 =
    putWord8 76 >> putWord8 (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
putOpSep (OP (OP_PUSHDATA2 x)) | WS.length x < 2^16 =
    putWord8 77 >> putWord16be (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
putOpSep (OP (OP_PUSHDATA4 x)) | WS.length x < 2^32 =
    putWord8 78 >> putWord32be (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
putOpSep (OP (OP_1)) = putWord8 81
putOpSep (OP (OP_2)) = putWord8 82
putOpSep (OP (OP_3)) = putWord8 83
putOpSep (OP (OP_4)) = putWord8 84
putOpSep (OP (OP_5)) = putWord8 85
putOpSep (OP (OP_6)) = putWord8 86
putOpSep (OP (OP_7)) = putWord8 87
putOpSep (OP (OP_8)) = putWord8 88
putOpSep (OP (OP_9)) = putWord8 89
putOpSep (OP (OP_10)) = putWord8 90
putOpSep (OP (OP_11)) = putWord8 91
putOpSep (OP (OP_12)) = putWord8 92
putOpSep (OP (OP_13)) = putWord8 93
putOpSep (OP (OP_14)) = putWord8 94
putOpSep (OP (OP_15)) = putWord8 95
putOpSep (OP (OP_16)) = putWord8 96
putOpSep (OP (OP_NOP)) = putWord8 97
putOpSep (OP (OP_IF)) = putWord8 99
putOpSep (OP (OP_NOTIF)) = putWord8 100
putOpSep (OP (OP_ELSE)) = putWord8 103
putOpSep (OP (OP_ENDIF)) = putWord8 104
putOpSep (OP (OP_VERIFY)) = putWord8 105
putOpSep (OP (OP_RETURN)) = putWord8 106
putOpSep (OP (OP_TOALTSTACK)) = putWord8 107
putOpSep (OP (OP_FROMALTSTACK)) = putWord8 108
putOpSep (OP (OP_IFDUP)) = putWord8 115
putOpSep (OP (OP_DEPTH)) = putWord8 116
putOpSep (OP (OP_DROP)) = putWord8 117
putOpSep (OP (OP_DUP)) = putWord8 118
putOpSep (OP (OP_NIP)) = putWord8 119
putOpSep (OP (OP_OVER)) = putWord8 120
putOpSep (OP (OP_PICK)) = putWord8 121
putOpSep (OP (OP_ROLL)) = putWord8 122
putOpSep (OP (OP_ROT)) = putWord8 123
putOpSep (OP (OP_SWAP)) = putWord8 124
putOpSep (OP (OP_TUCK)) = putWord8 125
putOpSep (OP (OP_2DROP)) = putWord8 109
putOpSep (OP (OP_2DUP)) = putWord8 110
putOpSep (OP (OP_3DUP)) = putWord8 111
putOpSep (OP (OP_2OVER)) = putWord8 112
putOpSep (OP (OP_2ROT)) = putWord8 113
putOpSep (OP (OP_2SWAP)) = putWord8 114
putOpSep (OP (OP_CAT)) = putWord8 126
putOpSep (OP (OP_SUBSTR)) = putWord8 127
putOpSep (OP (OP_LEFT)) = putWord8 128
putOpSep (OP (OP_RIGHT)) = putWord8 129
putOpSep (OP (OP_SIZE)) = putWord8 130
putOpSep (OP (OP_INVERT)) = putWord8 131
putOpSep (OP (OP_AND)) = putWord8 132
putOpSep (OP (OP_OR)) = putWord8 133
putOpSep (OP (OP_XOR)) = putWord8 134
putOpSep (OP (OP_EQUAL)) = putWord8 135
putOpSep (OP (OP_EQUALVERIFY)) = putWord8 136
putOpSep (OP (OP_1ADD)) = putWord8 139
putOpSep (OP (OP_1SUB)) = putWord8 140
putOpSep (OP (OP_2MUL)) = putWord8 141
putOpSep (OP (OP_2DIV)) = putWord8 142
putOpSep (OP (OP_NEGATE)) = putWord8 143
putOpSep (OP (OP_ABS)) = putWord8 144
putOpSep (OP (OP_NOT)) = putWord8 145
putOpSep (OP (OP_0NOTEQUAL)) = putWord8 146
putOpSep (OP (OP_ADD)) = putWord8 147
putOpSep (OP (OP_SUB)) = putWord8 148
putOpSep (OP (OP_MUL)) = putWord8 149
putOpSep (OP (OP_DIV)) = putWord8 150
putOpSep (OP (OP_MOD)) = putWord8 151
putOpSep (OP (OP_LSHIFT)) = putWord8 152
putOpSep (OP (OP_RSHIFT)) = putWord8 153
putOpSep (OP (OP_BOOLAND)) = putWord8 154
putOpSep (OP (OP_BOOLOR)) = putWord8 155
putOpSep (OP (OP_NUMEQUAL)) = putWord8 156
putOpSep (OP (OP_NUMEQUALVERIFY)) = putWord8 157
putOpSep (OP (OP_NUMNOTEQUAL)) = putWord8 158
putOpSep (OP (OP_LESSTHAN)) = putWord8 159
putOpSep (OP (OP_GREATERTHAN)) = putWord8 160
putOpSep (OP (OP_LESSTHANOREQUAL)) = putWord8 161
putOpSep (OP (OP_GREATERTHANOREQUAL)) = putWord8 162
putOpSep (OP (OP_MIN)) = putWord8 163
putOpSep (OP (OP_MAX)) = putWord8 164
putOpSep (OP (OP_WITHIN)) = putWord8 165
putOpSep (OP (OP_RIPEMD160)) = putWord8 166
putOpSep (OP (OP_SHA1)) = putWord8 167
putOpSep (OP (OP_SHA256)) = putWord8 168
putOpSep (OP (OP_HASH160)) = putWord8 169
putOpSep (OP (OP_HASH256)) = putWord8 170
putOpSep OP_CODESEPARATOR = putWord8 171
putOpSep (OP (OP_CHECKSIG)) = putWord8 172
putOpSep (OP (OP_CHECKSIGVERIFY)) = putWord8 173
putOpSep (OP (OP_CHECKMULTISIG)) = putWord8 174
putOpSep (OP (OP_CHECKMULTISIGVERIFY)) = putWord8 175
putOpSep (OP (OP_NOP1)) = putWord8 176
putOpSep (OP (OP_NOP2)) = putWord8 177
putOpSep (OP (OP_NOP3)) = putWord8 178
putOpSep (OP (OP_NOP4)) = putWord8 179
putOpSep (OP (OP_NOP5)) = putWord8 180
putOpSep (OP (OP_NOP6)) = putWord8 181
putOpSep (OP (OP_NOP7)) = putWord8 182
putOpSep (OP (OP_NOP8)) = putWord8 183
putOpSep (OP (OP_NOP9)) = putWord8 184
putOpSep (OP (OP_NOP10)) = putWord8 185

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

newtype Script = Script WS.Word8s deriving (Eq, Show)

-- TODO: I probably want to replace getVarByteString with getVarWord8s
instance Serialize Script where
  get = Script . WS.fromByteString <$> getVarByteString

  put (Script ws) = putVarByteString . WS.toByteString $ ws

-- TODO make this a non-empty list of lists
scriptOps :: Script -> Either String [[OP]]
scriptOps (Script ws) = runGet getOps . WS.toByteString $ ws
 where
  getOps = do empty <- isEmpty
              if empty then return $ [[]]
                       else push <$> getOpSep <*> getOps
  push OP_CODESEPARATOR  s     = []:s
  push (OP o)            (h:s) = (o:h):s


opsScript :: [[OP]] -> Script
opsScript [] = error "opsScript: Script must be non-empty"
opsScript s = Script . WS.fromByteString . runPut . mapM_ putOpSep . intercalate [OP_CODESEPARATOR] $ map (map OP) s