module Purecoin.Core.Script
       ( OP( OP_1NEGATE
           , OP_1, OP_2, OP_3, OP_4, OP_5, OP_6, OP_7, OP_8
           , OP_9, OP_10, OP_11, OP_12, OP_13, OP_14, OP_15, OP_16
           , OP_NOP
           , OP_IF
           , OP_NOTIF
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
       , Script, scriptOps, opsScript, nullScript
       , MakeHash, ScriptMonad, doScript, execScriptMonad
       ) where

import Prelude hiding (length)
import Data.List (intersperse, intercalate, tails, genericLength, genericSplitAt)
import Data.NEList (NEList(..), toList, headTailView, (<|))
import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Monad (MonadPlus, guard, when)
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.State as MS
import Data.ByteString (ByteString, length)
import Purecoin.Core.Serialize
       ( Serialize, Get, Put
       , get, getWord8, getWord16be, getWord32be, getVarByteString, getBytes
       , put, putWord8, putWord16be, putWord32be, putVarByteString, putByteString
       , runGet, runPut, encode, decode
       , isEmpty
       )
import qualified Purecoin.WordArray as WS
import Purecoin.Core.Hash (hashBS, hash160BS, ripemd160BS, sha256BS)
import Purecoin.Core.Signature (CoinSignature, csSig)
import Purecoin.Crypto.EcDsaSecp256k1 (verifySignature)
import Purecoin.Utils (integerByteStringLE, integerToByteStringLE)

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
        | OP_IF (NEList [OP])
        | OP_NOTIF (NEList [OP])
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

data IfTerminator = OP_ELSE | OP_ENDIF

getIfBlock :: Get (NEList [OP])
getIfBlock = go =<< getOp
 where
  go (Left OP_ENDIF) = return (NENil [])
  go (Left OP_ELSE) = do rest <- getIfBlock
                         return (NECons [] rest)
  go (Right op) = do (hd,tl) <- headTailView <$> getIfBlock
                     return ((op:hd) <| tl)

getOp :: Get (Either IfTerminator OP)
getOp = code =<< getWord8
 where
  -- TODO: make WS.getBytes
  code x | x <= 75 = getBytes (fromIntegral x) >>= return . Right . OP_PUSHDATA . WS.fromByteString
  code 76 = do l <- getWord8
               d <- getBytes (fromIntegral l)
               return . Right . OP_PUSHDATA1 . WS.fromByteString $ d
  code 77 = do l <- getWord16be
               d <- getBytes (fromIntegral l)
               return . Right . OP_PUSHDATA2 . WS.fromByteString $ d
  code 78 = do l <- getWord32be
               d <- getBytes (fromIntegral l)
               return . Right . OP_PUSHDATA4 . WS.fromByteString $ d
  code 79 = return (Right OP_1NEGATE)
  code 81 = return (Right OP_1)
  code 82 = return (Right OP_2)
  code 83 = return (Right OP_3)
  code 84 = return (Right OP_4)
  code 85 = return (Right OP_5)
  code 86 = return (Right OP_6)
  code 87 = return (Right OP_7)
  code 88 = return (Right OP_8)
  code 89 = return (Right OP_9)
  code 90 = return (Right OP_10)
  code 91 = return (Right OP_11)
  code 92 = return (Right OP_12)
  code 93 = return (Right OP_13)
  code 94 = return (Right OP_14)
  code 95 = return (Right OP_15)
  code 96 = return (Right OP_16)
  code 97 = return (Right OP_NOP)
  code 99 = do ifBlock <- getIfBlock
               return (Right (OP_IF ifBlock))
  code 100 = do ifBlock <- getIfBlock
                return (Right (OP_NOTIF ifBlock))
  code 103 = return (Left OP_ELSE)
  code 104 = return (Left OP_ENDIF)
  code 105 = return (Right OP_VERIFY)
  code 106 = return (Right OP_RETURN)
  code 107 = return (Right OP_TOALTSTACK)
  code 108 = return (Right OP_FROMALTSTACK)
  code 115 = return (Right OP_IFDUP)
  code 116 = return (Right OP_DEPTH)
  code 117 = return (Right OP_DROP)
  code 118 = return (Right OP_DUP)
  code 119 = return (Right OP_NIP)
  code 120 = return (Right OP_OVER)
  code 121 = return (Right OP_PICK)
  code 122 = return (Right OP_ROLL)
  code 123 = return (Right OP_ROT)
  code 124 = return (Right OP_SWAP)
  code 125 = return (Right OP_TUCK)
  code 109 = return (Right OP_2DROP)
  code 110 = return (Right OP_2DUP)
  code 111 = return (Right OP_3DUP)
  code 112 = return (Right OP_2OVER)
  code 113 = return (Right OP_2ROT)
  code 114 = return (Right OP_2SWAP)
  code 126 = return (Right OP_CAT)
  code 127 = return (Right OP_SUBSTR)
  code 128 = return (Right OP_LEFT)
  code 129 = return (Right OP_RIGHT)
  code 130 = return (Right OP_SIZE)
  code 131 = return (Right OP_INVERT)
  code 132 = return (Right OP_AND)
  code 133 = return (Right OP_OR)
  code 134 = return (Right OP_XOR)
  code 135 = return (Right OP_EQUAL)
  code 136 = return (Right OP_EQUALVERIFY)
  code 139 = return (Right OP_1ADD)
  code 140 = return (Right OP_1SUB)
  code 141 = return (Right OP_2MUL)
  code 142 = return (Right OP_2DIV)
  code 143 = return (Right OP_NEGATE)
  code 144 = return (Right OP_ABS)
  code 145 = return (Right OP_NOT)
  code 146 = return (Right OP_0NOTEQUAL)
  code 147 = return (Right OP_ADD)
  code 148 = return (Right OP_SUB)
  code 149 = return (Right OP_MUL)
  code 150 = return (Right OP_DIV)
  code 151 = return (Right OP_MOD)
  code 152 = return (Right OP_LSHIFT)
  code 153 = return (Right OP_RSHIFT)
  code 154 = return (Right OP_BOOLAND)
  code 155 = return (Right OP_BOOLOR)
  code 156 = return (Right OP_NUMEQUAL)
  code 157 = return (Right OP_NUMEQUALVERIFY)
  code 158 = return (Right OP_NUMNOTEQUAL)
  code 159 = return (Right OP_LESSTHAN)
  code 160 = return (Right OP_GREATERTHAN)
  code 161 = return (Right OP_LESSTHANOREQUAL)
  code 162 = return (Right OP_GREATERTHANOREQUAL)
  code 163 = return (Right OP_MIN)
  code 164 = return (Right OP_MAX)
  code 165 = return (Right OP_WITHIN)
  code 166 = return (Right OP_RIPEMD160)
  code 167 = return (Right OP_SHA1)
  code 168 = return (Right OP_SHA256)
  code 169 = return (Right OP_HASH160)
  code 170 = return (Right OP_HASH256)
  code 171 = return (Right OP_CODESEPARATOR)
  code 172 = return (Right OP_CHECKSIG)
  code 173 = return (Right OP_CHECKSIGVERIFY)
  code 174 = return (Right OP_CHECKMULTISIG)
  code 175 = return (Right OP_CHECKMULTISIGVERIFY)
  code 176 = return (Right OP_NOP1)
  code 177 = return (Right OP_NOP2)
  code 178 = return (Right OP_NOP3)
  code 179 = return (Right OP_NOP4)
  code 180 = return (Right OP_NOP5)
  code 181 = return (Right OP_NOP6)
  code 182 = return (Right OP_NOP7)
  code 183 = return (Right OP_NOP8)
  code 184 = return (Right OP_NOP9)
  code 185 = return (Right OP_NOP10)
  code x = fail $ "Unknown OP code: " ++ show x

putIfBlock :: NEList [OP] -> Put
putIfBlock blk = sequence_ (intersperse (putWord8 103) (map (mapM_ putOp) (toList blk))) >> putWord8 104

putOp :: OP -> Put
putOp (OP_PUSHDATA x)  | WS.length x <= 75 =
    putWord8 (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
putOp (OP_PUSHDATA1 x) | WS.length x < 2^8 =
    putWord8 76 >> putWord8 (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
putOp (OP_PUSHDATA2 x) | WS.length x < 2^16 =
    putWord8 77 >> putWord16be (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
putOp (OP_PUSHDATA4 x) | WS.length x < 2^32 =
    putWord8 78 >> putWord32be (fromIntegral $ WS.length x) >> putByteString (WS.toByteString x)
putOp OP_1NEGATE = putWord8 79
putOp OP_1 = putWord8 81
putOp OP_2 = putWord8 82
putOp OP_3 = putWord8 83
putOp OP_4 = putWord8 84
putOp OP_5 = putWord8 85
putOp OP_6 = putWord8 86
putOp OP_7 = putWord8 87
putOp OP_8 = putWord8 88
putOp OP_9 = putWord8 89
putOp OP_10 = putWord8 90
putOp OP_11 = putWord8 91
putOp OP_12 = putWord8 92
putOp OP_13 = putWord8 93
putOp OP_14 = putWord8 94
putOp OP_15 = putWord8 95
putOp OP_16 = putWord8 96
putOp OP_NOP = putWord8 97
putOp (OP_IF ops) = putWord8 99 >> putIfBlock ops
putOp (OP_NOTIF ops) = putWord8 100 >> putIfBlock ops
putOp OP_VERIFY = putWord8 105
putOp OP_RETURN = putWord8 106
putOp OP_TOALTSTACK = putWord8 107
putOp OP_FROMALTSTACK = putWord8 108
putOp OP_IFDUP = putWord8 115
putOp OP_DEPTH = putWord8 116
putOp OP_DROP = putWord8 117
putOp OP_DUP = putWord8 118
putOp OP_NIP = putWord8 119
putOp OP_OVER = putWord8 120
putOp OP_PICK = putWord8 121
putOp OP_ROLL = putWord8 122
putOp OP_ROT = putWord8 123
putOp OP_SWAP = putWord8 124
putOp OP_TUCK = putWord8 125
putOp OP_2DROP = putWord8 109
putOp OP_2DUP = putWord8 110
putOp OP_3DUP = putWord8 111
putOp OP_2OVER = putWord8 112
putOp OP_2ROT = putWord8 113
putOp OP_2SWAP = putWord8 114
putOp OP_CAT = putWord8 126
putOp OP_SUBSTR = putWord8 127
putOp OP_LEFT = putWord8 128
putOp OP_RIGHT = putWord8 129
putOp OP_SIZE = putWord8 130
putOp OP_INVERT = putWord8 131
putOp OP_AND = putWord8 132
putOp OP_OR = putWord8 133
putOp OP_XOR = putWord8 134
putOp OP_EQUAL = putWord8 135
putOp OP_EQUALVERIFY = putWord8 136
putOp OP_1ADD = putWord8 139
putOp OP_1SUB = putWord8 140
putOp OP_2MUL = putWord8 141
putOp OP_2DIV = putWord8 142
putOp OP_NEGATE = putWord8 143
putOp OP_ABS = putWord8 144
putOp OP_NOT = putWord8 145
putOp OP_0NOTEQUAL = putWord8 146
putOp OP_ADD = putWord8 147
putOp OP_SUB = putWord8 148
putOp OP_MUL = putWord8 149
putOp OP_DIV = putWord8 150
putOp OP_MOD = putWord8 151
putOp OP_LSHIFT = putWord8 152
putOp OP_RSHIFT = putWord8 153
putOp OP_BOOLAND = putWord8 154
putOp OP_BOOLOR = putWord8 155
putOp OP_NUMEQUAL = putWord8 156
putOp OP_NUMEQUALVERIFY = putWord8 157
putOp OP_NUMNOTEQUAL = putWord8 158
putOp OP_LESSTHAN = putWord8 159
putOp OP_GREATERTHAN = putWord8 160
putOp OP_LESSTHANOREQUAL = putWord8 161
putOp OP_GREATERTHANOREQUAL = putWord8 162
putOp OP_MIN = putWord8 163
putOp OP_MAX = putWord8 164
putOp OP_WITHIN = putWord8 165
putOp OP_RIPEMD160 = putWord8 166
putOp OP_SHA1 = putWord8 167
putOp OP_SHA256 = putWord8 168
putOp OP_HASH160 = putWord8 169
putOp OP_HASH256 = putWord8 170
putOp OP_CODESEPARATOR = putWord8 171
putOp OP_CHECKSIG = putWord8 172
putOp OP_CHECKSIGVERIFY = putWord8 173
putOp OP_CHECKMULTISIG = putWord8 174
putOp OP_CHECKMULTISIGVERIFY = putWord8 175
putOp OP_NOP1 = putWord8 176
putOp OP_NOP2 = putWord8 177
putOp OP_NOP3 = putWord8 178
putOp OP_NOP4 = putWord8 179
putOp OP_NOP5 = putWord8 180
putOp OP_NOP6 = putWord8 181
putOp OP_NOP7 = putWord8 182
putOp OP_NOP8 = putWord8 183
putOp OP_NOP9 = putWord8 184
putOp OP_NOP10 = putWord8 185

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

scriptOps :: Script -> Either String [OP]
scriptOps (Script ws) = runGet getOps . WS.toByteString $ ws
 where
  getOps = do empty <- isEmpty
              if empty then return []
                       else go =<< getOp
  go (Left OP_ELSE) = fail "scriptOps: Unexpected OP_ELSE"
  go (Left OP_ENDIF) = fail "scriptOps: Unexpected OP_ENDIF"
  go (Right op) = (op:) <$> getOps

opsScript :: [OP] -> Script
opsScript = Script . WS.fromByteString . runPut . mapM_ putOp

nullScript :: Script
nullScript = opsScript []

asBool :: ByteString -> Bool
asBool bs = integerByteStringLE bs /= 0

{- notes: OP_CHECKSIG pushes false rather than failing if decoding of the signature or the public key fails
          the alternate stack is cleared between invocations of runScript
          integers are little endian with a sign bit as the leading bit (this is NOT two's complement)
          False is equiavlent to any encoding of zero, including any encoding of negative zero
          success means that the top of the stack is non-zero (see previous line)
          All if statements are terminated between invocations of runScript
-}
runScript :: MakeHash -> [OP] -> MS.StateT ([ByteString],[ByteString]) Maybe ()
runScript mkHash script = mapM_ go script
 where
  getMain = do {(main,_) <- MS.get; return main}
  putMain x = do MS.modify $ \(_,alt) -> (x,alt)
  pop = do {(top:bot) <- getMain; putMain bot; return top}
  peek = do {(top:_) <- getMain; return top}
  push x = do {main <- getMain; putMain (x:main)}
  pushInteger = push . integerToByteStringLE
  popInteger = integerByteStringLE <$> pop
  pushBool True  = pushInteger 1
  pushBool False = pushInteger 0
  popBool = asBool <$> pop
  checkSig pubkeycode sigcode = either (const False) (const True)
                              $ do pubkey <- decode pubkeycode
                                   sig <- decode sigcode
                                   let check = verifySignature pubkey (mkHash script sig) (csSig sig)
                                   guard check
  checkMultiSig _ [] = True
  checkMultiSig [] _ = False
  checkMultiSig (pk:pks) (sig:sigs) | npks < nsigs = False -- short circut failure
                                    | otherwise    = checkMultiSig pks (if checkSig pk sig then sigs else sig:sigs)
   where
    npks, nsigs :: Integer
    npks  = genericLength pks
    nsigs = genericLength sigs
  go (OP_PUSHDATA ws)       = push (WS.toByteString ws)
  go (OP_PUSHDATA1 ws)      = push (WS.toByteString ws)
  go (OP_PUSHDATA2 ws)      = push (WS.toByteString ws)
  go (OP_PUSHDATA4 ws)      = push (WS.toByteString ws)
  go OP_DUP                 = do {(x:main) <- getMain; putMain (x:x:main)}
  go OP_VERIFY              = do {t <- popBool; guard t}
  go OP_CHECKSIG            = do pubkeycode <- pop
                                 sigcode <- pop
                                 pushBool (checkSig pubkeycode sigcode)
  go OP_CHECKSIGVERIFY      = go OP_CHECKSIG >> go OP_VERIFY
  go OP_HASH160             = do {t1 <- pop; push . encode . hash160BS $ t1}
  go OP_EQUAL               = do {t1 <- pop; t2 <- pop; pushBool (t1 == t2)}
  go OP_EQUALVERIFY         = go OP_EQUAL >> go OP_VERIFY
  {-- extended operations --}
  go OP_1NEGATE             = pushInteger (-1)
  go OP_1                   = pushInteger 1
  go OP_2                   = pushInteger 2
  go OP_3                   = pushInteger 3
  go OP_4                   = pushInteger 4
  go OP_5                   = pushInteger 5
  go OP_6                   = pushInteger 6
  go OP_7                   = pushInteger 7
  go OP_8                   = pushInteger 8
  go OP_9                   = pushInteger 9
  go OP_10                  = pushInteger 10
  go OP_11                  = pushInteger 11
  go OP_12                  = pushInteger 12
  go OP_13                  = pushInteger 13
  go OP_14                  = pushInteger 14
  go OP_15                  = pushInteger 15
  go OP_16                  = pushInteger 16
  go OP_NOP                 = return ()
  go OP_RETURN              = fail "OP_RETURN called"
  go OP_TOALTSTACK          = do (x1:main, alt) <- MS.get
                                 MS.put (main, x1:alt)
  go OP_FROMALTSTACK        = do (main, x1:alt) <- MS.get
                                 MS.put (x1:main, alt)
  go OP_IFDUP               = do {t <- peek; when (asBool t) (push t)}
  go OP_DEPTH               = do {main <- getMain; pushInteger (genericLength main)}
  go OP_DROP                = do (_:main) <- getMain
                                 putMain main
  go OP_NIP                 = do (x2:_:main) <- getMain
                                 putMain (x2:main)
  go OP_OVER                = do (x2:x1:main) <- getMain
                                 putMain (x1:x2:x1:main)
  go OP_PICK                = do n <- popInteger
                                 main <- getMain
                                 let (front,irest) = genericSplitAt n main
                                 (i:_) <- return irest
                                 putMain (i:front ++ irest)
  go OP_ROLL                = do n <- popInteger
                                 main <- getMain
                                 let (front,irest) = genericSplitAt n main
                                 (i:rest) <- return irest
                                 putMain (i:front ++ rest)
  go OP_ROT                 = do (x3:x2:x1:main) <- getMain
                                 putMain (x1:x3:x2:main)
  go OP_SWAP                = do (x2:x1:main) <- getMain
                                 putMain (x1:x2:main)
  go OP_TUCK                = do (x2:x1:main) <- getMain
                                 putMain (x2:x1:x2:main)
  go OP_2DROP               = do (_:_:main) <- getMain
                                 putMain main
  go OP_2DUP                = do (x2:x1:main) <- getMain
                                 putMain (x2:x1:x2:x1:main)
  go OP_3DUP                = do (x3:x2:x1:main) <- getMain
                                 putMain (x3:x2:x1:x3:x2:x1:main)
  go OP_2OVER               = do (x4:x3:x2:x1:main) <- getMain
                                 putMain (x2:x1:x4:x3:x2:x1:main)
  go OP_2ROT                = do (x6:x5:x4:x3:x2:x1:main) <- getMain
                                 putMain (x2:x1:x6:x5:x4:x3:main)
  go OP_2SWAP               = do (x4:x3:x2:x1:main) <- getMain
                                 putMain (x2:x1:x4:x3:main)
  go OP_SIZE                = do {t <- peek; pushInteger (toInteger (length t))}
  go OP_1ADD                = do {t <- popInteger; pushInteger (t + 1)}
  go OP_1SUB                = do {t <- popInteger; pushInteger (t - 1)}
  go OP_NEGATE              = do {t <- popInteger; pushInteger (negate t)}
  go OP_ABS                 = do {t <- popInteger; pushInteger (abs t)}
  go OP_NOT                 = do {t <- popBool; pushBool (not t)}
  go OP_0NOTEQUAL           = do {t <- popBool; pushBool t}
  go OP_ADD                 = do {t2 <- popInteger; t1 <- popInteger; pushInteger (t1 + t2)}
  go OP_SUB                 = do {t2 <- popInteger; t1 <- popInteger; pushInteger (t1 - t2)}
  go OP_BOOLAND             = do {t2 <- popBool; t1 <- popBool; pushBool (t1 && t2)}
  go OP_BOOLOR              = do {t2 <- popBool; t1 <- popBool; pushBool (t1 || t2)}
  go OP_NUMEQUAL            = do {t2 <- popInteger; t1 <- popInteger; pushBool (t1 == t2)}
  go OP_NUMEQUALVERIFY      = go OP_NUMEQUAL >> go OP_VERIFY
  go OP_NUMNOTEQUAL         = do {t2 <- popInteger; t1 <- popInteger; pushBool (t1 /= t2)}
  go OP_LESSTHAN            = do {t2 <- popInteger; t1 <- popInteger; pushBool (t1 < t2)}
  go OP_GREATERTHAN         = do {t2 <- popInteger; t1 <- popInteger; pushBool (t1 > t2)}
  go OP_LESSTHANOREQUAL     = do {t2 <- popInteger; t1 <- popInteger; pushBool (t1 <= t2)}
  go OP_GREATERTHANOREQUAL  = do {t2 <- popInteger; t1 <- popInteger; pushBool (t1 >= t2)}
  go OP_MIN                 = do {t2 <- popInteger; t1 <- popInteger; pushInteger (t1 `min` t2)}
  go OP_MAX                 = do {t2 <- popInteger; t1 <- popInteger; pushInteger (t1 `max` t2)}
  go OP_WITHIN              = do {tmax <- popInteger; tmin <- popInteger; t <- popInteger; pushBool (tmin <= t && t < tmax) }
  go OP_CHECKMULTISIG       = do npk <- popInteger;
                                 pubkeycodes <- reverseReplicateM npk pop
                                 nsig <- popInteger;
                                 guard (nsig <= npk);
                                 sigcodes <- reverseReplicateM nsig pop
                                 _ <- pop -- Due to a bug in the protocol there is an extra pop here.
                                 pushBool (checkMultiSig pubkeycodes sigcodes)
  go OP_CHECKMULTISIGVERIFY = go OP_CHECKMULTISIG >> go OP_VERIFY
  go (OP_IF blk)            = goIfBlock blk =<< popBool
  go (OP_NOTIF blk)         = goIfBlock blk =<< not <$> popBool
  go OP_RIPEMD160           = do {t1 <- pop; push . encode . ripemd160BS $ t1}
  go OP_SHA256              = do {t1 <- pop; push . encode . sha256BS $ t1}
  go OP_HASH256             = do {t1 <- pop; push . encode . hashBS $ t1}
  go OP_NOP1                = return ()
  go OP_NOP2                = return ()
  go OP_NOP3                = return ()
  go OP_NOP4                = return ()
  go OP_NOP5                = return ()
  go OP_NOP6                = return ()
  go OP_NOP7                = return ()
  go OP_NOP8                = return ()
  go OP_NOP9                = return ()
  go OP_NOP10               = return ()
  go x@OP_CAT               = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_SUBSTR            = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_LEFT              = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_RIGHT             = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_INVERT            = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_AND               = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_OR                = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_XOR               = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_2MUL              = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_2DIV              = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_MUL               = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_DIV               = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_MOD               = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_LSHIFT            = fail $ "operation" ++ show x ++ " is disabled"
  go x@OP_RSHIFT            = fail $ "operation" ++ show x ++ " is disabled"
  goIfBlock (NENil ops) True = mapM_ go ops
  goIfBlock (NENil _  ) False = return ()
  goIfBlock (NECons ops rest) True = mapM_ go ops >> goIfBlock rest False
  goIfBlock (NECons _   rest) False = goIfBlock rest True

reverseReplicateM :: (MonadPlus m) => Integer -> m a -> m [a]
-- reverseReplicateM n cmd | 0 <= n = reverse <$> replicateM n cmd
--                         | otherwise = fail "reverseReplicateM given negative number"
reverseReplicateM n cmd = go n []
  where
   go 0 l = return l
   go m l | 0 < m = do { i <- cmd; go (m-1) (i:l) }
          | otherwise = fail "reverseReplicateM given negative number"

type MakeHash = [OP] -> CoinSignature -> Integer

newtype ScriptMonad a = ScriptMonad (MR.ReaderT MakeHash (MS.StateT ([ByteString]) Maybe) a)

instance Functor ScriptMonad where
  fmap f (ScriptMonad sm) = ScriptMonad (fmap f sm)

instance Applicative ScriptMonad where
  pure = ScriptMonad . pure
  ScriptMonad f <*> ScriptMonad x = ScriptMonad $ f <*> x

instance Monad ScriptMonad where
  return = ScriptMonad . return
  fail = ScriptMonad . fail
  ScriptMonad x >>= f = ScriptMonad $ (x >>= \x' -> let ScriptMonad y' = f x' in y')

doScript :: [OP] -> ScriptMonad ()
doScript script = ScriptMonad . MR.ReaderT $ go
 where
  go mkHash = MS.StateT (\inStack -> proj <$> MS.runStateT (runScript mkHash script) (inStack,[]))
   where
    proj (a, (outStack,_)) = (a, outStack)

execScriptMonad :: MakeHash -> ScriptMonad () -> Maybe ()
execScriptMonad mkHash (ScriptMonad sm) = do
  (top:_) <- MS.execStateT (MR.runReaderT sm mkHash) []
  guard (asBool top)
