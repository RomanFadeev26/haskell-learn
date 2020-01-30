data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded, Eq)
data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN size letter = toEnum rotation
                   where half = (size `div` 2)
                         offset = half + (fromEnum letter)
                         rotation = offset `mod` size

rotChar::Char -> Char
rotChar char = rotN maxSize char
               where maxSize = 1 + (fromEnum (maxBound::Char))

message :: [FourLetterAlphabet]
message = [L1, L4, L1, L3, L2]

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha,Alpha,Beta,Alpha,Kappa]

rotFourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
rotFourLetterAlphabetEncoder word = map (rotN maxSize) word
                             where maxSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)


rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
 where halfN = n `div` 2
       charPos = fromEnum c
       offset = if even n
                then charPos + halfN
                else charPos + halfN + 1
       rotation =  offset `mod` n

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder word = map rot3lEncoder word
                          where size = 1 + fromEnum (maxBound::ThreeLetterAlphabet)
                                rot3lEncoder = rotN size

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder word = map rot3Decoder word
                        where alphabetSize = 1 + fromEnum (maxBound::ThreeLetterAlphabet)
                              rot3Decoder = rotNdecoder alphabetSize

rotEncoder :: [Char] -> [Char]
rotEncoder = map rotChar

rotDecoder :: [Char] -> [Char]
rotDecoder = map rotCharDecoder
                  where size = 1 + fromEnum (maxBound::Char)
                        rotCharDecoder = rotNdecoder size

rotFourLetterAlphabetDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
rotFourLetterAlphabetDecoder = map rot4ldecoder
                               where rot4ldecoder = rotNdecoder 4

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool, Bool) -> Bool
xorPair (value1, value2) = xorBool value1 value2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

intoBits' :: Int -> [Bool]
intoBits' 0 = [False]
intoBits' 1 = [True]
intoBits' num = if (remainder == 0)
                then False:intoBits' nextVal
                else True:intoBits' nextVal
                where remainder = num `mod` 2
                      nextVal = num `div` 2

maxBits :: Int
maxBits = length (intoBits' maxBound)

intToBits :: Int -> [Bool]
intToBits n = leadingFalses ++ reversedBits
                  where reversedBits = reverse (intoBits' n)
                        missingBits = maxBits - length (reversedBits)
                        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits = intToBits.fromEnum

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
                  where bitsLength = length bits
                        indexes = [bitsLength - 1, bitsLength - 2..0]
                        trueLocations = filter (\x -> fst x) (zip bits indexes)

bitsToChar :: Bits -> Char
bitsToChar = toEnum.bitsToInt

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits plainTextBits)
                              where padBits = map charToBits pad
                                    plainTextBits = map charToBits plainText
applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar (applyOTP' pad plainText)

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
      encode :: a -> String -> String
      decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
      encode Rot text = rotEncoder text
      decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
      encode (OTP pad) text = applyOTP pad text
      decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound..maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1321 38 1000

randomOTP :: Int -> String -> String
randomOTP seed str = map bitsToChar (map (\pair -> (fst pair) `xor` (snd pair)) bitsZipped)
                        where zippedWithPairs = foldl (\acc ch -> [(ch, examplePRNG (snd (head acc)))] ++ acc) [('A', seed)] str
                              bitsZipped = map (\pair -> (charToBits (fst pair), intToBits (snd pair))) (tail (reverse zippedWithPairs))

data StreamCipher = Seed Int

instance Cipher StreamCipher where
      encode (Seed seed) text = randomOTP seed text
      decode (Seed seed) text = randomOTP seed text