module BigDecimal where

data BigDecimal = BigDecimal Integer Integer deriving (Show, Read, Eq)
instance Num BigDecimal where
    a + b         = plus $ matchDigits (a, b)
    a * b         = mul (a, b)
    abs           = undefined
    signum        = undefined
    fromInteger i = BigDecimal i 0
    negate (BigDecimal num digits) = BigDecimal (-num) digits

instance Fractional BigDecimal where
    a / b = divi $ matchDigits (a ,b)
    fromRational = undefined

-- | add two BigDecimals with same precision
plus :: (BigDecimal, BigDecimal) -> BigDecimal
plus (BigDecimal integerA scaleA, BigDecimal integerB scaleB)
    = BigDecimal (integerA + integerB) scaleA

-- | multiply two BigDecimals    
mul :: (BigDecimal, BigDecimal) -> BigDecimal
mul (BigDecimal integerA scaleA, BigDecimal integerB scaleB)
    = BigDecimal (integerA * integerB) (scaleA + scaleB)

-- | divide two BigDecimals
divi :: (BigDecimal, BigDecimal) -> BigDecimal
divi (BigDecimal numA digitsA, BigDecimal numB digitsB) = 
    BigDecimal 
        (round $ fromInteger numA / fromInteger numB *10^16) 
        16

-- | match the scales of a tuple of BigDecimals
matchDigits :: (BigDecimal, BigDecimal) -> (BigDecimal, BigDecimal)
matchDigits (a@(BigDecimal integerA scaleA), b@(BigDecimal integerB scaleB))
    | scaleA < scaleB = (BigDecimal (integerA * 10^(scaleB-scaleA)) scaleB, b)
    | scaleA > scaleB = (a, BigDecimal (integerB * 10^(scaleA-scaleB)) scaleA)
    | otherwise       = (a, b)


a = BigDecimal 1234 2
b = BigDecimal 5678 3
ad = 12.34
bd = 5.678    