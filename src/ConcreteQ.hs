{-# LANGUAGE GADTs #-}
module ConcreteQ where
import           Stack

newtype StackImpl e = StackImpl [e] deriving (Show)

instance Stack StackImpl where
    empty      = StackImpl []
    isEmpty (StackImpl s)  = null s
    pop (StackImpl [])     = (Nothing, empty)
    pop (StackImpl (x:xs)) = (Just x, StackImpl xs)
    top (StackImpl [])     = Nothing
    top (StackImpl (x:xs)) = Just x
    push x (StackImpl s)   = StackImpl (x:s)
    reverse (StackImpl s)  = StackImpl (Prelude.reverse s)

stack :: StackImpl Int
stack = push 3 $ push 2 $ push 1 $ StackImpl []

-- GADTs are required to allow the following definition of a Stack based Queue
data StackBasedQueue e where
    ConsSQ :: (Stack s) => s e -> s e -> StackBasedQueue e


instance Queue StackBasedQueue where
    --enq :: e -> Q e -> Q e
    enq x (ConsSQ sIn sOut) = ConsSQ (push x sIn) sOut

    --deq :: Q e -> (Maybe e, Q e)
    deq q@(ConsSQ sIn sOut)
        | isEmpty sOut && isEmpty sIn = (Nothing, q)
        | isEmpty sOut = deq (ConsSQ empty (Stack.reverse sIn))
        | otherwise    = let (popped, stackOut) = pop sOut
                         in  (popped, ConsSQ sIn stackOut)

q = ConsSQ stack (StackImpl [])

newtype ListBasedQueue e = ConsLQ [e] deriving (Show)

instance Queue ListBasedQueue where
    enq x (ConsLQ l) = ConsLQ (x:l)

    deq q@(ConsLQ []) = (Nothing, q)
    deq q@(ConsLQ l) = let popped = last l
                           rest   = Prelude.reverse $ tail $ Prelude.reverse l
                       in (Just popped, ConsLQ rest)
