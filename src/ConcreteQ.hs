{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ConcreteQ where
import Stack 

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

--data Q e = Q (StackImpl e) (StackImpl e) deriving (Show)
data Stack s =>  Q s =  Q s s deriving (Show)

instance Queue Q where
    --enq :: e -> Q e -> Q e
    enq x (Q sIn sOut) = Q (push x sIn) sOut

    --deq :: Q e -> (Maybe e, Q e)
    deq q@(Q sIn sOut)
        | isEmpty sOut && isEmpty sIn = (Nothing, q)
        | isEmpty sOut = deq (Q empty (Stack.reverse sIn))
        | otherwise    = let (popped, stackOut) = pop sOut
                        in (popped, Q sIn stackOut)

q = Q stack (StackImpl [])                     
