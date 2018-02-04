{-# LANGUAGE MultiParamTypeClasses #-}
module Stack where

-- | defines a stack interface with Stack type s and Element type e 
class Stack s where
    empty   :: s e
    isEmpty :: s e -> Bool
    top     :: s e -> Maybe e
    pop     :: s e -> (Maybe e, s e)
    push    :: e -> s e -> s e
    reverse :: s e -> s e

-- | a queue interface with Q type q and Element type e    
class Queue q where
    enq :: e -> q e -> q e
    deq :: q e -> (Maybe e, q e)
    
--class (Queue q, Stack s) => StackBasedQueue q s where
--    --enq :: e -> q e -> q e
--    enq x (q (sIn :: s)  (sOut :: s)) = q (push x sIn) sOut
