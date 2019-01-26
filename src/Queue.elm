module Queue exposing (Queue, dequeue, emptyQueue, enqueue, mapQ)

import List exposing (append, drop, head)


type Queue a
    = Queue (List a)


emptyQueue : Queue a
emptyQueue =
    Queue []


mapQ : (List a -> List a) -> Queue a -> Queue a
mapQ fn (Queue xs) =
    Queue (fn xs)


enqueue : a -> Queue a -> Queue a
enqueue x q =
    mapQ (\xs -> append xs [ x ]) q


dequeue : Queue a -> ( Maybe a, Queue a )
dequeue (Queue xs) =
    ( head xs, Queue (drop 1 xs) )
