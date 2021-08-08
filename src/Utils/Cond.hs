module Utils.Cond where


(<||>) :: (Monad m) => (Bool, m a) -> (Bool, m a) -> (Bool, m a)
(<||>) (True, p) _ = (True, p)
(<||>) _ q = q

(||>) :: (Monad m) => (Bool, m a) -> m a -> (Bool, m a)
(||>) p q = p <||> (True, q)

(|>) :: (Monad m) => (Bool, m a) -> m a
(|>) (_, p) = p