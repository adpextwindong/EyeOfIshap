import Control.Monad

bind f = join . fmap f

foo :: Monad m => (b -> m c) -> (a -> m b) -> m a -> m c
foo f g = bind f . bind g

bar :: Monad m => (b -> m c) -> (a -> m b) -> m a -> m c
bar f g = bind (bind f . g)

quux :: Monad m => m a -> m a
quux = bind return
