m >>= k  = StateT $ \ s -> do
	~(a, s') <- runStateT m s
	runStateT (k a) s'
	
return a = StateT (\s -> return(a, s))

m >> k  = StateT $ \ s -> do
	~(_, s') <- runStateT m s
	runStateT k s'

lift m = StateT $ \ s -> do
	a <- m
	return (a, s)
	
modify f =  StateT (\s ->return ((), f s))

put s = StateT $ \t -> return ((), s) 

mapStateT f m = StateT $ f . runStateT m