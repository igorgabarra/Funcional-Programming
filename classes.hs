module Classes where


class YesNo t where
    yesno :: t -> Bool

instance YesNo Bool where
    yesno x = x
    
instance YesNo [a] where
    yesno []    = False
    yesno (_:_) = True

instance YesNo (Maybe a) where
    yesno Nothing  = False
    yesno (Just _) = True

instance YesNo Int where --Integer
    yesno 0 = False
    yesno _ = True

data Semaforo = Verde | Amarelo | Vermelho

instance YesNo Semaforo where
    yesno Vermelho = False
    yesno _        = True


yesnoIf :: YesNo t => t -> a -> a -> a
yesnoIf test x y =
      if yesno test then x else y
