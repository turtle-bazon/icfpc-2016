module Developer (Developer (..)) where

data Developer = Developer { name :: String
                           , fullName :: String
                           }

instance Show Developer where
    show developer = fullName developer ++ " (" ++ name developer ++ ")"
