import Prelude hiding ((||)) -- Explicity says we are importing the prelude, but removing the or boolean function from the import.

|| :: Bool -> Bool -> Bool
True || True    = True
False || True   = True
True || False   = True
False || False  = False

(||) :: Bool -> Bool -> Bool 
False || False  = False
_ || _          = True

(||) :: Bool -> Bool -> Bool
True || _ = True
False || p = p