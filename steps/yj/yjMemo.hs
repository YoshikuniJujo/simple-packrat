{-# LANGUAGE TypeFamilies #-}

import Text.Papillon
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Error
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Data.Functor.Identity
import GHC.Base

import System.Environment

main :: IO ()
main = do
	str : _ <- getArgs
	case runError . yj $ parse str of
		Left e -> putStrLn $ peMessage e
		Right (r, _) -> print r

data Initial = Y | J deriving Show

data Derivs
    = Derivs {yj :: (Control.Monad.Trans.Error.ErrorT (ParseError (Pos String)
                                                                  Derivs)
                                                      Data.Functor.Identity.Identity
                                                      ((Initial, Initial), Derivs)),
              y :: (Control.Monad.Trans.Error.ErrorT (ParseError (Pos String)
                                                                 Derivs)
                                                     Data.Functor.Identity.Identity
                                                     (Initial, Derivs)),
              j :: (Control.Monad.Trans.Error.ErrorT (ParseError (Pos String)
                                                                 Derivs)
                                                     Data.Functor.Identity.Identity
                                                     (Initial, Derivs)),
              char :: (Control.Monad.Trans.Error.ErrorT (ParseError (Pos String)
                                                                    Derivs)
                                                        Data.Functor.Identity.Identity
                                                        (Token String, Derivs)),
              position :: (Control.Monad.Trans.Error.ErrorT (ParseError (Pos String)
                                                                        Derivs)
                                                            Data.Functor.Identity.Identity
                                                            (Pos String, Derivs))}
parse :: String -> Derivs
parse = parse11_0 initialPos
          where parse11_0 pos7_1 s10_2 = d9_3
                              where d9_3 = Derivs yj1_4 y2_5 j3_6 chars8_7 (return (pos7_1,
                                                                                    d9_3))
                                    yj1_4 = Control.Monad.Trans.State.Lazy.runStateT yj4_8 d9_3
                                    y2_5 = Control.Monad.Trans.State.Lazy.runStateT y5_9 d9_3
                                    j3_6 = Control.Monad.Trans.State.Lazy.runStateT j6_10 d9_3
                                    chars8_7 = Control.Monad.Trans.State.Lazy.runStateT (case getToken s10_2 of
                                                                                             Just (c12_11,
                                                                                                   s'13_12) -> do {Control.Monad.State.Class.put (parse11_0 (updatePos c12_11 pos7_1) s'13_12);
                                                                                                                   return c12_11}
                                                                                             _ -> Control.Monad.Trans.State.Lazy.StateT position >>= (Control.Monad.Error.Class.throwError . mkParseError "" "end of input" "" undefined [])) d9_3
                yj4_8 = foldl1 GHC.Base.mplus [do {y <- Control.Monad.Trans.State.Lazy.StateT y;
                                                   j <- Control.Monad.Trans.State.Lazy.StateT j;
                                                   return (y, j)}]
                y5_9 = foldl1 GHC.Base.mplus [do {d15_13 <- Control.Monad.State.Class.get;
                                                  t18_14 <- Control.Monad.Trans.State.Lazy.StateT char;
                                                  case t18_14 of {
                                                      'Y' -> return ();
                                                      _ -> Control.Monad.Trans.State.Lazy.StateT position >>= (Control.Monad.Error.Class.throwError . mkParseError "'Y'" "not match pattern: " "" d15_13 ["char"]) };
                                                  let {'Y' = t18_14};
                                                  return ();
                                                  return Y}]
                j6_10 = foldl1 GHC.Base.mplus [do {d19_15 <- Control.Monad.State.Class.get;
                                                   t20_16 <- Control.Monad.Trans.State.Lazy.StateT char;
                                                   case t20_16 of {
                                                       'J' -> return ();
                                                       _ -> Control.Monad.Trans.State.Lazy.StateT position >>= (Control.Monad.Error.Class.throwError . mkParseError "'J'" "not match pattern: " "" d19_15 ["char"]) };
                                                   let {'J' = t20_16};
                                                   return ();
                                                   return J}]
