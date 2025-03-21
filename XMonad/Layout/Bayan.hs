{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module XMonad.Layout.Bayan (
    Bayan(..)) where

import XMonad
import qualified XMonad.StackSet as W
import Data.Maybe
import Data.Tuple

data Bayan a = Bayan { bayanNMaster :: !Int
                     , bayanMasterIdx :: !Int
                     , bayanPrevStack :: Maybe (W.Stack Window)
                     }

    deriving ( Read, Show )

instance LayoutClass Bayan Window where
    doLayout l' r' s' = return $ doLayout' l' r' s'
      where doLayout' l@(Bayan nMaster masterIdx prevStack) r s
                | masterIdx >= nMaster = doLayout' l{bayanMasterIdx = nMaster - 1} r s
                | masterIdx < 0 = doLayout' l{bayanMasterIdx = 0} r s
                | otherwise = let newLayout = maybe (l {bayanPrevStack = Just s}) (handleStack s nMaster masterIdx) prevStack
                                  newMasterIdx = bayanMasterIdx newLayout
                                in (flip (,) $ Just newLayout) $ (drawWindows r $
                                    (reverse . fmap fromJust . takeWhile isJust . take (max 0 newMasterIdx) $ ((Just <$> (W.up s)) ++ (reverse $ (Just <$> (W.down s))) ++ [Nothing]))
                                 ++ (W.focus s)
                                  : (fmap fromJust . takeWhile isJust . take (max 0 (nMaster - newMasterIdx - 1)) $ ((Just <$> (W.down s)) ++ (reverse $ (Just <$> (W.up s))) ++ [Nothing])))

            handleStack newStack nMaster masterIdx prevStack
              | (W.focus prevStack == W.focus newStack) = Bayan nMaster masterIdx (Just newStack)
              | (not . null . W.up $ newStack) && (W.focus prevStack == (head . W.up $ newStack)) = Bayan nMaster (min (nMaster - 1) (masterIdx + 1)) (Just newStack)
              | (not . null . W.down $ newStack) && (W.focus prevStack == (head . W.down $ newStack)) = Bayan nMaster (max 0 (masterIdx - 1)) (Just newStack)
              | otherwise = Bayan nMaster masterIdx (Just newStack)
            drawWindows r windows' = zip windows' (splitVertically (length windows') r)

    emptyLayout (Bayan nMaster _ _) _ = return ([], Just $ (Bayan nMaster 0 Nothing))

    pureMessage l m = fmap incmastern (fromMessage m)
      where incmastern (IncMasterN d) = l { bayanNMaster = max 1 ((bayanNMaster l) + d) }

