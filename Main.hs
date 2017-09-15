import Control.Monad.State
import System.IO.Unsafe
import Data.Maybe

import LexLanguage
import ParLanguage
import SkelLanguage
import PrintLanguage
import AbsLanguage
import ErrM
import AbsLanguage
import Data.Map.Lazy
import Data.List

type MyState a = StateT (Data.Map.Lazy.Map String String) IO a

genInf :: [String]
genInf = do
    let cs = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","x","y","z"]
    cs ++ (zipWith (\a -> \b -> a ++ b) cs genInf)

getNextUniq :: Int -> String
getNextUniq i = genInf !! i

sample :: Exp -> MyState String
sample e = do
    liftIO $ print e
    eval e

main = interact (\s -> unsafePerformIO $ start s)
 
start :: String -> IO String
start b = do
    let e = lexAndParse b
    (a,b) <- runStateT (sample e) Data.Map.Lazy.empty
    return a

lexAndParse :: String -> Exp
lexAndParse s = do
    let Ok e = pExp (myLexer s)
    e

eval :: Exp -> MyState String
eval x = case x of
    EOR exp1 exp2 -> do
        i1 <- eval exp1
        i2 <- eval exp2
        return (i1 ++ " | " ++ i2)
    EAND exp1 exp2 -> do
        i1 <- eval exp1
        i2 <- eval exp2
        return (i1 ++ " & " ++ i2)
    ESTR (Ident s) -> do
        state <- get
        let res = Data.Map.Lazy.lookup s state
        case res of 
            Nothing -> do
                let len = size state
                let value = getNextUniq len
                let newState = Data.Map.Lazy.insert s value state
                put newState
                return value
            Just a -> do
                return a