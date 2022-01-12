module Variables where

import           Control.Monad.Except (MonadError (throwError),
                                       MonadIO (liftIO), runExceptT)
import           Core                 (Env, IOThrowsError,
                                       LispError (UnboundVal), LispVal,
                                       ThrowsError, extractValue, trapError)
import           Data.Functor         ((<&>))
import           Data.IORef           (newIORef, readIORef, writeIORef)
import           Data.Maybe           (isJust)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

-- 変数が定義されているのかの確認
isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

-- 変数の値の取り出し
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  let err = throwError $ UnboundVal "Getting an unbound variable: " var
      value = lookup var env
  maybe err (liftIO . readIORef) value

-- 変数への値のセット
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVal "Setting an unbound variable: " var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value


-- 新しい変数の定義
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef): env)
      return value

-- 一度に変数の定義できる
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
                                    ref <- newIORef value;
                                    return (var, ref);
