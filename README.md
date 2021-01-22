# HaskellExceptionsTable
<table>
<tr>
  <th> Разновидность Exception-а </th> <th> Комментарий </th>  <th> Место кидания </th> <th> Способ кидания </th> <th> Место отлова </th> <th> Способ поимки </th>
</tr>
<tr> 
  <td> ExceptT                   </td> <td> Имитация исключения с помощью Either </td> <td> Чистый код </td> <td> 

```haskell
throwError
```
</td> <td> Чистый код </td> <td> 

```haskell
catchError
``` 
</td>
</tr>
<tr> <td> Exception                 </td> <td> Чистый Exception </td> <td> Чистый код </td> <td> 

```haskell
throw
```
</td> <td> 

```haskell
IO
```
</td> <td>
 
```haskell
catch
```
</td>
</tr>
<tr> <td> IOException               </td> <td> "Грязный" Exception </td> <td> 
   
```haskell
IO
```
</td> <td> 

```haskell
throwIO
```
</td> <td>  

```haskell
IO
```
</td> <td> 

```haskell
catch
```  
</td> </tr>
<tr> <td> AsyncException            </td> <td> Асинхронное исключение, аналог сигналов в линуксе </td> <td>  

```haskell
IO
```
</td> <td> 

```haskell
throwTo
```
</td> <td> 
 
```haskell
IO
```
(поток с указанным `id`) </td> <td> 
 
```haskell
catch
```
</td>  </tr>
</table>

```haskell
throwError :: MonadError e m => e -> m a
```

```haskell
catchError :: MonadError e m => m a -> (e -> m a) -> m a
``` 

```haskell
throw :: Exception e => e -> a
```

```haskell
throwIO :: Exception e => e -> IO a
```

```haskell
throwTo :: Exception e => ThreadId -> e -> IO ()
```

```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

## Examples

* ExceptT

```haskell
import Control.Monad.Except

-- An IO monad which can return String failure.
-- It is convenient to define the monad type of the combined monad,
-- especially if we combine more monad transformers.
type LengthMonad = ExceptT String IO

main = do
  -- runExceptT removes the ExceptT wrapper
  r <- runExceptT calculateLength
  reportResult r

-- Asks user for a non-empty string and returns its length.
-- Throws an error if user enters an empty string.
calculateLength :: LengthMonad Int
calculateLength = do
  -- all the IO operations have to be lifted to the IO monad in the monad stack
  liftIO $ putStrLn "Please enter a non-empty string: "
  s <- liftIO getLine
  if null s
    then throwError "The string was empty!"
    else return $ length s

-- Prints result of the string length calculation.
reportResult :: Either String Int -> IO ()
reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))
```

* Real exceptions
```haskell
data MyException = MyException   deriving Show
instance Exception MyException

main = do
  throwIO MyException
    `catch` \MyException -> putStrLn "Caught my IO exception"
  when (1 `div` 0 > 0) (putStrLn "not to happen")
    `catch` \e -> putStrLn $ "Caught " <> show (e :: ArithException)
  tid <- forkIO $
    (threadDelay 1000000 >> putStrLn "Done")
      `catch` \MyException -> putStrLn "Caught my async exception"
  threadDelay 500000
  throwTo tid MyException
```
