# HaskellExceptionsTable
<table>
<tr>
  <th> Разновидность Exception-а </th> <th> Комментарий </th>  <th> Место кидания </th> <th> Способ кидания </th> <th> Место поимки </th> <th> Способ поимки </th> <th> Пример </th>
</tr>
<tr> 
  <td> ExceptT                   </td> <td> Имитация исключения с помощью Either </td> <td> Чистый код </td> <td> 

```haskell
throwError :: MonadError e m => e -> m a
```
</td> <td> Чистый код </td> <td> 

```haskell
catchError :: MonadError e m => m a -> (e -> m a) -> m a
``` 
</td> <td>

```haskell
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
```
</tr>
<tr> <td> Exception                 </td> <td> Чистый Exception </td> <td> Чистый код </td> <td> 

```haskell
throw :: Exception e => e -> a
```
</td> <td> 

```haskell
IO
```
</td> <td>
 
```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```
</td> <td>

```haskell
when (1 `div` 0 > 0) (putStrLn "not to happen")
    `catch` \e -> putStrLn $ "Caught " <> show (e :: ArithException)
```
</td> </tr>
<tr> <td> IOException               </td> <td> "Грязный" Exception </td> <td> 
   
```haskell
IO
```
</td> <td> 

```haskell
throwIO :: Exception e => e -> IO a
```
</td> <td>  

```haskell
IO
```
</td> <td> 

```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```  
</td> <td> 

```haskell
throwIO MyException
    `catch` \MyException -> putStrLn "Caught my IO exception"
```
</td> </tr>
<tr> <td> AsyncException            </td> <td> Асинхронное исключение, аналог сигналов в линуксе </td> <td>  

```haskell
IO
```
</td> <td> 

```haskell
throwTo :: Exception e => ThreadId -> e -> IO ()
```
</td> <td> 
 
```haskell
IO
```
(поток с указанным `id`) </td> <td> 
 
```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```
</td> <td>

```haskell
tid <- forkIO $
    (threadDelay 1000000 >> putStrLn "Done")
      `catch` \MyException -> putStrLn "Caught my async exception"
threadDelay 500000
throwTo tid MyException
```
</td> </tr>
</table>
