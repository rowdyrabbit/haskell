{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell io.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  do
    args <- getArgs
    void (sequenceIO (map run args))

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run filepath =
  do
    content <- readFile filepath
    files <- getFiles (lines content)
    printFiles files

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles Nil = pure(Nil)
getFiles (h :. t) =
    getFile h >>= \_ -> getFiles t
    -- getFiles paths = sequenceIO (map getFile paths) -- better solution than recursion.

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile path =
 readFile path >>= \content ->
 pure (path, content)


printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles Nil = pure()
printFiles ((x,y) :. t) =
  printFile x y >>= \_ -> printFiles t

--  printFiles x =
--    void (sequenceIO (map (\(path, ct) -> printFile path ct) x) - Tony's solution


printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile path contents =
  putStrLn path >>= \_ -> putStrLn contents


sequenceIO ::
  List (IO a)
    -> IO (List a)
sequenceIO  Nil = pure Nil
sequenceIO (h :. t) =
-- h :: IO a
-- sequence t :: IO (List a)
-- a :: a
-- r :: List a
  h >>= \a ->
  sequenceIO t >>= \r ->
  pure (a :. r)




--    foldRight (twiceIO (:.)) (pure Nil)
--
--
--twiceIO :: (a -> b-> c)
--  -> IO a
--  -> IO b
--  -> IO c
--twiceIO f a b =
--   do aa <- a
--      bb <- b
--      pure (f aa bb)