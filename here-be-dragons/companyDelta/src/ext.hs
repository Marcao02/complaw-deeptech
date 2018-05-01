-- in which we understand the GHC extension
{-# LANGUAGE ExistentialQuantification #-}

-- often, we want a heterogeneous list:
-- a list whose elements are, ultimately, of different types.
-- Now, Haskell doesn't allow list elements to be of different types!

--   myList = [5, (), False] -- this doesn't work.

-- So we have to wrap the underlying elements in a uniform box.
data MyBox = WrapInt Int
           | WrapUnit ()
           | WrapBool Bool

-- Now Haskell is mollified; the boxes are all the same type.
boxList :: [MyBox]
boxList = [WrapUnit (), WrapInt 5, WrapBool True]

-- let's write a utility function to see what's in the box.
showBox :: MyBox -> String
showBox (WrapUnit x) = "we have a unit: " ++ show x
showBox (WrapInt  x) = "we have an int: " ++ show x
showBox (WrapBool x) = "we have a bool: " ++ show x

-- but this will get tedious quickly; we want the MyBox type to become
-- more capable of holding underlying elements of different types,
-- but we don't want to have to explicitly write
-- a new WrapWhatever data constructor
-- and a new showBox destructurer pattern
-- for each new type that we augment the box with.
-- there's got to be a better way!

-- maybe we can get rid of the verbose showBox situation by using typeclasses.
-- let's have a MyBox2 that we derive into the Show typeclass:
data MyBox2 = WrapInt2 Int
            | WrapUnit2 ()
            | WrapBool2 Bool
              deriving (Show)

boxList2 :: [MyBox2]
boxList2 = [WrapUnit2 (), WrapInt2 5, WrapBool2 True]

-- now our utility function can delegate the work to the derived show function.
showBox2 :: MyBox2 -> String
showBox2 x = "we have a whatever: " ++ show x

-- but we are left with the verbosity of the data constructors:
-- each underlying type still needs its own wrapper constructor.
-- is there a better way to abstract away the showability of the underlying thing
-- that we put in the box? if there were such a better way, what would it look like?

-- for inspiration,
-- we recall the fat-arrow syntax of typeclass constraints, also known as contexts:
myshow :: (Show ss) => ss -> String
myshow s = "showing anything: " ++ show s
-- this means:
-- myshow is a function that can consume any data value s that is Showable,
-- ie. (1) the argument to myshow, s, is a member of a type ss
-- and (2) ss is a type which is an instance of the typeclass Show.

-- so that's a good clue; now, wouldn't it be nice if we could use the idea of
-- typeclass constraints in a type definition rather than a function declaration?
-- after all, data constructors are functions too!
--   Wait, what does that mean? We can think of a data constructor as a function:
--     WrapUnit :: ()   -> MyBox
--     WrapInt  :: Int  -> MyBox
--     WrapBool :: Bool -> MyBox
--   (this syntax is not valid Haskell2010, but until we learn about GADTSyntax and GADTs,
--    bear with me and use your imagination to pretend that it is)
-- so we want some way to say, WrapWhatever :: (Show ss) => ss -> MyBox

-- and that's where existential quantification comes in. it allows us to say:
-- for any type ss that is Showable,
-- ie. ss is a type which is an instance of the typeclass Show
-- the data constructor Wrap3 will wrap ss in a MyBox3.
data MyBox3 = forall ss. Show ss => Wrap3 ss
-- you can think of this as syntactic sugar for the above construction,
--   WrapWhatever :: (Show ss) => ss -> MyBox

-- now we can use the same data constructor for different underlying types.
boxList3 :: [MyBox3]
boxList3 = [Wrap3 (), Wrap3 5, Wrap3 True]

-- and the utility function works for all of them.
showBox3 :: MyBox3 -> String
showBox3 (Wrap3 whatever) = "we have a whatever: " ++ show whatever

main = do
  putStrLn "the old way:"; 
  mapM_ (putStrLn . ("  "++) . showBox) boxList

  putStrLn "the typeclassy way:"
  mapM_ (putStrLn . ("  "++) . showBox2) boxList2

  putStrLn "the existential quantification way:"
  mapM_ (putStrLn . ("  "++) . showBox3) boxList3
  

