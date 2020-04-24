import Test.HUnit
import Train
import System.Exit

shortTrain :: Train Int
shortTrain = Car 1 Engine

emptyTrain :: Train Int
emptyTrain = Engine

testLen0 = let
  size = len emptyTrain
  in TestCase (assertEqual "len should return 0 when there's only an Engine" size 0)

testLen1 = let
  size = len shortTrain
  in TestCase (assertEqual "len should return train's length" size 1)

testPush = let
  newTrain = push shortTrain 2
  in TestCase (assertEqual "push should add new Car with a value, just before the Engine" newTrain (Car 1 (Car 2 Engine)))

testAttach = let
  newTrain = attach shortTrain 42
  in TestCase (assertEqual "attach should add new Car with a value at the end of the train" newTrain (Car 42 (Car 1 Engine)))

testRev0 = let
  newTrain = rev emptyTrain
  in TestCase (assertEqual "reverse an empty train (Engine only)" newTrain emptyTrain)

testRev1 = let
  newTrain = rev shortTrain
  in TestCase (assertEqual "reverse a single-car train" newTrain shortTrain)

tests = TestList [
    TestLabel "train length" testLen0,
    TestLabel "train length" testLen1,
    TestLabel "push a Car" testPush,
    TestLabel "attach a Car" testAttach,
    TestLabel "reverse a train" testRev0,
    TestLabel "reverse a train" testRev1
  ]

main :: IO ()
main = do
  results <- runTestTT $ tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
