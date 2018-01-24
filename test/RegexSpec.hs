module RegexSpec
  ( spec
  ) where

import Regex
import Test.Hspec
import Types

spec :: Spec
spec =
  describe "The str2re function" $ do
    it "should create a Regex from \"a\"" $
      str2re "a" `shouldBe` Just (PrimitiveRE 'a')
    it "should create a Regex from \"a*\"" $
      str2re "a*" `shouldBe` Just (ClosureRE (PrimitiveRE 'a'))
    it "should create a Regex from \"ab\"" $
      str2re "ab" `shouldBe`
      Just (ConcatenatedRE (PrimitiveRE 'a') (PrimitiveRE 'b'))
    it "should create a Regex from \"ab*\"" $
      str2re "ab*" `shouldBe`
      Just (ConcatenatedRE (PrimitiveRE 'a') (ClosureRE (PrimitiveRE 'b')))
    it "should create a Regex from \"(ab)*\"" $
      str2re "(ab)*" `shouldBe`
      Just (ClosureRE (ConcatenatedRE (PrimitiveRE 'a') (PrimitiveRE 'b')))
    it "should create a Regex from \"a|b\"" $
      str2re "a|b" `shouldBe`
      Just (AlternativeRE (PrimitiveRE 'a') (PrimitiveRE 'b'))
    it "should create a Regex from \"ac|b\"" $
      str2re "ac|b" `shouldBe`
      Just
        (AlternativeRE
           (ConcatenatedRE (PrimitiveRE 'a') (PrimitiveRE 'c'))
           (PrimitiveRE 'b'))
    it "should create a Regex from \"a|b*\"" $
      str2re "a|b*" `shouldBe`
      Just (AlternativeRE (PrimitiveRE 'a') (ClosureRE (PrimitiveRE 'b')))
    it "should create a Regex from \"(a|b)*\"" $
      str2re "(a|b)*" `shouldBe`
      Just (ClosureRE (AlternativeRE (PrimitiveRE 'a') (PrimitiveRE 'b')))
    it "should create a Regex from \"(aa*a|(c|d)*b)*e\"" $
      str2re "(aa*a|(c|d)*b)*e" `shouldBe`
      Just
        (ConcatenatedRE
           (ClosureRE
              (AlternativeRE
                 (ConcatenatedRE
                    (PrimitiveRE 'a')
                    (ConcatenatedRE
                       (ClosureRE (PrimitiveRE 'a'))
                       (PrimitiveRE 'a')))
                 (ConcatenatedRE
                    (ClosureRE
                       (AlternativeRE (PrimitiveRE 'c') (PrimitiveRE 'd')))
                    (PrimitiveRE 'b'))))
           (PrimitiveRE 'e'))
    it "should create a Regex from \"a(c*|bd)*bd|b*e\"" $
      str2re "a(c*|bd)*bd|b*e" `shouldBe`
      Just
        (AlternativeRE
           (ConcatenatedRE
              (PrimitiveRE 'a')
              (ConcatenatedRE
                 (ClosureRE
                    (AlternativeRE
                       (ClosureRE (PrimitiveRE 'c'))
                       (ConcatenatedRE (PrimitiveRE 'b') (PrimitiveRE 'd'))))
                 (ConcatenatedRE (PrimitiveRE 'b') (PrimitiveRE 'd'))))
           (ConcatenatedRE (ClosureRE (PrimitiveRE 'b')) (PrimitiveRE 'e')))
