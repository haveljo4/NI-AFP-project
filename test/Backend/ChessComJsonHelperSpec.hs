module Backend.ChessComJsonHelperSpec (spec) where
import Test.Hspec
import qualified Backend.ChessComJsonHelper as JsonHelper 

spec :: Spec
spec = do
  describe "ChessComJsonHelper tests" $ do
    it "extractYearAndMonthFromURL" $ do
      JsonHelper.extractYearAndMonthFromURL "https://api.chess.com/pub/player/somePlayerName/games/2015/08" `shouldBe` Right (2015, 8)  
      JsonHelper.extractYearAndMonthFromURL "https://api.chess.com/pub/player/#&#^(*(/games/2015/12" `shouldBe` Right (2015, 12)  
      JsonHelper.extractYearAndMonthFromURL "https://api.chess.com/pub/player/ mn *& 93 *+/games/2016/-5" `shouldBe` Right (2016, -5)  
    it "filterArchives" $ do
      JsonHelper.filterArchives 2015 1 2016 6 jsonString  `shouldBe` Right (filteredValidUrls, invalidUrls )
        where
          jsonString :: String 
          jsonString =     "{ \"archives\": ["
                           ++ "\"https://api.chess.com/pub/player/somePlayerName/games/2009/04\","
                           ++ "\"https://api.chess.com/pub/player/somePlayerName/games/2015/05\","
                           ++ "\"https://api.chess.com/pub/player/somePlayerName/games/2015/06\","
                           ++ "\"https://api.chess.com/pub/player/somePlayerName/games/2023/06\","
                           ++ "\"mn *& 93 *+/games/2017/13\""
                           ++ "]}"
          filteredValidUrls = ["https://api.chess.com/pub/player/somePlayerName/games/2015/05",
                           "https://api.chess.com/pub/player/somePlayerName/games/2015/06"]
          invalidUrls = ["mn *& 93 *+/games/2017/13"]

