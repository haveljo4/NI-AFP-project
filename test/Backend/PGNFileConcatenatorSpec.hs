module Backend.PGNFileConcatenatorSpec (spec) where

import Test.Hspec
import Data.Text (pack, replace, unpack)
import System.Directory (removeFile, doesFileExist)
import qualified Backend.PGNFileConcatenator as PGNConcatenator

spec :: Spec
spec = do
  describe "PGNFileConcatenator tests" $ do
    it "concatenates PGN files in a folder" $ do
      let folderPath = "test/ResourcesTmp/pgns"
          outputFilePath = "test/ResourcesTmp/pgns/All_output.pgn"
      
      -- Process the folder with PGN files
      PGNConcatenator.processFolderWithPGNs folderPath outputFilePath

      -- Check if the output file was created
      outputFileExists <- doesFileExist outputFilePath
      outputFileExists `shouldBe` True
      
      -- Read the actual content from the output file
      actualContent <- readFile outputFilePath

      actualContent `shouldBe` expectedContent
      
      -- Clean up the output file after the test
      removeFile outputFilePath

expectedContent = "[Event \"Vergani Cup 2023  Master Group\"]\n[Site \"Bassano del Grappa Hotel Palladio ****\"]\n[Date \"2023.02.01\"]\n[Round \"1\"]\n[Board \"3\"]\n[White \"Urbani, Gabriel\"]\n[Black \"Sebenik, Matej\"]\n[Result \"0-1\"]\n[ECO \"C02\"]\n[WhiteElo \"2166\"]\n[BlackElo \"2540\"]\n[PlyCount \"120\"]\n[EventDate \"2023.02.01\"]\n[EventRounds \"9\"]\n[EventCountry \"ITA\"]\n\n1. e4 e6 2. d4 d5 3. e5 c5 4. c3 Nc6 5. Nf3 Bd7 6. Be2 Nge7 7. a3 Rc8 8. b4 cxd4\n9. cxd4 Nf5 10. O-O Be7 11. Bb2 O-O 12. Nc3 f6 13. Rc1 Be8 14. h3 a6 15. Re1 fxe5\n16. dxe5 Bg6 17. Na4 Bg5 18. Ra1 Bh6 19. Nc5 Qe7 20. Bc1 a5 21. Bxh6 Nxh6 22. Nd4\nNxd4 23. Qxd4 Nf5 24. Qd2 Nh4 25. Bg4 Bf5 26. Rac1 axb4 27. axb4 b6 28. Nb3 Rxc1\n29. Rxc1 Bxg4 30. hxg4 Ng6 31. Nd4 Nxe5 32. Re1 Nc4 33. Qe2 e5 34. Nc6 Qf6 35. Nxe5\nNxe5 36. Qxe5 Qxf2+ 37. Kh1 Kh8 38. Rd1 Qh4+ 39. Kg1 Qxg4 40. Rd4 Qf5 41. Qxd5 h5\n42. Qxf5 Rxf5 43. Kh2 Rg5 44. Kg1 Kg8 45. Kf2 Kf7 46. Rd6 Rb5 47. Rd4 Ke6 48. Ke3\nRe5+ 49. Kf3 g5 50. Rd8 Rb5 51. Rd4 Rd5 52. Re4+ Kf5 53. Rc4 Rd3+ 54. Ke2 Rb3 55.\nKf2 Ke5 56. g3 Kd5 57. Rc7 Rb2+ 58. Ke3 Rxb4 59. Rh7 h4 60. Rh5 Rb3+  0-1\n\n[Event \"Vergani Cup 2023  Master Group\"]\n[Site \"Bassano del Grappa Hotel Palladio ****\"]\n[Date \"2023.02.01\"]\n[Round \"1\"]\n[Board \"4\"]\n[White \"Nasuta, Grzegorz\"]\n[Black \"Chkhaidze, Nikoloz\"]\n[Result \"1-0\"]\n[ECO \"B44\"]\n[WhiteElo \"2535\"]\n[BlackElo \"2165\"]\n[PlyCount \"113\"]\n[EventDate \"2023.02.01\"]\n[EventRounds \"9\"]\n[EventCountry \"ITA\"]\n\n1. e4 c5 2. Nf3 e6 3. d4 cxd4 4. Nxd4 Nc6 5. Bf4 d6 6. Nxc6 bxc6 7. Bd3 Rb8 8. Nd2\nNe7 9. O-O Ng6 10. Be3 Qc7 11. f4 Be7 12. Rb1 O-O 13. c3 e5 14. f5 Nf4 15. Bxf4 exf4\n16. Rxf4 Re8 17. Rf1 Bb7 18. Qc2 Bf6 19. Rbe1 Re7 20. Nf3 Rbe8 21. Re2 c5 22. c4\nBc6 23. g4 Qb7 24. Rfe1 Be5 25. Qd2 f6 26. Kg2 g5 27. fxg6 hxg6 28. Rf1 Kg7 29. g5\nfxg5 30. Nxg5 Bf6 31. Rxf6 Kxf6 32. Rf2+ Kg7 33. Qf4 Bxe4+ 34. Bxe4 Rxe4 35. Qf6+\nKh6 36. Nf7+ Kh7 37. Ng5+ Kh6 38. Nf7+ Kh7 39. Nxd6 Qc6 40. Qf7+ Kh6 41. Qd5 Qxd5\n42. cxd5 Kg5 43. Nxe4+ Rxe4 44. Rd2 Re7 45. d6 Rd7 46. Kf3 Kf5 47. h4 c4 48. a4 a5\n49. Rd5+ Ke6 50. Ke4 c3 51. bxc3 Rxd6 52. Rxd6+ Kxd6 53. Kd4 Ke6 54. c4 Kd6 55. c5+\nKc7 56. Kd5 Kd7 57. c6+  1-0[Event \"Vergani Cup 2023  Master Group\"]\n[Site \"Bassano del Grappa Hotel Palladio ****\"]\n[Date \"2023.02.01\"]\n[Round \"1\"]\n[Board \"1\"]\n[White \"Andreev, Konstantin\"]\n[Black \"Pultinevicius, Paulius\"]\n[Result \"0-1\"]\n[ECO \"B12\"]\n[WhiteElo \"2186\"]\n[BlackElo \"2559\"]\n[PlyCount \"112\"]\n[EventDate \"2023.02.01\"]\n[EventRounds \"9\"]\n[EventCountry \"ITA\"]\n\n1. e4 c6 2. d4 d5 3. e5 Bf5 4. Nf3 e6 5. Be2 Nd7 6. O-O Bg6 7. Nbd2 c5 8. dxc5 Bxc5\n9. c4 Ne7 10. Nb3 dxc4 11. Nxc5 Nxc5 12. Bxc4 h6 13. Qe2 Bd3 14. Bxd3 Qxd3 15. Re1\nNc6 16. Bd2 Qxe2 17. Rxe2 Nd3 18. Bc3 O-O-O 19. Rd1 Nf4 20. Rxd8+ Rxd8 21. Rd2 Rxd2\n22. Bxd2 Nd3 23. Bc3 b5 24. Nd4 Nxd4 25. Bxd4 Kb7 26. Kf1 a5 27. a3 Kc6 28. Ke2 Nf4+\n29. Kf3 Kd5 30. Bc3 Nd3 31. Bxa5 Nxb2 32. Bb4 h5 33. Bf8 g6 34. Kf4 Nd3+ 35. Kg5\nNxe5 36. f3 Nd7 37. Bb4 e5 38. g4 hxg4 39. fxg4 Ke6 40. h4 Nf6 41. h5 Ne4+ 42. Kh6\nNf2 43. Kg5 Nh3+ 44. Kh4 Ng1 45. Kg5 Nf3+ 46. Kh6 Kf6 47. Bc3 Nh2 48. g5+ Kf5 49.\nKg7 gxh5 50. Kxf7 Ng4 51. g6 h4 52. g7 h3 53. Bd2 Nf6 54. Be1 h2 55. Bh4 h1=Q 56.\nBxf6 Qd5+  0-1\n\n[Event \"Vergani Cup 2023  Master Group\"]\n[Site \"Bassano del Grappa Hotel Palladio ****\"]\n[Date \"2023.02.01\"]\n[Round \"1\"]\n[Board \"2\"]\n[White \"Sonis, Francesco\"]\n[Black \"Kalosha, Zhiulien Oleksii\"]\n[Result \"1-0\"]\n[ECO \"D30\"]\n[WhiteElo \"2558\"]\n[BlackElo \"2170\"]\n[PlyCount \"77\"]\n[EventDate \"2023.02.01\"]\n[EventRounds \"9\"]\n[EventCountry \"ITA\"]\n\n1. d4 d5 2. c4 e6 3. Nf3 c6 4. e3 Nf6 5. Bd3 Nbd7 6. b3 Bb4+ 7. Bd2 Bxd2+ 8. Nbxd2\nO-O 9. O-O Qe7 10. Qc2 h6 11. Rfe1 Rd8 12. e4 dxe4 13. Bxe4 Nxe4 14. Nxe4 Nf6 15.\nc5 Nxe4 16. Qxe4 b6 17. b4 Rd5 18. Ne5 Bd7 19. Nc4 Rb8 20. Ne3 Rh5 21. g3 Qf6 22.\nNg4 Qf5 23. Qxf5 Rxf5 24. Ne3 Rf6 25. Rab1 Be8 26. h4 Rd8 27. Red1 h5 28. a4 Kf8\n29. Nc4 Rb8 30. cxb6 axb6 31. a5 bxa5 32. bxa5 Ra8 33. Rb6 Rf5 34. Ra1 Rb5 35. a6\nRa7 36. Ra5 Rxa5 37. Nxa5 Bd7 38. Nb3 Ke7 39. Rb7  1-0\n"
