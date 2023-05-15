# Chess Data Tool

**Description**

The Chess Data Tool is a graphical user interface (GUI) application written in Haskell that allows users to download and compile chess game data into a single Portable Game Notation (.pgn) file. The tool provides two main functionalities:

- Chess.com Games Download: Users can specify the username of a player registered on chess.com and download all of their chess games. The tool will fetch the games and transform them into a single .pgn file. Chesscom website currently misses this feature, there is currently a button for downloading only 50 games. 

- The Week in Chess (TWIC) Games Download: Users can download chess game files from The Week in Chess website and compile them into a single .pgn database. TWIC is a popular website that provides weekly updates on chess events and tournaments around the world. Otherwise user has to manually download.zip files with pgn. files and compile them manually. 

Chess.com and The Week in Chess (TWIC) are widely recognized as the most frequently updated websites, providing users with access to the most recent chess games from various tournaments and events. 
The Chess Data Tool simplifies the process of gathering and organizing chess game data for further analysis or study. Practical players recognize the importance of studying their opponent's games before a match, as it allows them to prepare strategically and be better equipped for the upcoming game. By analyzing their opponent's playing style, strengths, and weaknesses, players can devise effective strategies and make informed decisions during the game. This thorough preparation increases their chances of success and enables them to approach the match with confidence.


The tool is designed to be user-friendly and accessible to individuals without any programming knowledge.

**Features**

- Download chess games from chess.com for a specified player and create a .pgn database.
- Download chess game files from The Week in Chess website and compile them into a single .pgn database.
- Interactive graphical user interface (GUI).
- Progress indicators during the download process.
- Error handling for invalid inputs and network connectivity issues.