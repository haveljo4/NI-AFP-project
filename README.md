# Chess.com and TWIC games downloader

**Download**
Latest release: https://github.com/haveljo4/chess-games-download/releases/latest

Stable release: https://github.com/haveljo4/chess-games-download/releases/tag/120

**Usage** 
Tutorial can be found in the https://github.com/haveljo4/chess-games-download/blob/main/doc/Tutorial.pdf

The tutorial is for the release 120

**Description**

The Chess Data Tool is a graphical user interface (GUI) application written in Haskell that allows users to download and compile chess game data into a single Portable Game Notation (.pgn) file. The tool provides two main functionalities:

- Chess.com Games Download: Users can specify the username of a player registered on chess.com and download all of their chess games. The tool will fetch the games and transform them into a single .pgn file. Chesscom website currently misses this feature, there is currently a button for downloading only 50 games.

- The Week in Chess (TWIC) Games Download: Users can download chess game files from The Week in Chess website and compile them into a single .pgn database. TWIC is a popular website that provides weekly updates on chess events and tournaments around the world. Otherwise user has to manually download.zip files with pgn. files and compile them manually.

Chess.com and The Week in Chess (TWIC) are widely recognized as the most frequently updated websites, providing users with access to the most recent chess games from various tournaments and events.
The Chess Data Tool simplifies the process of gathering and organizing chess game data for further analysis or study. Practical players recognize the importance of studying their opponent's games before a match, as it allows them to prepare strategically and be better equipped for the upcoming game. By analyzing their opponent's playing style, strengths, and weaknesses, players can devise effective strategies and make informed decisions during the game. This thorough preparation increases their chances of success and enables them to approach the match with confidence.


The tool is designed to be user-friendly and accessible to individuals without any programming knowledge. 

**Features**

- Download chess games from chess.com for a specified player and create and compile them into a single .pgn database.
- Download chess game files from The Week in Chess website and compile them into a single .pgn database.
- Interactive graphical user interface (GUI).

**Requirements for build**

***Windows***
The whole building process can be also found in the github action workflow under the .github directory. 

It is important to install msys2 https://www.msys2.org/.
Then install autoconf with version 2.69

    wget http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
    tar xvfvz autoconf-2.69.tar.gz
    cd autoconf-2.69
    ./configure
    make
    make install


Then when you want to add some extra-deps, it is important to run: stack install --flag fltkhs:bundled
But it takes quite a lot of time


**Relevant sources**
The chess.com api documentation is here.
https://www.chess.com/news/view/published-data-api#pubapi-endpoint-player

The pdf file with the api can be found in the "doc" folder. 

Twic
curl is like this:
curl "https://theweekinchess.com/zips/twic${i}g.zip"

where ${i} is number of the zip file
