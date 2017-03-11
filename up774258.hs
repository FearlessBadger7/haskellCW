--
-- MATHFUN
-- UP774258

import Data.List

                -- title director year fans
data Film = Film String String Int [String]
    deriving (Read, Eq, Show)

testDatabase :: [Film]
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Sam", "Olga", "Tim"]
  , Film "The Fly" "David Cronenberg" 1986 ["Garry", "Dave", "Zoe", "Kevin", "Emma"]
  , Film "Body Of Lies" "Ridley Scott" 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"]
  , Film "Avatar" "James Cameron" 2009 ["Dave", "Amy", "Liz"]
  , Film "Titanic" "James Cameron" 1997 ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"]
  , Film "The Departed" "Martin Scorsese" 2006 ["Wally", "Liz", "Kevin", "Tim", "Emma"]
  , Film "Aliens" "Ridley Scott" 1986 ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"]
  , Film "Kingdom Of Heaven" "Ridley Scott" 2005 ["Jo", "Wally", "Emma"]
  , Film "Prometheus" "Ridley Scott" 2012 ["Kevin", "Tim", "Emma", "Jo", "Liz"]
  , Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 ["Dave", "Amy", "Garry", "Ian", "Neal"]
  , Film "Bridge of Spies" "Steven Spielberg" 2015 ["Wally", "Sam", "Dave", "Neal"]
  , Film "Jaws" "Steven Spielberg" 1975 ["Dave", "Jo", "Zoe", "Wally", "Emma", "Kate"]
  , Film "The Martian" "Ridley Scott" 2015 ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"]
  , Film "The BFG" "Steven Spielberg" 2016 ["Sam", "Wally", "Dave", "Jo", "Kate"]
  , Film "The Shawshank Redemption" "Frank Darabont" 1994 ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe"]
  , Film "Gladiator" "Ridley Scott" 2000 ["Olga", "Neal", "Kate", "Heidi", "Bill", "Sam", "Zoe"]
  , Film "The Green Mile" "Frank Darabont" 1999 ["Kevin", "Tim", "Emma", "Heidi"]
  , Film "True Lies" "James Cameron" 1994 ["Sam", "Dave"]
  , Film "Super 8" "J J Abrams" 2011 ["Kevin", "Tim", "Emma", "Olga", "Heidi"]
  , Film "Minority Report" "Steven Spielberg" 2002 ["Kevin", "Kate", "Tim", "Emma", "Olga", "Jenny", "Zoe"]
  , Film "War Horse" "Steven Spielberg" 2011 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"]
  , Film "Silence" "Martin Scorsese" 2016 ["Wally", "Emma", "Tim", "Heidi", "Bill", "Olga", "Jo"]
  , Film "The Terminal" "Steven Spielberg" 2004 ["Kate", "Dave", "Jo", "Wally", "Emma"]
  , Film "Star Wars: The Force Awakens" "J J Abrams" 2015 ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz", "Jo"]
  , Film "Hugo" "Martin Scorsese" 2011 ["Wally", "Sam"]]

-- Functional code
addFilm :: [Film] -> String -> String -> Int -> [String] -> [Film]
addFilm database t d y f = database ++ [(Film t d y f)]

filmsAsString :: [Film] -> Bool -> String -- true to show fan names false to show number of fans
filmsAsString [x] True = filmToStringFans x ++ "\n"
filmsAsString (x:xs) True = (filmToStringFans x) ++ "\n\n" ++ (filmsAsString xs True)
filmsAsString [x] _ = filmToString x ++ "\n"
filmsAsString (x:xs) _ = (filmToString x) ++ "\n\n" ++ (filmsAsString xs False)

filmToString :: Film -> String -- fan list on newline so easier to read
filmToString (Film t d y f) = t ++ " " ++ d ++ " " ++ (show y) ++ " "++ show (length f)

filmToStringFans :: Film -> String -- fan list on newline so easier to read
filmToStringFans (Film t d y f) = t ++ " -- " ++ d ++ ", " ++ (show y) ++ "\n" ++ fansToString f

fansToString :: [String] -> String
fansToString []  = []
fansToString [x] = x -- not needed but removes , from end of list
fansToString (x:xs) =  x ++ ", " ++ (fansToString xs)

findFilmsAfterYear :: [Film] -> Int -> [Film]
findFilmsAfterYear (x:xs) year = [ x | x <- xs, (afterDateTest x year) ]

afterDateTest :: Film -> Int -> Bool
afterDateTest (Film t d y f) year
    | y > year      = True
    | otherwise     = False

findFilmsByFanName :: [Film] -> String -> [Film]
findFilmsByFanName (x:xs) name = [ x | x <- xs, (areTheyAFan x name)]

areTheyAFan :: Film -> String -> Bool
areTheyAFan (Film t d y f) name
    | elem name f   = True
    | otherwise     = False

getFansOfFilm :: [Film] -> String -> [String]
getFansOfFilm films title = fans
    where
      film = findFilm films title
      fans = getFans film

findFilm :: [Film] -> String -> Film
findFilm (x:xs) title
    | sameFilm x title  = x
    | xs == []          = Film "" "" 0 []
    | otherwise         = findFilm xs title

sameFilm :: Film -> String -> Bool
sameFilm (Film t d y f) title
    | t == title    = True
    | otherwise     = False

getFans :: Film -> [String]
getFans (Film t d y f) = f

addFanToFilm :: [Film] -> String -> String -> [Film]
addFanToFilm (x:xs) title fan =
  map (\x -> if (sameFilm x title) then (addFans film fan) else x) xs
    where
      film = findFilm (x:xs) title

addFans :: Film -> String -> Film
addFans (Film t d y f) fan
    | elem fan f    = Film t d y f --if already a fan, fan list is unaltered
    | otherwise     = Film t d y fans
    where
      fans = f ++ [fan]

fansOfDirector :: [Film] -> String -> [String]
fansOfDirector films direct = nub (allFans directorsFilms)
  where
    directorsFilms = getFilmsbyDirector films direct

getFilmsbyDirector :: [Film] -> String -> [Film]
getFilmsbyDirector (x:xs) direct = [ x | x <- xs, (sameDirector x direct)]

sameDirector :: Film -> String -> Bool
sameDirector (Film t d y f) direct
    | d == direct    = True
    | otherwise     = False

allFans :: [Film] -> [String]
allFans (x:xs)
    | xs == []    = getFans x
    | otherwise   = (getFans x) ++ (allFans xs )
  where
    (Film t d y f) = x

directorsByFan :: [Film] -> String -> [String] --list of directors with repeats
directorsByFan (x:xs) fan = allDirectors [ x | x <- xs, (areTheyAFan x fan)]

allDirectors :: [Film] -> [String]
allDirectors (x:xs)
    | xs == []    = [d]
    | otherwise   = d : (allDirectors xs )
  where
    (Film t d y f) = x

count :: Eq a => a -> [a] -> Int
count n [] = 0
count n (x:xs)
    | n == x = 1 + count n xs
    | otherwise = count n xs

freq :: Eq a => [a] -> [(a, Int)]
freq [] = []
freq (x:xs) = [(x, count x (x:xs))] ++ freq (filter (/= x) xs)

lol :: [(String, Int)] -> String
lol (x:xs)
    | xs == []   = d ++ " " ++ (show c)
    | otherwise  = d ++ " " ++ (show c) ++ "\n" ++ lol xs
  where
    (d,c) = x

directLikes :: [Film] -> String -> String
directLikes films fan = lol (freq (directorsByFan films fan))



-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
demo 1  = putStrLn (filmsAsString (addFilm testDatabase "Alien: Covenant" "Ridley Scott" 2017 []) True)
demo 2  = putStrLn (filmsAsString testDatabase False)
demo 3  = putStrLn (filmsAsString (findFilmsAfterYear testDatabase 2008) False)
demo 4  = putStrLn (filmsAsString (findFilmsByFanName testDatabase "Liz") False)
demo 5  = putStrLn (fansToString (getFansOfFilm testDatabase "Jaws"))
demo 6  = putStrLn (filmsAsString (addFanToFilm testDatabase "The Fly" "Liz") True)
demo 66 = putStrLn (filmsAsString (addFanToFilm testDatabase "Avatar" "Liz") True)
demo 7  = putStrLn (fansToString (fansOfDirector testDatabase "James Cameron"))
demo 8  = putStrLn (directLikes testDatabase "Liz")

-------------------------------------------------------------------------------------------------
-- User interface

main :: IO ()
main = do
    name <- getName
    db <- readFilmFile
    showMenu db name
    return ()

getName :: IO String
getName = do
    putStrLn "Please enter your name:"
    respone <- getLine
    return respone


readFilmFile :: IO [Film]
readFilmFile = do
    file <- readFile "films.txt"
    return (read file :: [Film])

showMenu :: [Film] -> String -> IO ()
showMenu films name = do
  putStrLn "\n"
  putStrLn ("Hello "++ name ++ ". Enter an option number.")
  putStrLn "==========================================="
  putStrLn "1. Show all films"
  putStrLn "9. Exit\n"
  putStr (filmsAsString films True)





-- getdata file
-- showall films
-- ask user'sname
-- show menu
-- allow users to modify variable
-- exit
-- save file
--
-- Your user interface code goes here
--
--
