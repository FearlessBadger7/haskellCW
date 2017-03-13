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

----------------------------------------------------------------------------------------------------
-- Functional code

-- adds new film to the list returns the new film
addFilm :: [Film] -> String -> String -> Int -> [String] -> [Film]
addFilm database t d y f = database ++ [(Film t d y f)]

-- converts a list of films to a String
-- True to show fan names, False to show number of fans
filmsAsString :: [Film] -> Bool -> String
filmsAsString [] _ = "Sorry there are no results for that."
filmsAsString [x] True = filmToStringFans x ++ "\n"
filmsAsString (x:xs) True = (filmToStringFans x) ++ "\n\n" ++ (filmsAsString xs True)
filmsAsString [x] _ = filmToString x ++ "\n"
filmsAsString (x:xs) _ = (filmToString x) ++ "\n\n" ++ (filmsAsString xs False)

-- converts a Film to a string showing th number of fans
filmToString :: Film -> String -- fan num on newline so easier to read
filmToString (Film t d y f) = t ++ " " ++ d ++ " " ++ (show y) ++ "\nNumber of Fans: "++ show (length f)

-- converts a Film to a String showing a list of fans
filmToStringFans :: Film -> String -- fan list on newline so easier to read
filmToStringFans (Film t d y f) = t ++ " -- " ++ d ++ ", " ++ (show y) ++ "\n" ++ fansToString f

-- converts a list of strings to a string
fansToString :: [String] -> String
fansToString []  = "Sorry there are no results for that."
fansToString [x] = x -- not needed but removes , from end of list
fansToString (x:xs) =  x ++ ", " ++ (fansToString xs)

-- returns all the films after but not including a particular year
findFilmsAfterYear :: [Film] -> Int -> [Film]
findFilmsAfterYear (x:xs) year = [ x | x <- xs, (afterDateTest x year) ]

-- returns if a film is after a particular year
afterDateTest :: Film -> Int -> Bool
afterDateTest (Film t d y f) year
    | y > year      = True
    | otherwise     = False

-- returns list of films who have a particular fan in their fan list
findFilmsByFanName :: [Film] -> String -> [Film]
findFilmsByFanName (x:xs) name = [ x | x <- xs, (areTheyAFan x name)]

-- returns if a person is a fan of a film or not
areTheyAFan :: Film -> String -> Bool
areTheyAFan (Film t d y f) name
    | elem name f   = True
    | otherwise     = False

-- returns all the fans of a particular film
getFansOfFilm :: [Film] -> String -> [String]
getFansOfFilm films title = fans
    where
      film = findFilm films title
      fans = getFans film

-- returns the searched for film title as its id tag
findFilm :: [Film] -> String -> Film
findFilm (x:xs) title
    | sameFilm x title  = x
    | xs == []          = Film "" "" 0 []
    | otherwise         = findFilm xs title

-- returns if a film matches the given title or not
sameFilm :: Film -> String -> Bool
sameFilm (Film t d y f) title
    | t == title    = True
    | otherwise     = False

-- returns a list of fans for a film
getFans :: Film -> [String]
getFans (Film t d y f) = f

-- adds a fan to a film returns new list of films
addFanToFilm :: [Film] -> String -> String -> [Film]
addFanToFilm (x:xs) title fan =
  map (\x -> if (sameFilm x title) then (addFans film fan) else x) xs
    where
      film = findFilm (x:xs) title

-- adds a fan to a film returns new list of films
addFans :: Film -> String -> Film
addFans (Film t d y f) fan
    | elem fan f    = Film t d y f --if already a fan, fan list is unaltered
    | otherwise     = Film t d y fans
    where
      fans = f ++ [fan]

-- returns all the fans of a particular director
fansOfDirector :: [Film] -> String -> [String]
fansOfDirector films direct
    | films == []  = []
    | otherwise    = nub (allFans directorsFilms)
  where
    directorsFilms = getFilmsbyDirector films direct

-- returns list of films by a particular director
getFilmsbyDirector :: [Film] -> String -> [Film]
getFilmsbyDirector (x:xs) direct = [ x | x <- xs, (sameDirector x direct)]

-- returns if a film has the same director as the provided director
sameDirector :: Film -> String -> Bool
sameDirector (Film t d y f) direct
    | d == direct    = True
    | otherwise     = False

-- returns all the fans within a list of films (including repeats)
allFans :: [Film] -> [String]
allFans []     = []
allFans [x] = getFans x
allFans (x:xs) = getFans x ++ (allFans xs)

-- returns a list of all the directors who have a particular fan (including repeats)
directorsByFan :: [Film] -> String -> [String] --list of directors with repeats
directorsByFan (x:xs) fan = allDirectors [ x | x <- xs, (areTheyAFan x fan)]

-- returns a list of all the directors (including repeats)
allDirectors :: [Film] -> [String]
allDirectors []     = []
allDirectors (x:xs)
    | (x:xs) == [x] = [d]
    | otherwise = [d] ++ (allDirectors xs)
  where
    (Film t d y f) = x

-- counts the number of times a particular item appears in a list
count :: Eq a => a -> [a] -> Int
count n [] = 0
count n (x:xs)
    | n == x = 1 + count n xs
    | otherwise = count n xs

-- counts the number of times all item appears in a list
freq :: Eq a => [a] -> [(a, Int)]
freq [] = []
freq (x:xs) = [(x, count x (x:xs))] ++ freq (filter (/= x) xs)

-- displays the list of tupes string, int as a string
displyFavDirects :: [(String, Int)] -> String
displyFavDirects []  = "Sorry there are no results for that."
displyFavDirects (x:xs)
    | xs == []   = "You like " ++ (show c) ++ " films by " ++ d
    | otherwise  = "You like " ++ (show c) ++ " films by " ++ d ++ "\n" ++ displyFavDirects xs
  where
    (d,c) = x

-- displays all the directors that a particular fan likes
-- and the number of films of theirs they like
directLikes :: [Film] -> String -> String
directLikes films fan = displyFavDirects (freq (directorsByFan films fan))

-- returns if a film appears in list of films or not
filmExits :: [Film] -> String -> Bool
filmExits (x:xs) title
    | xs == []          = sameFilm x title --last item in list
    | sameFilm x title  = True
    | otherwise         = filmExits xs title

-- returns if a a value is an Int or not
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

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

----------------------------------------------------------------------------------------------------
-- User interface

main :: IO ()
main = do
  db <- readFilm
  putStr (filmsAsString db True)
  putStrLn "\n"
  name <- getName
  getNameIf name
  showMenu db name
  return ()

getNameIf :: String -> IO ()
getNameIf name = do
  if (name == "")
    then do
      name <- getName
      getNameIf name
    else do
      return ()

getName :: IO String
getName = do
  putStr "Please enter your name: "
  respone <- getLine
  return respone

-- reads from file films.txt
readFilm :: IO [Film]
readFilm = do
  file <- readFile "films.txt"
  return (read file :: [Film])

-- writes to file films.txt and exits program
writeToFilms :: [Film] -> IO ()
writeToFilms films = do
  putStrLn "Saving..."
  writeFile "films.txt" ""
  writeFile "films.txt" (show films)
  putStrLn "Done"
  return ()

-- displays the menu to the users and gets their input option for
showMenu :: [Film] -> String -> IO ()
showMenu films name = do
  putStrLn "Press enter to continue."
  fake <- getLine -- allows users to read error msg and result before displaying menu
  putStrLn ""
  putStrLn ("Hello "++ name ++ ", enter an option number.")
  putStrLn "==========================================="
  putStrLn "1. Show all films"
  putStrLn "2. Add a film to the database"
  putStrLn "3. Show all the films, after a chosen year"
  putStrLn "4. Show all the films I'm a fan of"
  putStrLn "5. Show all the fans of a chosen film"
  putStrLn "6. Add me as a fan, to a chosen film"
  putStrLn "7. Show all the fans of a chosen director"
  putStrLn "8. Show all directors I'm a fan of"
  putStrLn "9. Exit"
  putStrLn ""
  putStr "I choose you, option number "

  respone <- getLine
  case respone of
    "1" -> option1 films name
    "2" -> option2 films name
    "3" -> option3 films name
    "4" -> option4 films name
    "5" -> option5 films name
    "6" -> option6 films name
    "7" -> option7 films name
    "8" -> option8 films name
    "9" -> do
      writeToFilms films
      return ()
    _ -> do
      putStrLn ("Invalid input")
      showMenu films name

-- shows all films
option1 :: [Film] -> String -> IO ()
option1 films name = do
  putStrLn "Do you want to see the names of the fans? (y/n)"
  yesNo <- getLine
  case yesNo of
    "y" -> do
      putStrLn (filmsAsString films True)
    "n" -> do
      putStrLn (filmsAsString films False)
    _ -> do
      putStrLn ("Invalid input")
      option1 films name
  showMenu films name

-- add film to the database
option2 :: [Film] -> String -> IO ()
option2 films name = do
  putStrLn "To cancel type '123 back' into the year."
  putStr "Title: "
  t <- getLine
  if (t == "")
    then do
      putStrLn "Invalid input"
      option2 films name
    else do
      if (filmExits films t)
        then do
          putStrLn "This film already exists."
          showMenu films name
        else do
          option2Dir films name t

-- ensure director is not empty
option2Dir :: [Film] -> String -> String -> IO ()
option2Dir films name t = do
  putStr "Director: "
  d <- getLine
  if (d == "")
    then do
      putStrLn "Invalid input"
      option2Dir films name t
    else do
      option2Year films name t d

-- ensures year is valid and if user wants to cancel operation
option2Year :: [Film] -> String -> String -> String -> IO ()
option2Year films name t d = do
  putStr "Year: "
  y <- getLine
  if (isInteger y)
    then do
      let yr = read y :: Int
      if (yr >= 1888)
        then do
          let newFilms = addFilm films t d yr []
          showMenu newFilms name
        else do
          putStrLn "You do know the first movie was released in 1888."
          option2Year films name t d
    else do
      if (y == "123 back")
        then do
          showMenu films name
        else do
          putStrLn "Invalid input"
          option2Year films name t d

-- show all the films, after a chosen year
option3 :: [Film] -> String -> IO ()
option3 films name = do
  putStr "Show films after the year: "
  y <- getLine
  if (isInteger y)
    then do
      let yr = read y :: Int
      putStrLn (filmsAsString (findFilmsAfterYear films yr) False)
      showMenu films name
    else do
      putStrLn "Invalid input for year"
      option3 films name

-- show all the films I'm a fan of
option4 :: [Film] -> String -> IO ()
option4 films name = do
  putStrLn (filmsAsString (findFilmsByFanName films name) False)
  showMenu films name

-- show all the fans of a chosen film
option5 :: [Film] -> String -> IO ()
option5 films name = do
  putStr "Film title: "
  t <- getLine
  let fans = fansToString (getFansOfFilm films t)
  if (fans == "")
    then do
      putStrLn "This film either doesn't here or has no fans."
    else do
      putStrLn fans
  showMenu films name

-- add me as a fan, to a chosen film
option6 :: [Film] -> String -> IO ()
option6 films name = do
  putStrLn "Which film are you a fan of? "
  flm <- getLine
  if (filmExits films flm )
    then do
      let newFilms = addFanToFilm films flm name
      showMenu newFilms name
    else do
      putStrLn "Sorry this film doesn't exist."
      option6YesNo films name

option6YesNo :: [Film] -> String -> IO ()
option6YesNo films name = do
      putStrLn "Would you like to add this film? (y/n)"
      yesNo <- getLine
      case yesNo of
        "y" -> do
          putStrLn "Ok lets add it now."
          option2 films name
        "n" -> do
          putStrLn "Maybe later then."
          showMenu films name
        _ -> do
          putStrLn ("Invalid input")
          option6YesNo films name


-- show all the fans of a chosen director
option7 :: [Film] -> String -> IO ()
option7 films name = do
  putStr "Director name: "
  direct <- getLine
  putStrLn (fansToString (fansOfDirector films direct))
  showMenu films name

-- show all directors I'm a fan of
option8 :: [Film] -> String -> IO ()
option8 films name = do
  putStrLn (directLikes films name)
  showMenu films name
