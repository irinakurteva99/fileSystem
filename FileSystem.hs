import Data.List.Split
import Control.Monad (when, unless)

newtype Name = Name {getName :: String} deriving Show

instance Eq Name where
  (==) (Name first) (Name second) = first == second
 
newtype Content = Content {getContent :: String} deriving Show 

instance Eq Content where
  (==) (Content first) (Content second) = first == second

data FileNode = File 
  { getFileNodeName :: Name,
    getFileNodeContent :: Content } | 
                Folder 
  { getFileNodeName :: Name,
    getFolderContent :: [FileNode] } deriving Show

instance Eq FileNode where
  (==) (File firstName firstContent) (File secondName secondContent) = firstName == secondName && firstContent == secondContent
  (==) (Folder firstName firstList) (Folder secondName secondList) = firstName == secondName && and (zipWith (==) firstList secondList)
  (==) _ _ = False

data FileZipper = FileZipper 
  { getFileZipperName::Name,
    left :: [FileNode],
    right :: [FileNode] } deriving Show

type Zipper = (FileNode, [FileZipper])

emptyFS :: Maybe Zipper
emptyFS = Just (Folder (Name "")  [], [])

-- addFile is used internaly in the touch and mkdir functions which the user can use
addFile :: FileNode -> Zipper -> Maybe Zipper
addFile file (Folder name contents, t) = Just (Folder name (file : contents), t)
addFile _ (File _ _, _) = Nothing 

-- goBack, goToRoot and goTo are used to navigate between directories in the FileSystem
goBack :: Zipper -> Maybe Zipper 
goBack (item, FileZipper name ll rs:bs) = Just (Folder name (ll ++ [item] ++ rs), bs)
goBack (t, []) = Just (t,[])

goToRoot :: Zipper -> Maybe Zipper 
goToRoot (x, []) = Just (x, [])
goToRoot t = goBack t >>= goToRoot 

goTo :: Name -> Zipper -> Maybe Zipper 
goTo name (Folder folderName items, bs) = if null (dropWhile (not . equallFolderName name) items)
  then Nothing 
  else
    let (ll, item:rs) = break (equallFolderName name) items
    in Just (item, FileZipper folderName ll rs:bs)
goTo _ (File _ _, _) = Nothing

-- used in the break function in goTo in order to avoid going in files, the user can only go deeper if 
-- the desired destination is a folder
equallFolderName :: Name -> FileNode -> Bool
equallFolderName name (Folder folderName _) = name == folderName
equallFolderName _ (File _ _) = False

-- similar to the Linux pwd, extracts the path to the current folder
pwd :: Zipper -> String
pwd (File name _, []) = "/" ++  getName name
pwd (Folder name _, []) = "/" ++  getName name
pwd (x, ll) = "/" ++ getName (getFileZipperName (last ll)) ++ pwd (x, init ll)

-- cd is used after some conversion of the user input, a.k.a after we know whether the path is absolute
-- or relative
cd :: [Name] -> Zipper -> Maybe Zipper
cd [] z = Just z
cd (Name ".." : xs) z = goBack z >>= cd xs
cd (x:xs) z = goTo x z >>= cd xs

-- cdFrom is used to determine from where to start the change of directories
cdFrom :: [Name] -> Zipper -> Maybe Zipper
cdFrom [] t = cd [] t
cdFrom (Name "":xs) t = goToRoot t >>= cd xs
cdFrom ll t = cd ll t

-- lists all files and folders in the current one
ls :: Maybe Zipper -> [String]
ls Nothing = []
ls (Just (Folder _ ll, _)) = toStringList ll
ls (Just (File _ _, _)) = []

-- used to list the contents of some other folder, not the one from which the command has been used
lsFrom :: [Name] -> Zipper -> [String]
lsFrom path z = ls $ cdFrom path z

-- used for conveting the FileNode list into list of the names of the folders and files in ls
toStringList :: [FileNode] -> [String]
toStringList [] = []
toStringList (File name _ : xs) = getName name : toStringList xs
toStringList (Folder name _ : xs) = getName name : toStringList xs

-- deletes files stored in the current folder, if one of the arguments is a folder or non existant file
-- this function just skips it
rm :: [Name] -> Zipper -> Maybe Zipper
rm [] t = Just t
rm (x:xs) (Folder name content, z) = if x `elem` map getFileNodeName (filter isFile content)
  then rm xs (Folder name (filter (\ y -> not (isFile y && getFileNodeName y == x)) content), z)
  else rm xs (Folder name content, z)
rm _ (File _ _, _) = Nothing

-- same as rm but deletes folders
rmdir :: [Name] -> Zipper -> Maybe Zipper
rmdir [] t = Just t
rmdir (x:xs) (Folder name content, z) = if x `elem` map getFileNodeName (filter isFolder content)
  then rmdir xs (Folder name (filter (\ y -> not (isFolder y && getFileNodeName y == x)) content), z)
  else rmdir xs (Folder name content, z)
rmdir _ (File _ _, _) = Nothing

-- isFile and isFolder are used to distinguish between FileNodes which are files and those which are not
isFile :: FileNode -> Bool
isFile (File _ _) = True
isFile (Folder _ _) = False

isFolder :: FileNode -> Bool
isFolder = not . isFile

-- used for extracting the file with the given name from the current folder in the FileSystem
getFile :: Name -> Zipper -> Maybe FileNode
getFile _ (File _ _, _) = Nothing
getFile name (Folder _ nodes, _) = let filteredNodes = filter (\ node -> getFileNodeName node == name) nodes
                                       fileNodes = filter isFile filteredNodes
                                   in if null fileNodes then Nothing else Just (head fileNodes)

-- modification of mappend which works in a more suitable way for this project
myMappend :: Maybe String -> Maybe String -> Maybe String
myMappend Nothing _ = Nothing
myMappend _ Nothing = Nothing
myMappend a b = mappend a b

-- used for concatenating the contents of a few files
cat :: [[Name]] -> Zipper -> Maybe String
cat [] _ = Just ""
cat (x:xs) z = myMappend (fmap (getContent . getFileNodeContent) (cdFrom (init x) z >>= getFile (last x))) (cat xs z)

-- used in order to get a Name list from path, the first case is there because my implemantation relies on
-- this function return an empty list for an empty string and otherwise it would return [""] instead
toNameList :: String -> [Name]
toNameList "" = []
toNameList s = map Name $ splitOn "/" s

-- makes a file from two strings - the first one being the name and the second one the content
toFile :: String -> String -> FileNode
toFile name content = File (Name name) (Content content)

-- for the user to be able to input file contents which contain spaces
concatWithSpaces :: [String] -> String
concatWithSpaces [] = ""
concatWithSpaces [x] = x
concatWithSpaces (x:xs) = x ++ " " ++ concatWithSpaces xs

-- used when cat is called without arguments for input to get input from the console. It stops when
-- the input is a single dot on a new line
catFromConsole :: IO String -> IO String
catFromConsole text = do 
  currentText <- getLine
  if currentText /= "."
   then do
    lastText <- text
    catFromConsole (return (lastText ++ currentText))
   else text

-- checks whether the user has passed correctly file for the output of cat command
-- examples of incorrect data is only ">" without a file after it or if there are multiple files
isValidOutput :: [String] -> Bool
isValidOutput output = length output == 2 && head output == ">"

-- the most important function in the whole program, used for interaction with the current FileSystem
-- and changing its state
endless :: Maybe Zipper -> IO()
endless currentFileSystem = do
-- this print can be easily removed if one does not want to show the current state on every iteration
-- it is there because that way it's easier to check what happens on every step and whether everything
-- else works as expected
  print currentFileSystem
  commands <- getLine
  if null commands
    then endless currentFileSystem
    else do
         let commandWithParams = splitOn " " commands
             command = head commandWithParams
             params = tail commandWithParams
         case command of
              "cd" -> when (length params == 1) $
                      endless (currentFileSystem >>= cd (toNameList (head params)))
              "touch" -> unless (null params) $
                         endless (currentFileSystem >>= addFile (toFile (head params) (concatWithSpaces (tail params))))
              "mkdir" -> when (length params == 1) $
                         endless (currentFileSystem >>= addFile (Folder (Name (head params)) []))
              "ls" -> if null params
                         then print (fmap (lsFrom []) currentFileSystem)
                         else print (fmap (lsFrom (toNameList (head params))) currentFileSystem)
              "pwd" -> print $ fmap pwd currentFileSystem
              "rm" -> endless (currentFileSystem >>= rm (map Name params))
              "rmdir" -> endless (currentFileSystem >>= rmdir (map Name params))
              "cat" | ">" `elem` params ->
                      do let (input, output) = break (== ">") params
                         case output of
                          [">", outFile] ->
                           if null input
                           then do text <- catFromConsole (return "")
                                   endless $ currentFileSystem >>= addFile (toFile outFile text)
                           else do let text = currentFileSystem >>= cat (map toNameList input)
                                   case text of
                                      Nothing -> return ()
                                      (Just content) -> endless $
                                                     currentFileSystem >>= addFile (toFile outFile content)
                          _ -> return ()
                    | null params ->
                      do text <- catFromConsole (return "")
                         print text
                    | otherwise ->
                      print $ currentFileSystem >>= cat (map toNameList params)
              _ -> return ()
         endless currentFileSystem

main :: IO() 
main = endless emptyFS
