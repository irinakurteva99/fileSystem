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

addFile :: FileNode -> Zipper -> Maybe Zipper
addFile file (Folder name contents, t) = Just (Folder name (file : contents), t)
addFile _ (File _ _, _) = Nothing 

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

equallFolderName :: Name -> FileNode -> Bool
equallFolderName name (Folder folderName _) = name == folderName
equallFolderName _ (File _ _) = False

pwd :: Zipper -> String
pwd (File name _, []) = "/" ++  getName name
pwd (Folder name _, []) = "/" ++  getName name
pwd (x, ll) = "/" ++ getName (getFileZipperName (last ll)) ++ pwd (x, init ll)

cd :: [Name] -> Zipper -> Maybe Zipper
cd [] z = Just z
cd (Name ".." : xs) z = goBack z >>= cd xs
cd (x:xs) z = goTo x z >>= cd xs

cdFrom :: [Name] -> Zipper -> Maybe Zipper
cdFrom [] t = cd [] t
cdFrom (Name "":xs) t = goToRoot t >>= cd xs
cdFrom ll t = cd ll t

ls :: Maybe Zipper -> [String]
ls Nothing = []
ls (Just (Folder _ ll, _)) = toStringList ll
ls (Just (File _ _, _)) = []

lsFrom :: [Name] -> Zipper -> [String]
lsFrom path z = ls $ cdFrom path z

toStringList :: [FileNode] -> [String]
toStringList [] = []
toStringList (File name _ : xs) = getName name : toStringList xs
toStringList (Folder name _ : xs) = getName name : toStringList xs

rm :: [Name] -> Zipper -> Maybe Zipper
rm [] t = Just t
rm (x:xs) (Folder name content, z) = if x `elem` map getFileNodeName content
  then rm xs (Folder name (filter (\ y -> getFileNodeName y /= x) content), z)
  else rm xs (Folder name content, z)
rm _ (File _ _, _) = Nothing

isFile :: FileNode -> Bool
isFile (File _ _) = True
isFile (Folder _ _) = False

getFile :: Name -> Zipper -> Maybe FileNode
getFile _ (File _ _, _) = Nothing
getFile name (Folder _ nodes, _) = let filteredNodes = filter (\ node -> getFileNodeName node == name) nodes
                                       fileNodes = filter isFile filteredNodes
                                   in if null fileNodes then Nothing else Just (head fileNodes)

myMappend :: Maybe String -> Maybe String -> Maybe String
myMappend Nothing _ = Nothing
myMappend _ Nothing = Nothing
myMappend a b = mappend a b

cat :: [[Name]] -> Zipper -> Maybe String
cat [] _ = Just ""
cat (x:xs) z = myMappend (fmap (getContent . getFileNodeContent) (cdFrom (init x) z >>= getFile (last x))) (cat xs z)

toNameList :: String -> [Name]
toNameList "" = []
toNameList s = map Name $ splitOn "/" s

toFile :: String -> String -> FileNode
toFile name content = File (Name name) (Content content)

concatWithSpaces :: [String] -> String
concatWithSpaces [] = ""
concatWithSpaces [x] = x
concatWithSpaces (x:xs) = x ++ " " ++ concatWithSpaces xs

catFromConsole :: IO String -> IO String
catFromConsole text = do 
  currentText <- getLine
  if currentText /= "."
   then do
    lastText <- text
    catFromConsole (return (lastText ++ currentText))
   else text

endless :: Maybe Zipper -> IO()
endless currentFileSystem = do
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
              "cat" | ">" `elem` params ->
                      do let (input, output) = break (== ">") params
                         if null input then
                           do text <- catFromConsole (return "")
                              endless $ currentFileSystem >>= addFile (toFile (output !! 1) text)
                         else do let text = currentFileSystem >>= cat (map toNameList input)
                                 case text of
                                      Nothing -> return ()
                                      (Just content) -> endless $
                                                     currentFileSystem >>= addFile (toFile (output !! 1) content)
                    | null params ->
                      do text <- catFromConsole (return "")
                         print text
                    | otherwise ->
                      print $ currentFileSystem >>= cat (map toNameList params)
              _ -> return ()
         endless currentFileSystem

main :: IO() 
main = endless emptyFS
