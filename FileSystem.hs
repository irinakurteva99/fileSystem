type Name = String
type Content = String
-- ^ how about newtypes instead, so you can't accidentally swap these?
-- newtype Name = Name {getName :: String}
-- newtype Content = Content {getContent :: String}

data FileType = File Name Content | Folder Name [FileType] deriving Show
-- I would definitely call this filetype, becaues it's not only the type
-- but instead a "node" - the directory holds children in it

data FileTracer = FileTracer Name [FileType] [FileType] deriving Show
-- This is the zipper itself, no?
-- also make it a record, you would benefit from that further down

type FileZipper = (FileType, [FileTracer])
-- this could also be a record

makeEmptyFS :: FileZipper
makeEmptyFS = (Folder "" [], [])

addFile :: FileType -> FileZipper -> FileZipper
addFile file (Folder name contents, t) = (Folder name (file : contents), t)
addFile _ (File _ _, _) = error "cannot add file in file"
-- ^ I would definitely prefer to return `Maybe` from this function and handle it at call-site
-- if I see this function I would never know something might go wrong, without looking at the code
-- This applies to all the errors, so I won't point it out everywhere.
--
-- OPTIONAL FANCY TYPE STUFF BELOW:
-- Btw there is a thing you can do in haskell to avoid this:
-- If you define FileZipper like this:
-- {-# LANGUAGE GADTs #-}
-- data TFile
-- data TFolder
--
-- data FileType t where
--   File :: Name -> Content -> FileType TFile
--   Folder :: Name -> [FileType f] -> FileType TFolder
-- this is exactly the same thing as your data FileType declaration above, except
-- I've added a type parameter and specified what it should be in each case:
-- * when we're using the File constructor the parameter is TFile
-- * when we're using the Folder constructor the parameter is TFolder
--
-- This allows to know what the parameter t is when we pattern match on (FileType t)
--
-- type FileZipper t = (FileType t, [FileTracer])
--
-- addFile :: FileType t -> FileZipper TFolder -> FileZipper TFolder
--
-- and now it's impossible to call addFile with something that is constructed with File
-- (i.e. is a File)

goBack :: FileZipper -> FileZipper
goBack (item, FileTracer name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

goToRoot :: FileZipper -> FileZipper
goToRoot (x, []) = (x, [])
goToRoot t = goToRoot (goBack t)
-- if you want to be a cool kid you can implement this as
-- goToRoot = head . dropWhile ((/= []) . snd) . iterate goBack

goTo :: Name -> FileZipper -> FileZipper
goTo name (Folder folderName items, bs) =
                  let (ls, item:rs) = break (nameIs name) items
                  in (item, FileTracer folderName ls rs:bs)
-- use only one indentation for the let, i.e. instead of
--
--                  let (ls, item:rs) = break (nameIs name) items
--                  in (item, FileTracer folderName ls rs:bs)
--
-- write
--
--    let (ls, item:rs) = break (nameIs name) items
--    in (item, FileTracer folderName ls rs:bs)
--
-- it's way easier to read imo
goTo _ (File _ _, _) = error "already at a file, cannot go forward"
-- same comment for error as above

nameIs :: Name -> FileType -> Bool
-- ^ function is named misleadingly, I would expect it to also check the name for files
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = False

pwd :: FileZipper -> String
pwd (File name _, []) = "/" ++  name
pwd (Folder name _, []) = "/" ++  name
pwd (x, ls) = "/" ++ getName (last ls) ++ pwd (x, init ls)
-- what is ls? there is certainly a better name for it
-- also I would write versions of init and last that either
-- * return a Maybe
-- * accepta NonEmpty (I prefer this one), so that there are no hidden errors
-- (NonEmpty is this type
-- data NonEmpty = a :| [a]
-- i.e. a list with at least one element)

getName :: FileTracer -> String
getName (FileTracer name _ _) = name
-- just make FileTracer a record instead

emptyFileSystem :: FileType
emptyFileSystem = Folder "/" []

ls :: [String] -> FileZipper -> [String]
ls [] (File _ _, _) = []
ls [] (Folder _ xs, _) = toStringList xs
ls (x:xs) t = ls xs $ goTo x t


lsFrom :: [String] -> FileZipper -> [String]
lsFrom [] (Folder _ xs, _) = toStringList xs
lsFrom ("":xs) t = ls xs (goToRoot t)
-- why not
-- ls xs $ goToRoot t
-- and the same for all other places
lsFrom l t = ls l t
-- better names pls

cd :: [String] -> FileZipper -> FileZipper
cd [] t = t
cd (x:xs) t = cd xs $ goTo x t
-- this is a foldl
-- btw if you run hlint on your code it suggests the foldl

cdFrom :: [String] -> FileZipper -> FileZipper
cdFrom [] t = t
cdFrom ("":xs) t = cd xs (goToRoot t)
cdFrom ls t = cd ls t

toStringList :: [FileType] -> [String]
toStringList [] = []
toStringList (File name _ : xs) = name : toStringList xs
toStringList (Folder name _ : xs) = name : toStringList xs
-- toStringList = map getName

myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            , Folder "not_dick" [ File "whatever.txt" "I don't care"]
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                                                       -- ^ where did you find this?
                , File "random.hs" "main = print 4"
                ]
            ]
        ]
