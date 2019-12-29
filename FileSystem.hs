type Name = String
type Content = String

data FileType = File Name Content | Folder Name [FileType] deriving Show

data FileTracer = FileTracer Name [FileType] [FileType] deriving Show

type FileZipper = (FileType, [FileTracer])

makeEmptyFS :: FileZipper
makeEmptyFS = (Folder "" [], [])

addFile :: FileType -> FileZipper -> FileZipper
addFile file (Folder name contents, t) = (Folder name (file : contents), t)
addFile _ (File _ _, _) = error "cannot add file in file"

goBack :: FileZipper -> FileZipper 
goBack (item, FileTracer name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

goToRoot :: FileZipper -> FileZipper 
goToRoot (x, []) = (x, [])
goToRoot t = goToRoot (goBack t)

goTo :: Name -> FileZipper -> FileZipper
goTo name (Folder folderName items, bs) = 
                  let (ls, item:rs) = break (nameIs name) items
                  in (item, FileTracer folderName ls rs:bs)
goTo _ (File _ _, _) = error "already at a file, cannot go forward"

nameIs :: Name -> FileType -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = False 
 
pwd :: FileZipper -> String
pwd (File name _, []) = "/" ++  name
pwd (Folder name _, []) = "/" ++  name
pwd (x, ls) = "/" ++ getName (last ls) ++ pwd (x, init ls) 

getName :: FileTracer -> String
getName (FileTracer name _ _) = name

emptyFileSystem :: FileType
emptyFileSystem = Folder "/" []

ls :: [String] -> FileZipper -> [String]
ls [] (File _ _, _) = []
ls [] (Folder _ xs, _) = toStringList xs
ls (x:xs) t = ls xs $ goTo x t


lsFrom :: [String] -> FileZipper -> [String] 
lsFrom [] (Folder _ xs, _) = toStringList xs
lsFrom ("":xs) t = ls xs (goToRoot t)
lsFrom l t = ls l t

cd :: [String] -> FileZipper -> FileZipper
cd [] t = t
cd (x:xs) t = cd xs $ goTo x t

cdFrom :: [String] -> FileZipper -> FileZipper
cdFrom [] t = t
cdFrom ("":xs) t = cd xs (goToRoot t)
cdFrom ls t = cd ls t

toStringList :: [FileType] -> [String]
toStringList [] = []
toStringList (File name _ : xs) = name : toStringList xs
toStringList (Folder name _ : xs) = name : toStringList xs

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
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]  
