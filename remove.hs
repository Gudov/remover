import System.Win32.File
import Data.Bits
import Control.Monad
import System.Environment

absPath path = "\\\\?\\" ++ path

isDirectory filePath = do
    att <- getFileAttributes $ absPath filePath
    return $ (att .&. fILE_ATTRIBUTE_DIRECTORY) == fILE_ATTRIBUTE_DIRECTORY

isValidPath path = path /= "." && path /= ".."

recFiles path action = do
    let path' = absPath path
    (state, findData) <- findFirstFile (path' ++ "\\*")
    recFiles' state findData action path
    findClose state

recFiles' state findData action root = do
    path <- getFindDataFileName findData

    let fullPath = root ++ "\\" ++ path
    let isValid = isValidPath path
    isDir <- if isValid then isDirectory fullPath else return False

    when isValid (applyPath isDir fullPath action)

    findEnd <- findNextFile state findData
    if findEnd then recFiles' state findData action root else action (root, True)

applyPath isDir fullPath action = if isDir then recFiles fullPath action else action (fullPath, isDir)

deleteAny (path, isDir) = do
    let path' = absPath path
    setFileAttributes path' fILE_ATTRIBUTE_NORMAL
    if isDir then removeDirectory path' else deleteFile path'

main = do
    args <- getArgs
    let path = head args
    recFiles path deleteAny
