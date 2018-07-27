{
module Alex where
}

%wrapper "basic"

$special   = [\(\)\,\;\[\]\`\{\}]
$asciisymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$asciidigit = 0-9
$digit = $asciidigit -- TODO
$asciialpha = [a-z A-Z \_]
$alpha = $printable # [$white $special $digit $asciisymbol \'] -- lol
$symbol = $printable # [$white $special $digit $asciialpha \'] -- lol
$delim = [\'\`]
$namespace = [tv]
$idchar = [$alpha $digit \']

@id = ($alpha $idchar* | $symbol+)

:-
  $namespace? $delim @id $delim { Identifier }
  [. \n] ;

{
-- The token type:
newtype Identifier = Identifier String
  deriving Show

--main = do
--  s <- getContents
--  print (alexScanTokens s)
}
