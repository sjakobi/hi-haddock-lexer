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
$symbol = $printable # [$white $special $asciidigit $asciialpha \'] -- lol
$delim = [\'\`]
$namespace = [tv]
$idchar = [$alpha $digit \']
$nondelim = [^ $delim $namespace ]

@id = ($alpha $idchar* | $symbol+)

:-
  $white+ ;
  $namespace? $delim @id $delim { Identifier }
  $alpha+ ;
  $digit+ ;
  $symbol+ ;

{
-- The token type:
newtype Identifier = Identifier String
  deriving Show

--main = do
--  s <- getContents
--  print (alexScanTokens s)
}
