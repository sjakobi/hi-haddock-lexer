{
module Alex where
}

%wrapper "posn"

-- The character sets marked "TODO" are mostly overly inclusive
-- and should be defined more precisely once alex has better
-- support for unicode character sets (See
-- https://github.com/simonmar/alex/issues/126)
$special   = [\(\)\,\;\[\]\`\{\}]
$asciisymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$asciidigit = 0-9
$digit = $asciidigit                             -- TODO
$asciilower = [a-z \_]
$asciiupper = A-Z
$asciialpha = [$asciilower $asciiupper]
$interesting = $printable # [$white $special]
$alpha = $interesting # [$digit $asciisymbol \'] -- TODO
$upper = $alpha # $asciilower                    -- TODO
$symbol = $interesting # [$digit $asciialpha \'] -- TODO
$idchar = [$alpha $digit \']
$delim = [\'\`]

@id = ($alpha $idchar* | $symbol+)
@modname = $upper $idchar*
@qualid = (@modname \.)* @id

:-
  $delim ^ @qualid / $delim { \(AlexPn off _ _) s -> Identifier off s (off + length s) }
  [. \n] ;

{
-- The token type:
data Identifier = Identifier Int String Int
  deriving Show

--main = do
--  s <- getContents
--  print (alexScanTokens s)
}
