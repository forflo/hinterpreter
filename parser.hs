import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Monad

-- type aliases and data declarations for parse tree generation
type Id = String
type Nu = Integer

data Expression = 
      Identifier Id 
    | Number Nu 
    | Addition Expression Expression
    | Subtraction Expression Expression
    | Multiplication Expression Expression
    | Division Expression Expression
    deriving Show

data Statement = 
      Assignment Id Expression
    | If Expression [Statement] [Statement]
    | Ifs Expression [Statement]
    | While Expression [Statement]
    | Blockstatement (Block) deriving Show

data Program = Program (Block) deriving Show
data Block = Block [Declaration] [Statement] deriving Show
data Declaration = 
      Const Id Nu
    | Variable (Id) deriving Show

-- helper function for running parsec parsers
run :: Show a => Parser a -> String -> IO ()
run p input =
    case (parse p "" (filter (\x -> x /= ' ' && x /= '\n') input)) of
        Left err -> do
            putStr "Parse error at "
            print err
        Right x -> print x

interpret :: String -> Poststore
interpret input = 
    case (parse parse_program "" (filter (\x -> x /= ' ') input)) of
        Left err -> undefined --(Right (do { putStr "Parse error at "; print err}))
        Right x -> eval_program x firstloc newstore

parse_expression :: Parser Expression
parse_expression = buildExpressionParser table parse_factor
    <?> "expression"

table = [[op "*" (Multiplication) AssocLeft, op "/" (Division) AssocLeft],
         [op "+" (Addition) AssocLeft, op "-" (Subtraction) AssocLeft]]
        where
            op s f assoc
                = Infix (do{ string s; return f}) assoc

parse_factor :: Parser Expression
parse_factor = do
    char '('
    x <- parse_expression
    char ')'
    return x
    <|> do
        id <- parse_identifier
        return (Identifier id)
    <|> do
        number <- many1 digit
        return (Number (read number))
    <?> "simple expression"

parse_statement_list :: Parser [Statement]
parse_statement_list = 
    parse_statement `sepBy1` (char ';')

-- either an assignment, while-loop or block
parse_statement :: Parser Statement
parse_statement = do
        id <- parse_identifier
        string ":="
        expression <- parse_expression
        return (Assignment id expression)
    <|> do
        string "while"
        boolean_exp <- parse_expression
        string "do"
        statements <- parse_statement_list
        string "end"
        return (While boolean_exp statements)
    <|> do  
        block <- parse_block
        return (Blockstatement block)
    <|> do
        string "if"
        exp <- parse_expression
        string "then"
        statements <- parse_statement_list
        string "end"
        return (Ifs exp statements)
    <|> do
        string "cif"
        boolean_exp <- parse_expression
        string "then"
        t_statements <- parse_statement_list
        string "else"
        f_statements <- parse_statement_list
        string "end"
        return (If boolean_exp t_statements f_statements)

parse_declaration_list :: Parser [Declaration]
parse_declaration_list =
    parse_declaration `sepBy1` (char ';')

parse_declaration :: Parser Declaration
parse_declaration = do
        string "const"
        id <- parse_identifier
        char '='
        num <- parse_number
        return (Const id (read num))
    <|> do
        string "var"
        id <- parse_identifier 
        return (Variable id)

parse_block :: Parser Block
parse_block = do
    string "begin"
    declarations <- parse_declaration_list
    string "is"
    statements <- parse_statement_list
    string "end"
    return (Block declarations statements)

parse_program :: Parser Program
parse_program = do
    block <- parse_block
    char '.'
    return (Program block)

-- parses upper case letter
parse_identifier :: Parser Id
parse_identifier = many1 upper

-- parses one digit
parse_number :: Parser String
parse_number = many1 digit

-- Type wrapper
type Store = Location -> StorableValue
type Environment = ((Id -> DenotableValue), Location)
type StorableValue = Integer
type Location = Integer

-- Auxillary types
data DenotableValue = 
      DvLocation Location 
    | DvNumber Nu
    | DvErr Errvalue
    deriving Show

data ExpressibleValue =
      EvNumber Nu
    | EvErr Errvalue
    deriving Show

data Errvalue = Errvalue deriving Show

data Poststore = 
      Error Store
    | Ok Store

-- The Store domain
newstore :: Store
newstore = (\x -> 0)

access :: Location -> Store -> StorableValue
access location store = store location

update :: Location -> StorableValue -> Store -> Store
update location value store = 
    \x -> if x == location then value else store x

-- The Location domain
firstloc :: Location
firstloc = 0

nextloc :: Location -> Location
nextloc location = succ location

equalloc :: Location -> Location -> Bool
equalloc = (==)

lessthanloc :: Location -> Location -> Bool
lessthanloc = (<=)

-- The environment domain
emptyenv :: Location -> Environment
emptyenv location = (\x -> DvErr Errvalue, location)

accessenv :: Id -> Environment -> DenotableValue
accessenv identifier = \(map, location) -> map identifier

updateenv :: Id -> DenotableValue -> Environment -> Environment
updateenv identifier value = \(map, location) -> 
    (\x -> if x == identifier then value else map x, location)

reservelocn :: Environment -> (Location, Environment)
reservelocn = \(map, location) -> (location, (map, nextloc location))


preturn :: Store -> Poststore
preturn store = (Ok store)

signalerr :: Store -> Poststore
signalerr store = (Error store)

check :: (Store -> Poststore) -> (Poststore -> Poststore)
check mapping = 
    \x -> case x of 
        (Ok x) -> mapping x
        (Error x) -> (Error x)
 
--
-- The valuation functions!
-- AKA the interpreter
-- 
eval_program :: Program -> Location -> Store -> Poststore
eval_program (Program block) = 
    (\location -> eval_block block (emptyenv location))

eval_block :: Block -> Environment -> Store -> Poststore
eval_block (Block declarations statements) environment = 
    eval_statements statements (eval_declarations declarations environment)

-- statement list evaluation
eval_statements :: [Statement] -> Environment -> Store -> Poststore
eval_statements (x:[]) env = 
    eval_statement x env
eval_statements (x:xs) env = 
    (check (eval_statements xs env)) . (eval_statement x env)

-- statement evaluation
eval_statement :: Statement -> Environment -> Store -> Poststore
eval_statement (Assignment id expression) = 
    \environment store -> case (accessenv id environment) of 
        (DvLocation loc) -> case (eval_expression expression environment store) of
            (EvNumber num) -> preturn (update loc num store)
            (EvErr err) -> (signalerr store)
        (DvNumber num) -> (signalerr store)
        (DvErr err) -> (signalerr store)

eval_statement (While expression statements) =
    \environment store -> 
        let newt = statements ++ [While expression statements]
        in eval_statement (Ifs expression newt) environment store

eval_statement (Ifs expression statements) =
    \environment store -> case (eval_expression expression environment store) of
        (EvNumber num) -> case num of
            0 -> preturn store
            _ -> eval_statements statements environment store
        (EvErr err) -> (signalerr store)

eval_statement (If expression truestmts falsestmts)=
    \environment store -> case (eval_expression expression environment store) of
        (EvNumber num) -> case num of
            0 -> eval_statements falsestmts environment store
            _ -> eval_statements truestmts environment store
        (EvErr err) -> (signalerr store)
    
eval_statement (Blockstatement block) = eval_block block

-- declaration list evaluation
eval_declarations :: [Declaration] -> Environment -> Environment
eval_declarations (x:[]) = eval_declaration x
eval_declarations (x:xs) =
    (eval_declarations xs) . (eval_declaration x)

-- declaration evaluation
eval_declaration :: Declaration -> Environment -> Environment
eval_declaration (Const id num) = updateenv id (DvNumber num)
eval_declaration (Variable id) =
    \environment -> let (loc, env) = (reservelocn environment) in
        (updateenv id (DvLocation loc) env)

-- expression evaluation
eval_expression :: Expression -> Environment -> Store -> ExpressibleValue
-- +-*/
eval_expression (Addition opa opb) environment store =
    case (eval_expression opa environment store) of
        (EvNumber numa) -> case (eval_expression opb environment store) of
            (EvNumber numb) -> EvNumber (numa + numb)
            (EvErr err) -> (EvErr err)
        (EvErr err) -> (EvErr err)
eval_expression (Multiplication opa opb) environment store =
    case (eval_expression opa environment store) of
        (EvNumber numa) -> case (eval_expression opb environment store) of
            (EvNumber numb) -> EvNumber (numa * numb)
            (EvErr err) -> (EvErr err)
        (EvErr err) -> (EvErr err)
eval_expression (Subtraction opa opb) environment store =
    case (eval_expression opa environment store) of
        (EvNumber numa) -> case (eval_expression opb environment store) of
            (EvNumber numb) -> EvNumber (numa - numb)
            (EvErr err) -> (EvErr err)
        (EvErr err) -> (EvErr err)
eval_expression (Division opa opb) environment store =
    case (eval_expression opa environment store) of
        (EvNumber numa) -> case (eval_expression opb environment store) of
            (EvNumber numb) -> EvNumber (numa `div` numb)
            (EvErr err) -> (EvErr err)
        (EvErr err) -> (EvErr err)

eval_expression (Identifier id) environment store =
    case (accessenv id environment) of
        (DvLocation loc) -> (EvNumber (access loc store))
        (DvNumber num) -> (EvNumber num)
        (DvErr err) -> (EvErr err)

eval_expression (Number num) environment store = (EvNumber num)
