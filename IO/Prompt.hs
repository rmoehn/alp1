module IO.Prompt(
    prompt,
) where

prompt :: String -> IO String
prompt inquiry = do
    putStr inquiry
    getLine
