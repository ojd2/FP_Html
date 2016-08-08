{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Layouts where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Prelude hiding (head, body, id, div, span)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title, span, meta, content)
import qualified Text.Blaze.Renderer.Utf8 as R (renderHtml)

import Control.Monad.Trans
import System.Environment
import System.Random (randomRIO)

-- | Create some data for our random numbers.
data NumberFormat = NFPlain
                  | NFUSD
-- | Union for reading vals.
readsNumberFormat x
  | x == "usd" = NFUSD
  | otherwise  = NFPlain
  
-- | Create some data for our random numbers.
data RandomNumber = Plain Integer
                  | USD Integer Integer
                  

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

-- | Create a Logo img element.
brandLogo :: Html
brandLogo = img ! src "https://devchat.tv/wp-content/themes/devchat/images/dev-white.svg" ! alt "A Brand Image."

-- | Create some Para element.
para :: Html
para = p "As you can see, you can chain multiple arguments using the ! operator. Setting an attribute on an element with context also uses the ! operator."

-- | Create a Web Banner img element.
webBanner :: Html
webBanner = img ! src "https://devchat.tv/wp-content/themes/devchat/images/js-jabber.png" ! class_ "banner"

-- | Generate Number Format of NFP or NFUSD.
generateForNumberFormat low high format =
  case format of
    NFPlain -> Plain <$> randomRIO (low, high)
    NFUSD   -> do
      dollars <- randomRIO (low, high)
      cents   <- randomRIO (0, 99)
      return $ USD dollars cents
     
-- | Create procedure for showing numbers with commas.
showsIntegerWithCommas x = 
  if x < 0
  then "-" ++ f (abs x)
  else f (abs x)
  where
    f = reverse . intercalate "," . chunksOf 3 . reverse . show

-- | Format the USD.
formatUSD :: Integer -> Integer -> String
formatUSD dollars cents = 
  let dollars' = showsIntegerWithCommas dollars
      cents'   = if cents < 10
                 then "0" ++ show cents
                 else show cents
  in concat ["$", dollars', ".", cents']

-- | Create procedure for generating random numbers.
render :: RandomNumber -> Html
render num = layout "Pseudorandom numbers!" $ do
  h1 $ toHtml $ formattedNum
  where
    formattedNum =
      case num of
        (USD dollars cents) -> formatUSD dollars cents
        (Plain x) -> showsIntegerWithCommas x
        
-- | Bind Html some together.
layout :: Html -> Html 
layout usd = docTypeHtml $ do
    head $ do
        title "DevChatTv"
        stylesheet "/css/bootstrap.css"
        meta ! charset "utf-8"

    body $ do
         div ! class_ "navbar navbar-fixed-top" $ 
          div ! class_ "navbar-inner" $ contents
            div ! class_ "container" $ contents
              brandLogo $ ""
                div ! class_ "row" $ contents
                  div ! id "main" $ contents
                    div ! class_ "introduction" $ contents
                    h1 "This is the Introduction!" $ ""
                    para $ ""
                    
                    div ! class_ "middle" $ contents
                    h1 "This is the middle section!" $ ""
                    render $ ""
                    
                    div ! class_ "banner" $ contents
                    h1 "This is the banner!" $ ""
                    webBanner $ ""
                    
                    div ! class_ "footer" $ contents
                    h1 "This is the footer!" $ ""
                    script ! src "/js/jquery-3.1.0.min.js" $ ""
                    script ! src "/js/jquery.cookie.js" $ ""
                    script ! src "/js/bootstrap.min.js" $ ""