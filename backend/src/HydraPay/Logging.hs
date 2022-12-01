-- | 

module HydraPay.Logging where

import qualified Data.Text as T
import Data.String.Interpolate (i)

import Control.Monad.Log
import Data.Text.Prettyprint.Doc (Pretty, Doc, brackets, pretty, align, (<+>))

-- | Type to express we want abbreviated names for our logging output
newtype ShortSeverity = ShortSeverity { getFulllength :: Severity }

instance Pretty ShortSeverity where
  pretty sev = pretty text
    where
      text :: T.Text
      text = case getFulllength sev of
        Emergency -> "Emergency"
        Alert -> "Alert"
        Critical -> "Critical"
        Error -> "Error"
        Warning -> "Warn"
        Notice -> "Notice"
        Informational -> "Info"
        Debug -> "Debug"

withLogging :: LoggingT (WithSeverity (Doc ann)) IO a -> IO a
withLogging = flip runLoggingT (print . renderWithSeverityShort id)

hydraAsciiLogo :: String
hydraAsciiLogo = [i|

                                   ...''..
                     ........';coxxxol:,..
                .,:oxxxOKKKKKXNNNN0c.
             ,okKXNNNOx0NNNNNNNNNNNKx:'.
             :KNNNNNNNNNNNNNNNNNNNNNNNXOo;.
             .oXNNNNNNNNNNNNNNNNNNNNNNNNNk;
               ';cldkOKKXNNNNNNNNNNNNNNNN0:
        .':lllllcc:::cccccclox0XNNNNNNNNNNKl
      .:OXNNNNNNNNNNNNXXK0Oxoc::o0NNNNNNNNNK:
     .d0XNNNNNNNNNNNNXK0KKKXNNKkc;oKNNNNNNNNO'
     l0O0NNNNNNXOxolllcc:::::cdOKk;c0NNNNNNNX:
      ,oOXNX0xlccldOKXXXXKK0kdc:cdx:cKNNNNNNNo
        .,:::ld0XNNNNNNNXXXKKXNKx;,..xNNNNNNNl
       .cdx0XNNNNNNNXOdc,'...,:d00c  :XNNNNNX:
       lKXNNNNNNNN0o,.          .ok; :XNNNNNk'
      'OKOXNNNNXk:.               ;:.dNNNNNK:
      .;dOKNNKd,                   .cKNNNNXl
        .'';;.                     :0NNNNKc.
        .,:,.                    'dXNNNXx,
          .coo:'.             .:dKNNNKd;.
            .,oxxdl:,....',:lkKNNX0xc.
               .':oxkkkOO0000Oxoc,.
                     ........
|]

hydraPayAsciiLogo :: String
hydraPayAsciiLogo = [i|
░█░█░█░█░█▀▄░█▀▄░█▀█░░░█▀█░█▀█░█░█
░█▀█░░█░░█░█░█▀▄░█▀█░░░█▀▀░█▀█░░█░
░▀░▀░░▀░░▀▀░░▀░▀░▀░▀░░░▀░░░▀░▀░░▀░
|]

hydraPayAsciiSubhead :: String
hydraPayAsciiSubhead = [i|
░█▀▄░█░█░█▀█░█▀█░▀█▀░█▀█░█▀▀
░█▀▄░█░█░█░█░█░█░░█░░█░█░█░█
░▀░▀░▀▀▀░▀░▀░▀░▀░▀▀▀░▀░▀░▀▀▀
|]

renderWithSeverityShort
  :: (a -> Doc ann) -> (WithSeverity a -> Doc ann)
renderWithSeverityShort k (WithSeverity u a) =
  brackets (pretty $ ShortSeverity u) <+> align (k a)

