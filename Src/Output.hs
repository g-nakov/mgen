module Output where

import TranslateContext ( Translated )
import Expression (Program)
import Description ( Description,  QualifiedName)

class Output t where
  addPreamble :: t -> String -> Description QualifiedName -> Program -> Translated t
  render ::  t -> String

data Document = forall t. (Output t) => MkDocument t

instance Output Document where
  addPreamble (MkDocument x) f d p =  MkDocument <$> addPreamble x f d p
  render (MkDocument x) = render x
