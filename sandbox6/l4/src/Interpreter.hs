
module Interpreter where
import AbsBurger
import PrintBurger

interpret :: L4Module -> [String]
interpret (MkL4Module sections) = do
  return $ "We have here an L4 Module, containing " ++ show (length sections) ++ " sections."

--    case x of
--      MyAdd exp0 exp  -> interpret exp0 + interpret exp
--      MyInt n  -> n
