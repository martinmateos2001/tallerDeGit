
relacionesValidas :: [(String, String)]->Bool
relacionesValidas [] = True
relacionesValidas (t1:t2:ts) = recorroLista t1 (t2:ts) && (relacionesValidas (t2:ts)) 


relacionMismos :: (String, String) -> Bool
relacionMismos (x1,y1)
    |x1 == y1 = False
    |otherwise = True

relacionDeDos :: (String,String) -> (String,String) ->Bool
relacionDeDos (x1,y1) (x2,y2)
    |(x1 == x2) && (y1 == y2) = False
    |otherwise = True

recorroLista :: (String,String)->[(String,String)]->Bool
recorroLista t [] = relacionMismos t
recorroLista t1 (t2:ts) 
    |relacionMismos t1 == True && relacionDeDos t1 t2 == True = recorroLista t1 ts
    |otherwise = False