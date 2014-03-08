- module('test_cond').
- compile(export_all).

bmiTell(Weight,Height) ->
    ( fun(Bmi ,Skinny ,Normal ,Fat ) -> 
        if
            Bmi =< Skinny  -> 
                "You're underweight, you emo, you!";
            Bmi =< Normal  -> 
                "You're supposedly normal. Pffft, I bet you're ugly!";
            Bmi =< Fat  -> 
                "You're fat! Lose some weight, fatty!";
            true  -> 
                "You're a whale, congratulations!"
            
        end
    end)(Weight / (Height * Height )  ,18.5 ,25.0 ,30.0 ).

bmiTell2(Weight,Height) ->
    ( fun(Bmi ,Skinny ,Normal ,Fat ) -> 
        if
            Bmi =< Skinny  -> 
                "You're underweight, you emo, you!";
            Bmi =< Normal  -> 
                "You're supposedly normal. Pffft, I bet you're ugly!";
            Bmi =< Fat  -> 
                "You're fat! Lose some weight, fatty!";
            true  -> 
                "You're a whale, congratulations!"
            
        end
    end)(Weight / (Height * Height )  ,18.5 ,25.0 ,30.0 ).

bmiTell3(Weight,Height) ->
    ( fun(Bmi ,Skinny ,Normal ,Fat ) -> 
        if
            Bmi =< Skinny  -> 
                "You're underweight, you emo, you!";
            Bmi =< Normal  -> 
                "You're supposedly normal. Pffft, I bet you're ugly!";
            Bmi =< Fat  -> 
                "You're fat! Lose some weight, fatty!";
            true  -> 
                "You're a whale, congratulations!"
            
        end
    end)(Weight / (Height * Height )  ,18.5 ,25.0 ,30.0 ).

test() ->
    bmiTell(60,170),
    bmiTell2(70,180).
