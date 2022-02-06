module LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where


expectedMinutesInOven :: Int
expectedMinutesInOven = 40

preparationTimeInMinutes :: Int -> Int
preparationTimeInMinutes layers = layers * 2 

elapsedTimeInMinutes :: Int -> Int -> Int
elapsedTimeInMinutes layers minutes = preparationTimeInMinutes layers + minutes