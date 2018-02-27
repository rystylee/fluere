module Sound.Fluere.Chord where


c  = 261.626
d  = 293.665
e  = 329.628
f  = 349.228
g  = 391.995
a  = 440
b  = 493.883
c' = 523.251

scales :: [Float]
scales = [c, d, e, f, g, a, b, c']

chords :: [[Float]]
chords = [ [c, e, g]
          ,[d, f, a]
          ,[e, g, b]
          ,[f, a, c']
          ,[g, b, c]
          ,[a, c', d]
          ,[b, c, e]
          ,[c', d, f] ]
