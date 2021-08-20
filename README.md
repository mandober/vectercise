# Vector

- repo: https://github.com/mandober/vectercise
- name: `vectercise`
- lang: Haskell

Type-level programming in Haskell:    
Implementing list functions on length-indexed vectors,    
by following a video series by Richard Eisenberg    
(on a completely different subject! or not)


## Based on

This project follows Richard Eisenberg's video series about implementing, progressively ever so more complicated, `Data.List` functions on the Vec data type.

* How to program in types with length-indexed vectors
  - video 1: https://www.youtube.com/watch?v=PHS3Q-tRjFQ&list=PLyzwHTVJlRc9QcF_tdqc9RdxJED8Mvyh1&index=26
  - desc: This video is the start of a series covering the translation of the `Data.List` functions to work on the length-indexed vector GADT. This will be an exploration of many aspects of type-level programming, in small bite-sized chunks. This week: GADT basics, and how a type signature can make a partial-looking function - the dreaded `head` function of the `Prelude`- into a nice, safe total function.
  - code: 
  https://github.com/goldfirere/video-resources/tree/main/2021-04-13-vectors
  - Data.List: 
  https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html
  - Paper about untouchable variables:
  https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp-outsidein.pdf


* Using singleton types to replicate a length-indexed Vector
  - video 2: https://www.youtube.com/watch?v=PHS3Q-tRjFQ&list=PLyzwHTVJlRc9QcF_tdqc9RdxJED8Mvyh1&index=27
  - desc: 
  - code: https://github.com/goldfirere/video-resources/tree/main/2021-05-18-vectors

* Type families help define functions over length-indexed vectors
  - video 3: https://www.youtube.com/watch?v=PHS3Q-tRjFQ&list=PLyzwHTVJlRc9QcF_tdqc9RdxJED8Mvyh1&index=28
  - desc: 
  - code: https://github.com/goldfirere/video-resources/tree/main/2021-05-18-vectors

* Existentials and writing functions for length-indexed vectors
  - video 4: https://www.youtube.com/watch?v=PHS3Q-tRjFQ&list=PLyzwHTVJlRc9QcF_tdqc9RdxJED8Mvyh1&index=29
  - desc: 
  - code: https://github.com/goldfirere/video-resources/tree/main/2021-05-18-vectors

* Using proofs to make functions faster over length-indexed vectors
  - video 5: https://www.youtube.com/watch?v=PHS3Q-tRjFQ&list=PLyzwHTVJlRc9QcF_tdqc9RdxJED8Mvyh1&index=30
  - desc: 
  - code: https://github.com/goldfirere/video-resources/tree/main/2021-05-18-vectors

* Some functions on length-indexed vectors require custom GADTs
  - video 6: https://www.youtube.com/watch?v=PHS3Q-tRjFQ&list=PLyzwHTVJlRc9QcF_tdqc9RdxJED8Mvyh1&index=31
  - desc: This video covers writing `!!` and `span` for Vec, the latter of which requires the design of a custom GADT.
  - code: https://github.com/goldfirere/video-resources/tree/main/2021-05-18-vectors


Richard Eisenberg
- home: https://richarde.dev/
- github: https://github.com/goldfirere/


## Notes

Actually, these is my following Richard Eisenberg's videos while copy-coding and generating comments, at times, too enthusiastically. Redundancy abounds. The files do contain the actual code sporadically and disproportionally to the comments that reflect my step-by-step understanding of the laid out central, as well as some related, some not that much, some at least relevanat, and some quite near, subjects (loose ends abound).

Data! List! Vector! Twist! ... Worthwhile!
