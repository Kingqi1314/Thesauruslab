functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq
  open ASP

  (* Remove the following two lines when you're done! *)
  exception NYI

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  type thesaurus = graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
     let 
       fun copy (w : vertex ,s : vertex seq)= 
         Seq.map (fn(x) => (w,x) ) s
       val wordSeqCopy = Seq.map copy S
       val wordSeq = Seq.flatten wordSeqCopy
     in
       ASP.makeGraph wordSeq
     end

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    ASP.numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
    ASP.outNeighbors T w

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    ASP.report (ASP.makeASP T w1) w2

end
