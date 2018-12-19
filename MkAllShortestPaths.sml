functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  exception NYI
  (* graph:每一个条边（v，u）中的v形成一个表，并将其所有的out neighbour保存为一 table
   *)
  type graph = set table
  type asp =vertex *( vertex seq table)

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
    if Seq.length E = 0 then Table.empty()
    else
      let 
        val seqG = Table.collect E
      in
        Table.map (Set.fromSeq) seqG
      end
  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
      let 
        val countEveryVertex = Table.map (Set.size) G
      in
        Table.reduce op+ 0 countEveryVertex
      end

  fun numVertices (G : graph) : int =
     let 
       val graphDomain = Table.domain G
       val unioned = Table.reduce (Set.union) graphDomain G
      in
         Set.size unioned
     end

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    if Table.size G = 0 then Seq.empty()
    else 
      case Table.find G v 
        of NONE => Seq.empty()
         | SOME a => Set.toSeq a 
  (* Task 2.4 *)

  (*
  * union function is to union two seq table if they have a same
  * key then append their value(seq)
  * *)
  fun union(M,N)=
    Table.merge (fn(a,b) => Seq.append(a,b)) (M,N)
    
  fun ngf (F,G) = 
     let
         (*return a table the key is u's outneighbors and the value is
         * singleton(u)  --->seq table
         * *)
         fun addParent u = 
            Table.tabulate (fn _ => Seq.singleton(u)) (Set.fromSeq(outNeighbors G u))
         (*
         * added is a table , key is every element is set F 
         * and the value is a table
         * *)
         val  added = Table.tabulate addParent F
     in
         (*
         * return a unioned table
         * union(table,table) -->the value of f(f belonging F)
         * union aims to merge two table even they may have the same
         * key
         * *)
         Table.reduce union (Table.empty()) added
     end
  fun makeASP (G : graph) (v : vertex) : asp =     
      case Table.find G v
        of NONE => (v,Table.empty())
         | SOME _ =>
             let
               fun BFSReach (X,F) =
                 if Table.size F=0 then X
                 else 
                    let 
                      val X'= union(X,F)
                      val NGF = ngf(Table.domain(F),G)
                      val F' = Table.erase(NGF,Table.domain X')
                    in
                      BFSReach (X',F')
                    end
             in
               (v,BFSReach(Table.empty(),Table.singleton(v,Seq.empty())))
             end

  (* Task 2.5 *)
fun report (A : asp) (v : vertex) : vertex seq seq =
   let
     val (s,sTable) = A
     fun toRoot (u : vertex) : vertex seq seq =
       if Key.equal(u,s) then
         Seq.singleton(Seq.singleton(s))
       else
         let 
           val SOME uSeq = Table.find sTable u 
           val pre = Seq.map toRoot uSeq
           val pre' = Seq.flatten pre
         in
           Seq.map (fn (a) => Seq.append(a,Seq.singleton(u))) pre'
         end
   in
     case Table.find sTable v 
       of NONE => Seq.empty()
        | SOME vSeq =>
            toRoot(v)
   end
end
