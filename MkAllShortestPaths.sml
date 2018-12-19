functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  exception NYI
  (* graph:每一个条边（v，u）中的v形成一个表，并将其所有的out neighbour保存为一 table
   * asp :
   * source节点所能到达的每一个节点形成一个table，每个元素的value是它可能的父结点，另外保存下source结点 
   *)
  type graph = set table
  type asp =vertex *( vertex seq table)

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
    if Seq.length E = 0 then Table.empty()
    else
      let 
        (*w=O(|E|log|E|) s=O((log|E|)^2)*)
        val seqG = Table.collect E
      in
        (*w=O(|E|) s<=O(log|E|) *)
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
      (*W=O(lg|V|) S=O(lg|V|)*)
      case Table.find G v 
        of NONE => Seq.empty()
        (*W=O(|Vout|) S=O(lg|Vout|)*)
         | SOME a => Set.toSeq a 
  (* Task 2.4 *)

    
  fun ngf (F,G) = 
     let
         (*return a table the key is u's outneighbors and the value is
         * singleton(u)  --->seq table
         * 
         * *)
         fun addParent u = 
           let
             (*W=O(|Uouts|) S=log|Uouts|*)
             val  outs = Set.fromSeq(outNeighbors G u)
           in
             (*W=O(|Uouts|) S=O(1)*)
             Table.tabulate (fn _ => Seq.singleton(u)) outs
           end
         fun union(M,N)=
           (*W=O(mlog(1+n/m)) S=O(log(n+m))*)
            Table.merge (fn(a,b) => Seq.append(a,b)) (M,N)
         (*
         * added is a table , key is every element is set F 
         * and the value is a table
         * *)
         (*W=||F|| S=O(logn)*)
         val  added = Table.tabulate addParent F
     in
         (*
         * return a unioned table
         * union(table,table) -->the value of f(f belonging F)
         * union aims to merge two table even they may have the same
         * key
         * *)
         (*W=O(logn*||F||) S=O((logn)^2)*)
         Table.reduce union (Table.empty()) added
     end
  fun makeASP (G : graph) (v : vertex) : asp =     
      case Table.find G v
        of NONE => (v,Table.empty())
         | SOME _ =>
             let
               (*W=O(|E|log|V|) S=O(D(log|V|)^2)*)
               fun BFSReach (X,F) =
                 if Table.size F=0 then X
                 else 
                    let 
                      (*W=O(|F|logn) S=O(logn)*)
                      val X'= Table.merge (fn(a,b) => Seq.append(a,b)) (X,F)
                      (*W=O(||F||logn) S=O((logn)^2)*)
                      val NGF = ngf(Table.domain(F),G)
                      (*W=O(||F||logn) S=O(logn)*)
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
            (*W=O(lg|V| S=O(lg|V|))*)
            val SOME uSeq = Table.find sTable u 
            (*递归|L|<=log|V|层*)
            val pre = Seq.map toRoot uSeq
            (*W=O(l*p) S=O(lg(l))*)
            val pre' = Seq.flatten pre
          in
            (*W=O(l*p) S=O(1)*)
            Seq.map (fn (a) => Seq.append(a,Seq.singleton(u))) pre'
          end
    in
      case Table.find sTable v 
        of NONE => Seq.empty()
         | SOME vSeq =>
             toRoot(v)
    end 
end
