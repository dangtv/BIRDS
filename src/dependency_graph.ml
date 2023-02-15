
open Utils


module type ElementType = sig
  type t

  val compare : t -> t -> int
end


module Make (Element : ElementType) = struct

  module ElementMap = Map.Make(Element)

  module GraphImpl = Graph.Persistent.Digraph.Abstract(Element)

  module ComponentImpl = Graph.Components.Make(GraphImpl)

  module TopologicalImpl = Graph.Topological.Make(GraphImpl)

  type element = Element.t

  module Vertex = GraphImpl.V

  type 'a t = {
    labels : ('a * Vertex.t) ElementMap.t;
    main   : GraphImpl.t;
  }


  let empty =
    { labels = ElementMap.empty; main = GraphImpl.empty; }


  let add_vertex (elem : element) (data : 'a) (graph : 'a t) : ('a t * Vertex.t, 'a * Vertex.t) result =
    let open ResultMonad in
    match graph.labels |> ElementMap.find_opt elem with
    | Some pair ->
        err pair

    | None ->
        let vertex = Vertex.create elem in
        let graph =
          {
            labels = graph.labels |> ElementMap.add elem (data, vertex);
            main   = GraphImpl.add_vertex graph.main vertex
          }
        in
        return (graph, vertex)


  let get_vertex (elem : element) (graph : 'a t) : Vertex.t option =
    graph.labels |> ElementMap.find_opt elem |> Option.map (fun (_data, vertex) -> vertex)


  let add_edge ~from:(vertex1 : Vertex.t) ~to_:(vertex2 : Vertex.t) (graph : 'a t) : 'a t =
    { graph with main = GraphImpl.add_edge graph.main vertex1 vertex2 }


  let extract_vertex_info (graph : 'a t) (vertex : Vertex.t) : element * 'a =
    let elem = GraphImpl.V.label vertex in
    match graph.labels |> ElementMap.find_opt elem with
    | None           -> assert false
    | Some (data, _) -> (elem, data)


  let find_loop (g : GraphImpl.t) =
    GraphImpl.fold_vertex (fun v acc ->
      match acc with
      | Some _ -> acc
      | None   -> if GraphImpl.mem_edge g v v then Some v else None
    ) g None


  let topological_sort (graph : 'a t) : ((element * 'a) list, (element * 'a) list) result =
    let open ResultMonad in
    match find_loop graph.main with
    | Some v ->
        err  [ extract_vertex_info graph v ]

    | None ->
        (* Checks that all strongly connected components are singleton: *)
        let sccs = ComponentImpl.scc_list graph.main in
        sccs |> foldM (fun () scc ->
          match scc with
          | []          -> assert false
          | [ _ ]       -> return ()
          | _ :: _ :: _ -> err (scc |> List.map (extract_vertex_info graph))
        ) () >>= fun () ->

        let acc =
          TopologicalImpl.fold (fun v acc ->
            let info = extract_vertex_info graph v in
            info :: acc
          ) graph.main []
        in
        return (List.rev acc)

end
