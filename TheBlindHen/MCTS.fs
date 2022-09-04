module MCTS

type Node<'state, 'action> = {
    state: 'state
    mutable children: ('action * Node<'state, 'action>) array option
    mutable visits: int
    mutable totalPenalty: int64 // higher is worse
}

let mctsFindBestMove 
    (actions: 'state -> 'action array)
    (step: 'state -> 'action -> 'state)
    (simulate: 'state -> int)
    (exploreFactor: float)
    (iterations: int)
    (rootState: 'state) : ('action * 'state) option
    =
    let root = { state = rootState; children = None; visits = 0; totalPenalty = 0L }
    let expand node =
        let children = actions node.state |> Array.map (fun action -> action, { state = step node.state action; children = None; visits = 0; totalPenalty = 0L })
        node.children <- Some children
        children
    let select node =
        let rec select' node acc =
            match node.children with
            | None -> node, acc
            | Some [||] -> node, acc
            | Some children ->
                match Array.tryFind (fun (_, child) -> child.visits = 0) children with
                | Some (action, child) -> child, node :: acc
                | None ->
                    let rawPenalties = Array.map (fun (_, child) -> child, child.totalPenalty / int64(child.visits)) children
                    let minPenalty = Array.minBy snd rawPenalties |> snd
                    let maxPenalty = Array.maxBy snd rawPenalties |> snd
                    if minPenalty = maxPenalty then children[0] |> snd, node::acc else
                    let scaledScores = Array.map (fun (child, penalty) -> child, 1.0 - (float(penalty - minPenalty) / float(maxPenalty - minPenalty))) rawPenalties
                    let child, _ = Array.maxBy (fun (child, score) -> score + exploreFactor * sqrt (log (float node.visits)) / (float child.visits)) scaledScores
                    select' child (node :: acc)
        select' node []
    let rec backpropagate path penalty =
        match path with
        | [] -> ()
        | node :: rest ->
            node.visits <- node.visits + 1
            node.totalPenalty <- node.totalPenalty + penalty
            backpropagate rest penalty
    let rootChildren = expand root
    if Array.isEmpty rootChildren then None else
    for i in 1 .. iterations do
        let leaf, path = select root
        let child, path =
            if leaf.visits = 0 then
                leaf, path
            else
                match expand leaf with
                | [||] -> leaf, path
                | children -> children[0] |> snd, leaf :: path
        let penalty = simulate child.state
        backpropagate (child :: path) penalty
    let action, node = rootChildren |> Array.maxBy (fun (_, child) -> child.visits)
    Some (action, node.state)
