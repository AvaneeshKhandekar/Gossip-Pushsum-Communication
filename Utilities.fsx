module Utilities
open System

// Select random element from a list
let randomElement (list: List<_>) = list |> List.item (Random().Next(list.Length))

// Retrieve a random neighbor ID from the neighbor list of the specified node.
let getRandomNeighborID (topologyMap: Map<_, _>) nodeID =
    // Retrieve the neighbor list for the given node ID from the topology map.
    let (neighborList: List<_>) = (topologyMap.TryFind nodeID).Value

    // Create a random number generator.
    let random = Random()

    // Select and return a random neighbor ID from the neighbor list.
    neighborList.[random.Next(neighborList.Length)]

// Calculate neighbors in a 2D grid for a given node.
let getNeighbors2D nodeID numNodes =
    // Determine the length of each side in the 2D grid.
    let lenSide = int (sqrt (float numNodes))

    // Check if a node is within bounds.
    let isWithinBounds n = n > 0 && n <= numNodes

    // Calculate neighbors based on the position of the node in the 2D grid.
    let validNeighbors =
        match nodeID % lenSide with
        | 0 -> [nodeID - 1; nodeID - lenSide; nodeID + lenSide]
        | 1 -> [nodeID + 1; nodeID - lenSide; nodeID + lenSide]
        | _ -> [nodeID - 1; nodeID + 1; nodeID - lenSide; nodeID + lenSide]

    // Filter out neighbors that are out of bounds and return the result.
    validNeighbors |> List.filter isWithinBounds

// Build a 2D grid network topology where each node is connected to its neighbors in a grid.
let get2DGrid numNodes =
    let allNodes = [1 .. numNodes]

    let neighborsForNode nodeID =
        // Get neighbors in a 2D grid for the given node.
        getNeighbors2D nodeID numNodes

    // Use List.fold to build the topology by adding neighbors for each node to a Map.
    allNodes
    |> List.fold (fun acc nodeID ->
        // Add the nodeID and its neighbors to the Map.
        Map.add nodeID (neighborsForNode nodeID) acc) Map.empty

// Build a linear network topology where each node is connected to its immediate neighbors.
let getLineGrid numNodes =
    let allNodes = [1 .. numNodes]

    let neighborsForNode nodeID =
        // Filter nodes to include only immediate neighbors.
        List.filter (fun y -> y = nodeID + 1 || y = nodeID - 1) allNodes

    // Use List.fold to build the topology by adding neighbors for each node to a Map.
    allNodes
    |> List.fold (fun acc nodeID ->
        // Add the nodeID and its neighbors to the Map.
        Map.add nodeID (neighborsForNode nodeID) acc) Map.empty

// Calculate neighbors in a 3D grid for a given node.
let getNeighbors3D nodeID numNodes =
    // Determine the length of each side in the 3D grid.
    let lenSide = int (Math.Pow(float numNodes, 1.0 / 3.0))

    // Check if a node is within bounds.
    let isWithinBounds n = n > 0 && n <= numNodes

    // Check nodeID against its position in the 3D grid and return valid neighbors within bounds.
    let validNeighbors =
        match (nodeID % lenSide, nodeID % (int (Math.Pow(float lenSide, 2.0)))) with
        | 0, _ -> [nodeID - 1; nodeID - lenSide; nodeID + lenSide; nodeID - int (Math.Pow(float lenSide, 2.0)); nodeID + int (Math.Pow(float lenSide, 2.0))]
        | 1, 1 -> [nodeID + 1; nodeID + lenSide; nodeID - int (Math.Pow(float lenSide, 2.0)); nodeID + int (Math.Pow(float lenSide, 2.0))]
        | 1, lastRowFirstCol -> [nodeID + 1; nodeID - lenSide; nodeID - int (Math.Pow(float lenSide, 2.0)); nodeID + int (Math.Pow(float lenSide, 2.0))]
        | _, _ -> [nodeID - 1; nodeID + 1; nodeID - lenSide; nodeID + lenSide; nodeID - int (Math.Pow(float lenSide, 2.0)); nodeID + int (Math.Pow(float lenSide, 2.0))]

    // Filter out neighbors that are out of bounds and return the result.
    validNeighbors |> List.filter isWithinBounds

// Build an imperfect 3d network topology where nodes are arranged in 3d space and each node is connected to a random set of neighbors.
let getImperfect3DGrid numNodes =
    // Add a random neighbor that is not the current node or already in the list of neighbors
    let addRandomNeighbor nodeID neighbors =    
        let randomNeighbor =
            [1 .. numNodes]
            |> List.filter (fun m -> m <> nodeID && not (List.contains m neighbors))
            |> randomElement
        randomNeighbor :: neighbors
    // Build the imperfect 3D topology using functional approach
    [1 .. numNodes]
    |> List.fold (fun acc nodeID ->
        // Get the neighbors of the current node in a 3D grid.
        let neighbors = getNeighbors3D nodeID numNodes
        // Add the nodeID and its randomly selected neighbors to the Map.
        Map.add nodeID (addRandomNeighbor nodeID neighbors) acc) Map.empty

// Build a full network topology where each node is connected to all other nodes excluding itself.
let getFullGrid numNodes =
    // Create a list containing all node IDs from 1 to numNodes.
    let allNodes = [1 .. numNodes]

    // Define a function to get neighbors for a given node excluding itself.
    let neighborsWithoutSelf nodeID =
        List.filter (fun y -> nodeID <> y) allNodes

    // Use List.fold to build the topology by adding neighbors for each node to a Map.
    allNodes
    |> List.fold (fun acc nodeID ->
        Map.add nodeID (neighborsWithoutSelf nodeID) acc) Map.empty

// Build a network topology based on the specified topology type.
let createGrid numNodes topology =
    match topology with
    | "line" -> // Build a linear network topology.
                 getLineGrid numNodes
    | "2D" ->   // Build a 2D grid network topology.
                 get2DGrid numNodes
    | "imp3D" -> // Build an imperfect 3D network topology with random connections.
                 getImperfect3DGrid numNodes
    | "full" -> // Build a full network topology where each node is connected to all other nodes.
                 getFullGrid numNodes
    | _ ->      // Throw an exception for unsupported topology types.
                 failwithf "Unsupported topology: %s" topology


// Discriminated union type CounterMessage representing messages sent to the counter actor.
type CounterMessage =
    // Message indicating a node's convergence in the Gossip algorithm.
    | GossipNode
    // Message indicating a node's convergence in the Push-Sum algorithm with node ID and average value.
    | PushSumNode of int * float

// Round number of nodes to get perfect square in case of 2D and imperfect 3D grid
let roundToNearestSquare numNodes topology =
    match topology with
    | "2D" -> Math.Pow (Math.Round (sqrt (float numNodes)), 2.0) |> int
    | "imp3D" -> Math.Pow (Math.Round ((float numNodes) ** (1.0 / 3.0)), 3.0)  |> int
    | _ -> numNodes