open System
open Akka.FSharp
open Utilities
open Gossip
open Pushsum

type Result = { NodesConverged: int; ConvergenceTime: int64; }

// Define a counter actor that tracks the convergence status of nodes and records convergence information.
// The actor takes an initial count, the total number of nodes, a stopwatch, and a mailbox for receiving messages.
let counter initialCount totalNodes (stopwatch: Diagnostics.Stopwatch) (mailbox: Actor<CounterMessage>) =
    // Define a recursive function 'handleMessage' to handle messages asynchronously.
    let rec handleMessage convergenceCount (convergenceRecords: Result list) =
        actor {
            // Receive a message from the mailbox.
            let! message = mailbox.Receive()

            // Match the received message against possible cases.
            match message with
            // Handle the case where a node signals convergence.
            | GossipNode ->
                let newRecord = { NodesConverged = convergenceCount + 1; ConvergenceTime = stopwatch.ElapsedMilliseconds }
                
                // Check if all nodes have converged.
                if (convergenceCount + 1 = totalNodes) then
                    stopwatch.Stop()
                    printfn "Total nodes converged: %d" (convergenceCount + 1)
                    printfn "Convergence achieved in %d ms" stopwatch.ElapsedMilliseconds
                    mailbox.Context.System.Terminate() |> ignore
                
                // Continue the loop with updated count and convergenceRecords.
                return! handleMessage (convergenceCount + 1) (List.append convergenceRecords [newRecord])

            // Handle the case where a node signals convergence with Push-Sum.
            | PushSumNode (nodeID, average) ->
                let newRecord = { NodesConverged = convergenceCount + 1; ConvergenceTime = stopwatch.ElapsedMilliseconds }
                
                // Check if all nodes have converged.
                if (convergenceCount + 1 = totalNodes) then
                    stopwatch.Stop()
                    printfn "Total nodes converged: %d" (convergenceCount + 1)
                    printfn "Convergence achieved in %d ms" stopwatch.ElapsedMilliseconds
                    mailbox.Context.System.Terminate() |> ignore
                
                // Continue the loop with updated count and convergenceRecords.
                return! handleMessage (convergenceCount + 1) (List.append convergenceRecords [newRecord])
        }
    // Start the initial loop with the provided initial count and an empty convergenceRecords.
    handleMessage initialCount []

[<EntryPoint>]
let main argv =
    // Create a system with a given name
    let system = System.create "comms-system" (Configuration.load())

    // Set the maximum number of times a node should hear the rumor before stopping
    let maxRumorCount = 10

    // Parse command line arguments
    let desiredNumNodes = int argv.[0]
    let topologyType = argv.[1]
    let algorithmType = argv.[2]

    // Calculate the actual number of nodes based on the desired number and topology
    let numNodes = roundToNearestSquare desiredNumNodes topologyType

    // Build the topology map based on the given number of nodes and topology type
    let topologyMap = createGrid numNodes topologyType

    // Initialize a stopwatch for measuring time
    let stopwatch = Diagnostics.Stopwatch()

    // Spawn the counter actor to track convergence
    let counterRef = spawn system "counter" (counter 0 numNodes stopwatch)

    // Run the specified algorithm based on user input
    match algorithmType with
    | "gossip" ->
        // Create workers, select one randomly to start the algorithm
        let workers =
            [ 1 .. numNodes ]
            |> List.map (fun nodeID ->
                let name = "worker" + string nodeID
                spawn system name (gossip maxRumorCount topologyMap nodeID counterRef))
        let startingWorker = randomElement workers
        // Start the timer and initiate the algorithm
        stopwatch.Start()
        startingWorker <! "receivedMessage"

    | "push-sum" ->
        // Create worker actors
        let workers =
            [ 1 .. numNodes ]
            |> List.map (fun nodeID ->
                let name = "worker" + string nodeID
                spawn system name (pushSum topologyMap nodeID counterRef))
        // Start the timer and initiate the algorithm
        stopwatch.Start()
        workers |> List.iter (fun worker -> worker <! Initialize)

    | _ -> failwithf "Unsupported algorithm: %s" algorithmType

    // Wait for all actors to terminate
    system.WhenTerminated.Wait()

    // Return an integer exit code
    0