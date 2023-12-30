#load "Utilities.fsx"
#r "nuget: Akka.FSharp, 1.4.27"
open System
open Akka.FSharp

// Gossip Algorithm
let gossip maxRumorCount (topologyMap: Map<int, List<int>>) nodeID counterRef (mailbox: Actor<_>) =
    // Define a recursive function handleMessage to process messages.
    let rec handleMessage rumorCount = actor {
        // Receive a message from the mailbox.
        let! message = mailbox.Receive()

        // Match the received message against possible cases.
        match message with
        | "receivedMessage" ->
            // If this is the first message received, schedule the first message after a delay.
            if rumorCount = 0 then
                mailbox.Context.System.Scheduler.ScheduleTellOnce(
                    TimeSpan.FromMilliseconds(20.0),
                    mailbox.Self, "sendMessage")
                // Notify the counter actor about gossip convergence.
                counterRef <! Utilities.GossipNode
            // Continue handling received messages.
            return! handleMessage (rumorCount + 1)

        | "sendMessage" ->
            // If the maximum rumor count is reached, stop sending messages.
            if rumorCount >= maxRumorCount then
                return! handleMessage rumorCount
            else
                // Send message to a random neighbor.
                let neighborID = Utilities.getRandomNeighborID topologyMap nodeID
                mailbox.Context.ActorSelection(@"akka://comms-system/user/worker" + string neighborID)
                <! "receivedMessage"
                // Schedule the next message after a delay.
                mailbox.Context.System.Scheduler.ScheduleTellOnce(
                    TimeSpan.FromMilliseconds(20.0),
                    mailbox.Self, "sendMessage")
                // Continue handling sent messages.
                return! handleMessage rumorCount

        | _ ->
            // Handle unhandled messages.
            printfn "Node %d got an invalid message" nodeID
            // Continue handling other messages.
            return! handleMessage rumorCount
    }

    // Start the initial loop with rumor count initialized to 0.
    handleMessage 0