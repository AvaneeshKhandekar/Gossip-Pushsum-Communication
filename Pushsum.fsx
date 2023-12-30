#load "Utilities.fsx"
#r "nuget: Akka.FSharp, 1.4.27"
open System
open Akka.FSharp

// Discriminated union type named PushSumMessage
type PushSumMessage =
    | Initialize                 // Represents the initialization message
    | Message of float * float   // Represents a message with two float values sum and weight accumulator
    | Round                     // Represents a round message

// Push Sum Algorithm
let pushSum (topologyMap: Map<int, List<int>>) nodeID counterRef (mailbox: Actor<PushSumMessage>) =
    let rec loop ownSum ownWeight sumAccumulator weightAccumulator rounds isTransmitting = actor {
        // Check if the node is currently transmitting
        if isTransmitting then
            let! message = mailbox.Receive()
            match message with
            | Initialize ->
                // Initialize the Push-Sum algorithm
                mailbox.Self <! Message (float nodeID, 1.0)
                // Schedule repeated messages for each round
                mailbox.Context.System.Scheduler.ScheduleTellRepeatedly (
                    TimeSpan.FromMilliseconds(0.0),
                    TimeSpan.FromMilliseconds(20.0),
                    mailbox.Self,
                    Round
                )
                // Continue the loop with updated parameters
                return! loop (float nodeID) 1.0 0.0 0.0 0 isTransmitting

            | Message (receivedSum, receivedWeight) ->
                // Process received messages
                return! loop ownSum ownWeight (sumAccumulator + receivedSum) (weightAccumulator + receivedWeight) rounds isTransmitting

            | Round ->
                // Execute a round of the Push-Sum algorithm
                let neighborID = Utilities.getRandomNeighborID topologyMap nodeID
                let neighborPath = @"akka://comms-system/user/worker" + string neighborID
                let neighborRef = mailbox.Context.ActorSelection(neighborPath)
                
                // Send and receive messages with the neighbor
                mailbox.Self <! Message (sumAccumulator / 2.0, weightAccumulator / 2.0)
                neighborRef <! Message (sumAccumulator / 2.0, weightAccumulator / 2.0)

                // Check for convergence
                if abs ((sumAccumulator / weightAccumulator) - (ownSum / ownWeight)) < 1.0e-10 then
                    let newRounds = rounds + 1
                    if newRounds = 10 then
                        // Notify the counter actor about convergence
                        counterRef <! Utilities.PushSumNode (nodeID, sumAccumulator / weightAccumulator)
                        // Continue the loop with updated parameters
                        return! loop ownSum ownWeight 0.0 0.0 newRounds false
                    else
                        // Continue the loop with updated parameters
                        return! loop (sumAccumulator / 2.0) (weightAccumulator / 2.0) 0.0 0.0 newRounds isTransmitting 
                else
                    // Continue the loop with updated parameters
                    return! loop (sumAccumulator / 2.0) (weightAccumulator / 2.0) 0.0 0.0 0 isTransmitting
        }
    // Start the initial loop with the specified parameters
    loop (float nodeID) 1.0 0.0 0.0 0 true