# Gossip-Pushsum-Communication
#### Academic Project
This project simulates the gossip and push-sum distributed communication algorithms using F# and the Akka framework. The gossip
algorithm and push-sum algorithm are run on various network topologies. The convergence time for
both methods is calculated for four different network topologies (Line, 2D Grid, Imperfect 3D
grid, Full).

## How to Build & Run:
```sh
dotnet build
dotnet run numberOfNodes TopologyName AlgorithmName
```

## Supported Algorithms
o gossip
o push-sum

## Supported Network Topologies
o 2D
o line
o full
o imp3D

## Convergence Time
### Gossip Algorithm
![Gossip](Gossip.png)

### Push-sum Algorithm
![Push](Push-sum.png)
