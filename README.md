# The Blind Hen submission

We developed a number of approaches to solving the problems, and applied a post-optimizer on the resulting traces. Our main success was a semi-guided random sub-division approach, followed by a number of post-processing optimizations.

This year our team consisted of three members.  We used F# and developed in Visual Studio Code. GitHub Copilot was a fourth team member that oscillated between being very insightful and frightfully dumb.


Main AIs:
- Quad-tree like sub-division where the cut line is determined by a simple heuristic for finding strong "contrasts".
- Randomly placed sub-divisions. It uses the recursively determined sub-division to optimize the order in which to color the areas.
- MCTS where each search state is only allowed a strict bound on the number of operations, to limit the bounding factor. We had speed issues with this approach, and the solutions it generated were mostly not competitive with a huge number of runs of the random strategy.
- For the problems with initial grid-configuration, we used an Integer Programming solver to determine the optimal swap operations to apply. This gave us a decent score on some of the probles, but it turned out that simply merging the entire grid together and running one of our other solvers was much more effective.

We had implemented Weiszfeld's algorithm for computing geometric median color, and used approximations based on average color and/or random sampling in tight inner-loops of our solvers.

Post-optimization:
- Reorder all color operations such that we color a block before we cut it up, in the color of the smallest resulting sub-blocks it includes.
- Reassign all color operations to the true geometric median of the sub-blocks it ends up coloring in the final image. This allowed being less stringent in the initial color assignment in the AIs.
- Try to re-apply the random solver on each resulting simple block. There were a few problems where our random solver had missed some opportunities for further sub-division.
- Try to randomly perturb each cut-point to improve the penalty.

## Things we missed or didn't have time for

- We realised only after the contest how cut + color + remerging was an effective strategy for coloring small areas with low cost. The best scores were completely out of reach without this insight.

- We thought the straightforward merging strategy for merging the initial grid-configurations was optimal. Reading on Discord after the contest, it was clear that applying a combination of merges and cuts would have been better.

- We realised after the contest that problems with initial grid-configurations could possibly be solved using a combination of row- and column swaps, as a puzzle. We think that Integer Programming could be used to solve this.

- We chose not to spend time on implementing support for the last extension in Problems 36-40. This meant our own score computation was slightly off, and due to the nature of many of the problems, our solvers could have done better by keeping the original background image rather than re-coloring everything.


### Building and running from the command line

To build and run in debug mode:
```
dotnet run --project TheBlindHen
```

To build and then run in release mode:
```
dotnet build -c Release
dotnet exec TheBlindHen/bin/Release/net6.0/TheBlindHen.dll
```
