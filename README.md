# hammer

Tools for microstructure analysis in polycrystalline materials.

## What is this?

A Haskell library providing the core data structures and algorithms for analyzing the internal structure of polycrystalline materials (metals, ceramics, etc.) at the grain level. When you look at a metal under a microscope, you see a mosaic of differently oriented crystal grains. `hammer` provides the computational tools to identify those grains, represent their topology, and export results for visualization.

## What does it provide?

### 3D voxel grid system

A complete spatial data structure for representing crystalline microstructures as 3D voxel grids. Each voxel holds a value (e.g., a measured crystal orientation from an EBSD scan). The grid supports coordinate transformations between voxel indices, grid positions, and world-space coordinates, with operations for splitting, merging, and querying sub-regions.

### Grain finding (connected-component labeling)

Given a 3D voxel grid and a user-defined equivalence relation (e.g., "these two voxels have similar crystal orientations"), the grain finder identifies all connected regions -- the grains.

The algorithm uses a parallel divide-and-conquer approach: recursively split the grid in half along its longest axis, label grains in each half independently, then merge by scanning the shared wall and unifying grains that connect across it. For grids with 500+ voxels, the two recursive calls run in parallel using GHC's parallel evaluation strategies.

### Microstructure graph

Once grains are identified, `hammer` builds a topological graph encoding the relationships between microstructural features at four hierarchical levels:

- **Grains** -- connected voxel regions with unique identities
- **Faces** -- grain boundaries where two grains meet
- **Edges** -- triple lines where three grain boundaries intersect
- **Vertices** -- quadruple points where four grains meet

This hierarchical graph is the standard representation for microstructure topology in materials science.

### Sparse 3D matrices

A custom compressed sparse matrix optimized for the grain-finding algorithm's wall-scanning phase. Supports O(log n) lookup by (x, y, z) coordinates via binary search, and efficient merge operations along any axis.

### Numerical optimization (BFGS)

An implementation of the BFGS quasi-Newton algorithm for unconstrained function minimization, with an inexact line search using strong Wolfe conditions. This is a standard workhorse algorithm for smooth optimization: it approximates the inverse Hessian using rank-2 updates from gradient information, converging superlinearly without requiring second derivatives.

### Segment sorting

An algorithm for sorting connected line segments into open chains or closed loops -- useful for tracing grain boundaries and triple lines.

### VTK visualization export

Functions to export voxel grids, grain structures, and microstructure graphs to VTK format for visualization in [ParaView](https://www.paraview.org/). Builds on the `vtk` library with specialized renderers for voxel faces, edges, vertices, and complete microstructure graphs.

## Example

```haskell
import Hammer.VoxBox
import Hammer.VoxConn
import Hammer.MicroGraph

-- Find grains: connected voxels with equal values
case grainFinder (==) voxelBox of
  Just (labeledBox, grainMap) -> do
    -- Build the full microstructure graph (grains, faces, edges, vertices)
    let (micro, _) = getMicroVoxel (resetGrainIDs (labeledBox, grainMap))
    print $ "Grains: " ++ show (HM.size $ microGrains micro)
    print $ "Faces:  " ++ show (HM.size $ microFaces micro)
  Nothing -> putStrLn "No grains found"
```

## Where is it used?

hammer is a core dependency used by several packages in this ecosystem:

- **sledge** -- uses the BFGS optimizer for Bingham distribution fitting and VoxBox for EBSD data representation
- **SubZero** -- uses VTK rendering and segment sorting
- **VirMat** -- uses the microstructure graph, grain topology, and VTK export in the virtual microstructure generation pipeline

## How to build

```bash
# With Nix (recommended)
nix develop
cabal build --allow-newer

# With Cabal
cabal build

# Run tests
cabal test

# Benchmarks and profiling (behind test flag)
cabal build -ftest hammer-benchmark
cabal build -ftest hammer-profile
```

## License

MIT -- see [LICENSE](./LICENSE).
