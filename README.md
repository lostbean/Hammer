# Hammer

**Basic tools for microstructure analysis in polycrystalline materials**

`hammer` is a Haskell library providing data structures and algorithms for voxel-based 3D microstructure analysis. It is designed for metallurgical research, offering high-performance connected-component labeling (grain finding), sparse 3D matrices, a microstructure graph representation, numerical optimization routines, and VTK visualization export.

## Modules

### `Hammer.VoxBox` (exposed)

Defines the 3D voxel grid system and all geometric operations on it.

**Key data types:**

- `VoxelPos` -- A 3D integer coordinate `(x, y, z)` representing a voxel position. Strict, unboxed fields. Instances for `Hashable`, `Storable`, `NFData`, and `Unbox`.
- `VoxBox a` -- The primary container: a 3D voxel grid holding a `VoxBoxRange` (origin + dimensions), a `VoxBoxOrigin` (world-space origin), a `VoxelDim` (per-voxel size), and a flat `U.Vector a` of per-voxel values.
- `VoxBoxRange` -- An origin `VoxelPos` plus a `VoxBoxDim` defining a sub-region of the grid.
- `EdgeVoxelPos` -- An edge in X, Y, or Z direction attached to a `VoxelPos`.
- `FaceVoxelPos` -- A face in X, Y, or Z direction attached to a `VoxelPos`.
- `CartesianDir` -- `XDir | YDir | ZDir`.

**Key operators and functions:**

- `(%@)` / `(%@?)` -- Convert `VoxelPos` to linear index (unsafe / safe).
- `(%#)` / `(%#?)` -- Convert linear index to `VoxelPos` (unsafe / safe).
- `(#!)` / `(#!?)` -- Look up a voxel value from a `VoxBox` by position (unsafe / safe).
- `(#+#)`, `(#-#)`, `(#*#)`, `(#/#)` -- Voxel position arithmetic.
- `splitInTwoBox` -- Splits a `VoxBoxRange` in half along its longest axis.
- `mergeVoxBoxRange` -- Merges two adjacent `VoxBoxRange` values, returning the merged range, adjacency axis, and wall range.
- `evalVoxelPos` / `evalCentralVoxelPos` -- Convert a `VoxelPos` to world-space `Vec3D`.
- `evalFacePos` / `evalFaceCorners` -- Evaluate a face plane or four corner points of a face.
- `findIntersection` -- Ray-plane intersection.

### `Hammer.VoxConn` (exposed)

Implements the **grain-finding algorithm**: a parallel divide-and-conquer connected-component labeling system for 3D voxel data.

**Key data types:**

- `VoxConn cont grid` -- The intermediate D&C data structure containing a `VoxBoxRange`, container mapping positions to grain IDs, and a `HashMap` mapping grain IDs to constituent positions.
- `GridConn g` -- Type class for grid elements that can be connected. Instances for `VoxelPos`, `FacePos`, and `EdgePos`.
- `Cont c a` -- Type class abstracting over data containers (unboxed vectors, boxed vectors, `HashMap`, `IntMap`, `Sparse3`).

**Key functions:**

- `grainFinder :: (U.Unbox a) => (a -> a -> Bool) -> VoxBox a -> Maybe (VoxBox GrainID, HashMap Int (Vector VoxelPos))` -- The primary entry point. Given an equality predicate and a `VoxBox`, identifies all connected components (grains).
- `resetGrainIDs` -- Renumbers and shuffles grain IDs to produce compact sequential labeling.

**Algorithm:** Recursively splits the box in two along its longest axis. Base case: a single voxel becomes its own grain. On merge, the wall between two half-boxes is scanned and connected pairs trigger a union operation. For boxes >= 500 voxels, the two recursive calls are evaluated in parallel using `rpar`.

### `Hammer.Math.SparseMatrix` (exposed)

A custom sparse 3D matrix implementation optimized for the grain-finding algorithm.

**Key data types:**

- `Sparse3 a` -- A compressed sparse 3D matrix stored in a column-major-like scheme using value vectors, XY-plane index markers (CSR-style), and Z-coordinate markers.

**Key functions:**

- `mkSparse3` -- Construct from a vector of `((x, y, z), value)` pairs.
- `lookup` -- O(log n) lookup by `(x, y, z)` using binary search.
- `adjust` -- Batch-update values at given positions.
- `mergeSparse3` -- Auto-detects adjacency axis and merges two sparse matrices.

### `Hammer.MicroGraph` (exposed)

A graph-based representation of polycrystalline microstructure topology.

**Key data types:**

- `GrainID` -- Newtype over `Int` identifying a grain.
- `FaceID` -- Identifies a grain boundary face by the pair of `GrainID` values it separates.
- `EdgeID` -- Identifies a triple line (intersection of three grain boundaries).
- `VertexID` -- Identifies a quadruple point (intersection of four grains).
- `MicroGraph g f e v` -- The complete microstructure graph: `HashMap`s mapping `GrainID`, `FaceID`, `EdgeID`, and `VertexID` to their respective property types.
- `MicroVoxel` -- Type alias for `MicroGraph` specialized with voxel-based geometry.

**Key functions:**

- `getMicroVoxel :: (VoxBox GrainID, HashMap Int (Vector VoxelPos)) -> (MicroVoxel, VoxBox GrainID)` -- Scans the voxel grid to identify faces, edges, and vertices and assembles the full microstructure graph.
- `initMicroGraph`, `insertNewGrain`, `insertNewFace`, `insertNewEdge`, `insertNewVertex` -- Graph construction.
- `getGrainProp`, `getFaceProp`, `getEdgeProp`, `getVertexProp` -- Property retrieval.

### `Hammer.Math.Array` (exposed)

Multidimensional array and matrix utilities.

- `Array i e` -- An N-dimensional array backed by an unboxed vector.
- `Matrix2D` -- Type alias for `Array (Int, Int) Double`.
- Full matrix algebra instances: `AbelianGroup`, `MultSemiGroup`, `Ring`, `LeftModule`, `RightModule`, `Diagonal`, `Transpose`, `LinearMap`, `DotProd`, `Pointwise`.
- `binarySearch` -- O(log n) binary search on a sorted vector.

### `Hammer.Math.Optimum` (exposed)

Numerical optimization algorithms.

- `lineSearch` -- Inexact line search using strong Wolfe conditions.
- `bfgs` -- Quasi-Newton BFGS algorithm for unconstrained function minimization.
- `BFGScfg` -- Configuration: `epsi` (gradient norm tolerance), `tol` (step tolerance), `niter` (max iterations).
- `defaultBFGS` -- Default configuration: `epsi = 1e-9`, `tol = 1e-7`, `niter = 100`.

### `Hammer.Math.SortSeq` (exposed)

Algorithm for sorting and classifying sequences of connected segments.

- `SeqSeg a` -- Type class for segment-like types with head/tail access.
- `Seq a` -- Result type: `OpenSeq (Vector a)` or `LoopSeq (Vector a)`.
- `sortSegs` -- Classifies a vector of segments into open sequences and closed loops.

### `Hammer.Graph` (exposed)

Re-exports `Data.Graph.Markov` from the `mcl` package, providing Markov Clustering (MCL) graph analysis.

### `Hammer.VTK` (exposed)

Export voxel and microstructure data to VTK format for visualization in [ParaView](https://www.paraview.org/).

- `renderVoxBoxVTK` -- Render a `VoxBox` as a VTK `RectilinearGrid`.
- `renderVoxElemVTK` / `renderVoxElemListVTK` -- Render collections of voxel elements as VTK `UnstructuredGrid`.
- `renderMicroGrainsVTK`, `renderMicroFacesVTK`, `renderMicroEdgesVTK`, `renderMicroVertexVTK` -- Render components of a `MicroVoxel` graph.

## Usage Examples

### Grain Finding

```haskell
import qualified Data.Vector.Unboxed as U
import Hammer.VoxBox
import Hammer.VoxConn
import Hammer.MicroGraph

-- Find connected components (grains) where voxels with equal values are connected
case grainFinder (==) vbox of
  Just (labeledVbox, grainMap) -> do
    print $ "Found " ++ show (HM.size grainMap) ++ " grains"
  Nothing -> putStrLn "No grains found"
```

### Building a Microstructure Graph

```haskell
import Hammer.MicroGraph

case grainFinder (==) vbox of
  Just result -> do
    let (micro, vboxGID) = getMicroVoxel (resetGrainIDs result)
    print $ "Faces: " ++ show (HM.size $ microFaces micro)
    print $ "Edges: " ++ show (HM.size $ microEdges micro)
  Nothing -> return ()
```

### VTK Export

```haskell
import Hammer.VTK

let vec  = U.map unGrainID (grainID labeledVbox)
    vtk  = renderVoxBoxVTK labeledVbox [mkPointAttr "GrainID" (vec U.!)]
writeUniVTKfile "output.vtr" True vtk
```

### BFGS Optimization

```haskell
import Hammer.Math.Optimum
import Linear.Vect

-- Minimize f(x,y) = 2x^2 + 2xy + 2y^2 - 6x
let func (Vec2 x y) = ( 2*x*x + 2*x*y + 2*y*y - 6*x
                       , Vec2 (4*x + 2*y - 6) (2*x + 4*y) )
    result = bfgs defaultBFGS func (Vec2 100 431)
```

### Segment Sorting

```haskell
import qualified Data.Vector as V
import Hammer.Math.SortSeq

let segs = V.fromList [(1::Int, 2::Int), (10, 1), (2, 5), (10, 5)]
    result = sortSegs segs
-- result = [LoopSeq fromList [(1,2),(2,5),(5,10),(10,1)]]
```

## Dependencies

| Package | Purpose |
|---|---|
| `base` (>= 4, < 5) | Standard library |
| `linear-vect` | Linear algebra types |
| `mcl` | Markov Clustering algorithm |
| `vtk` | VTK file format I/O |
| `vector` | Boxed and unboxed vectors |
| `vector-algorithms` | Sorting algorithms |
| `vector-th-unbox` | Template Haskell for Unbox instances |
| `containers` | IntMap, Set, IntSet |
| `unordered-containers` | HashMap, HashSet |
| `hashable` | Hashable instances |
| `deepseq` | NFData for parallel evaluation |
| `parallel` | Parallel evaluation strategies |
| `primitive` | Mutable primitives |
| `random` | Random number generation |
| `binary` | Binary serialization |
| `bytestring` | Byte string support |
| `base64-bytestring` | Base64 encoding |
| `blaze-builder` | Efficient bytestring building |
| `text` | Text type |
| `xmlgen` | XML generation (for VTK) |

## Building

### With Nix (recommended)

```bash
nix develop
cabal build --allow-newer
```

### With Cabal

```bash
cabal build --project-file=cabal.project
```

### Benchmarks and profiling (behind `test` flag)

```bash
cabal build -ftest hammer-benchmark
cabal build -ftest hammer-profile

# Run profile tests
cabal run -ftest hammer-profile -- --test-suit
cabal run -ftest hammer-profile -- --grainfinder-profile output/
```

## Architecture

The library follows a layered architecture:

1. **VoxBox layer** -- 3D grid coordinate system, voxel positions, and geometric operations.
2. **VoxConn layer** -- Parallel connected-component labeling, parametric over storage backends.
3. **MicroGraph layer** -- Topological graph (grains, faces, edges, vertices) from grain-finder output.
4. **Math layer** -- Sparse matrices, N-dimensional arrays, optimization, segment sorting.
5. **VTK layer** -- VTK format export for 3D visualization.

## Author

Edgar Gomes de Araujo (<talktoedgar@gmail.com>)

## License

MIT -- see [LICENSE](./LICENSE).
