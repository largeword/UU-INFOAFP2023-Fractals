# UU-INFOAFP2023-Fraskell: Exploring Fractals in Haskell
This is the group project (rendering fractals) repository of [Advanced Functional Programming 2022/2023](https://www.cs.uu.nl/docs/vakken/afp/project.html) in Utrecht University.

The Accelerate folder contains source code to build and run the accelerated project in Cabal.
Due to Accelerate's constraints, it will work only on systems with CUDA-enabled GPUs and unix operating systems
The Universal folder contains source code to build and run a version of the project that may run on any Haskell-enabled system

Additionally, please find pre-built excutable files at the release section of the github project
Features described in this document are universal between the two versions: The only difference is acceleration


**Controlling keys:** \
                 &emsp; w -> Move Up\
                 &emsp; a -> Move Left\
                 &emsp; s -> Move Down\
                 &emsp; d -> Move Right\
                 &emsp; KeyUp -> Move Up\
                 &emsp; KeyLeft -> Move Left\
                 &emsp; KeyDown -> Move Down\
                 &emsp; KeyRight -> Move Right\
                 &emsp; q -> Zoom Out\
                 &emsp; e -> Zoom In\
                 &emsp; left mouseclick -> set offset to mouse coordinates\
                 &emsp; r -> Reset translation and zoom scale\
                 &emsp; g -> Re-generate fractal generation data

Please note that the application takes some time to generate the fractal after a key-press.  In particularly the mouse click may take longer to register and generate.

Project documentation is included in this hand-in, in the _Docs folder
Please enjoy the Fraskell tool
