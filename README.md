# Snake-Game-in-Assembly-Language

The Snake game in assembly language for the x8086 microprocessor involves controlling a "snake" that moves around the screen, consuming food items to grow in length while avoiding collisions with itself or the screen borders. The game typically uses keyboard inputs to control the direction of the snake and updates the screen using direct memory access to the video buffer.

Key components of the game include:

Snake Representation: The snake is represented as a series of characters or pixels stored in memory.
Movement Logic: Registers are used to handle the snake's position and movement across the screen in response to arrow key inputs.
Food Placement: Random generation of food items on the screen, using simple randomization techniques.
Collision Detection: Check for collisions with the borders or the snake's body by comparing coordinates.
Screen Drawing: The game's graphics are drawn using video interrupts (e.g., INT 10h) or directly accessing the video memory.
This basic game in x8086 assembly focuses on logic control, efficient use of registers, and memory management for smooth gameplay.
