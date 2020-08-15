# Assignment 1 Support Code

This is the support code for COMP3702 2020 Assignment 1.

The following files are provided:

**laser_tank.py**

This file contains a class representing Laser Tank game map. This class contains a number of functions which will be useful in developing your solver.

The class method
~~~~~
LaserTankMap.process_input_file(filename)
~~~~~
can be used to parse input files (testcases) and produce a LaserTankMap instance based on the input file.

The method
~~~~~
apply_move(self, move)
~~~~~
applies an action to the Laser Tank game map, changing it's state. Note that this method will mutate the internal variables of the class. Don't forget to deepcopy.

This method returns LaserTankMap.SUCCESS, LaserTankMap.COLLISION or LaserTankMap.GAME_OVER depending on the result of the applied move.

The method
~~~~~
is_finished(self)
~~~~~
tells you whether the state of this game map matches the goal state (i.e. the player's tank is on the flag tile).

You can run this file directly to launch an interactive game of Laser Tank. e.g:
~~~~~
$ python laser_tank.py testcases/<testcase_name>.txt
~~~~~
Press W to move forward, D and A to turn clockwise and counter-clockwise respectively, and spacebar to shoot the laser. Try this to get a feel for the rules and mechanics of the game.

**tester.py**

Use this script to test whether your output files are valid solutions. e.g:
~~~~~
$ python tester.py testcases/<testcase_name>.txt my_solver_output.txt
~~~~~
This will indicate whether your solution file passes all the requirements. Our autograder will make use of a copy of this tester script. To make sure your code is graded correctly, make sure your output files pass this tester.

**path_visualiser.py**

An animated version of tester which shows each step your agent takes. Use the same way as tester.

**solver.py**

A template for you to write your solution. All of the code you write should go inside this file. To make sure your code is graded correctly, do not rename this file.
