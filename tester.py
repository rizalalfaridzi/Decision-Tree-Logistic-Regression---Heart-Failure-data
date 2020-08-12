#!/usr/bin/python

import sys
from laser_tank import LaserTankMap

"""
Tester script.

Use this script to test whether your output files are valid solutions. You
should avoid modifying this file directly.

COMP3702 2020 Assignment 1 Support Code

Last updated by njc 02/08/20
"""


def main(arglist):
    """
    Test whether the given output file is a valid solution to the given map file.
    :param arglist: map file name, output file name
    """
    if len(arglist) != 2:
        print("Running this file tests whether the given output file is a valid solution to the given map file.")
        print("Note that this does not indicate whether the output file is optimal in the number of steps taken.")
        print("Usage: tester.py [map_file_name] [output_file_name]")
        return

    map_file = arglist[0]
    soln_file = arglist[1]

    game_map = LaserTankMap.process_input_file(map_file)

    f = open(soln_file, 'r')
    moves = f.readline().strip().split(',')

    # apply each move in sequence
    error_occurred = False
    for i in range(len(moves)):
        move = moves[i]
        ret = game_map.apply_move(move)
        if ret == LaserTankMap.COLLISION:
            print("ERROR: Move resulting in Collision performed at step " + str(i))
            error_occurred = True
        elif ret == LaserTankMap.GAME_OVER:
            print("ERROR: Move resulting in Game Over performed at step " + str(i))
            error_occurred = True

    if error_occurred:
        return -1

    if game_map.is_finished():
        print("Puzzle solved in " + str(len(moves)) + " steps!")
        return len(moves)
    else:
        print("ERROR: Goal not reached after all actions performed.")
        return -1


if __name__ == '__main__':
    main(sys.argv[1:])

